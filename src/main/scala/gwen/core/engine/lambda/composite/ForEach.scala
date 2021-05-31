/*
 * Copyright 2021 Branko Juric, Brady Wood
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package gwen.core.engine.lambda.composite

import gwen.core._
import gwen.core.engine.EvalContext
import gwen.core.engine.EvalEngine
import gwen.core.engine.lambda.CompositeStep
import gwen.core.model._
import gwen.core.node.GwenNode
import gwen.core.node.gherkin.Scenario
import gwen.core.node.gherkin.Step
import gwen.core.node.gherkin.Tag

import scala.util.Try
import gwen.core.model.state.ScopedData

abstract class ForEach[T <: EvalContext](engine: EvalEngine[T]) extends CompositeStep[T] {

  /**
    * Repeats a step for each element in list of elements of type U.
    */
  def evaluateForEach[U](elements: ()=>Seq[U], name: String, parent: GwenNode, step: Step, doStep: String, ctx: T): Step = {
    val keyword = FeatureKeyword.nameOf(FeatureKeyword.Scenario)
    val items = elements()
    val foreachSteps = items.toList.zipWithIndex map { case (_, index) => 
      step.copy(
        withKeyword = if (index == 0) step.keyword else StepKeyword.nameOf(StepKeyword.And)
      )
    }
    val tags = List(Tag(ReservedTags.Synthetic), Tag(ReservedTags.ForEach), Tag(ReservedTags.StepDef))
    val preForeachStepDef = Scenario(None, tags, keyword, name, Nil, None, foreachSteps, Nil, Nil)
    engine.beforeStepDef(step, preForeachStepDef, ctx.scopes)
    val steps =
      items match {
        case Nil =>
          logger.info(s"For-each[$name]: none found")
          Nil
        case elems =>
          val noOfElements = elems.size
          logger.info(s"For-each[$name]: $noOfElements found")
          try {
            if(Try(ctx.getBoundReferenceValue(name)).isSuccess) {
              Errors.ambiguousCaseError(s"For-each element name '$name' already bound (use a free name instead)")
            }
            elems.zipWithIndex.foldLeft(List[Step]()) { case (acc, (currentElement, index)) =>
              val elemNo = index + 1
              ctx.topScope.set(s"$name index", index.toString)
              ctx.topScope.set(s"$name number", elemNo.toString)
              val params: List[(String, String)] = currentElement match {
                case data: ScopedData => 
                  ctx.topScope.pushObject(name, data)
                  data.findEntries { _ => true } toList
                case value: String => 
                  ctx.topScope.set(name, value)
                  if (ctx.options.dryRun) {
                    ctx.topScope.pushObject(name, currentElement)
                  }
                  List((name, value))
                case _ =>
                  ctx.topScope.pushObject(name, currentElement)
                  List((name, s"$name $elemNo"))
              }
              (try {
                EvalStatus(acc.map(_.evalStatus)) match {
                  case status @ Failed(_, error)  =>
                    val isAssertionError = status.isAssertionError
                    val isSoftAssert = ctx.evaluate(false) { isAssertionError && AssertionMode.isSoft }
                    val failfast = ctx.evaluate(false) { GwenSettings.`gwen.feature.failfast` }
                    if (failfast && !isSoftAssert) {
                      logger.info(s"Skipping [$name] $elemNo of $noOfElements")
                      engine.transitionStep(preForeachStepDef, foreachSteps(index).copy(withParams = params), Skipped, ctx.scopes)
                    } else {
                      logger.info(s"Processing [$name] $elemNo of $noOfElements")
                      engine.evaluateStep(preForeachStepDef, Step(step.sourceRef, if (index == 0) step.keyword else StepKeyword.nameOf(StepKeyword.And), doStep, Nil, None, Nil, None, Pending, params), ctx)
                    }
                  case _ =>
                    logger.info(s"Processing [$name] $elemNo of $noOfElements")
                    engine.evaluateStep(preForeachStepDef, Step(step.sourceRef, if (index == 0) step.keyword else StepKeyword.nameOf(StepKeyword.And), doStep, Nil, None, Nil, None, Pending, params), ctx)
                }
              } finally {
                ctx.topScope.popObject(name)
              }) :: acc
            } reverse
          } finally {
            ctx.topScope.set(name, null)
            ctx.topScope.set(s"$name index", null)
            ctx.topScope.set(s"$name number", null)
          }
      }
    val foreachStepDef = preForeachStepDef.copy(withSteps = steps)
    engine.afterStepDef(foreachStepDef, ctx.scopes)
    step.copy(withStepDef = Some(foreachStepDef))
  }

}
