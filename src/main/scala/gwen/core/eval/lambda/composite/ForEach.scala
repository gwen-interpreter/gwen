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

package gwen.core.eval.lambda.composite

import gwen.core._
import gwen.core.eval.EvalContext
import gwen.core.eval.EvalEngine
import gwen.core.eval.lambda.CompositeStep
import gwen.core.node.GwenNode
import gwen.core.node.gherkin._
import gwen.core.state.ScopedData
import gwen.core.status._

import scala.util.Try

abstract class ForEach[T <: EvalContext](engine: EvalEngine[T]) extends CompositeStep[T] {

  /**
    * Repeats a step for each item in list of items of type U.
    */
  def evaluateForEach[U](itemList: ()=>Seq[U], name: String, parent: GwenNode, step: Step, doStep: String, ctx: T): Step = {
    val keyword = FeatureKeyword.nameOf(FeatureKeyword.Scenario)
    val items = itemList()
    val foreachSteps = items.toList.zipWithIndex map { case (_, index) => 
      step.copy(
        withKeyword = if (index == 0) step.keyword else StepKeyword.nameOf(StepKeyword.And)
      )
    }
    val tags = List(Tag(ReservedTags.Synthetic), Tag(ReservedTags.ForEach), Tag(ReservedTags.StepDef))
    val preForeachStepDef = Scenario(None, tags, keyword, name, Nil, None, foreachSteps, Nil, Nil, step.cumulativeParams)
    engine.beforeStepDef(preForeachStepDef, ctx)
    val steps =
      items match {
        case Nil =>
          logger.info(s"For-each[$name]: none found")
          Nil
        case _ =>
          val noOfElements = items.size
          logger.info(s"For-each[$name]: $noOfElements found")
          try {
            if(Try(ctx.getBoundReferenceValue(name)).isSuccess) {
              Errors.ambiguousCaseError(s"For-each element name '$name' already bound (use a free name instead)")
            }
            items.zipWithIndex.foldLeft(List[Step]()) { case (acc, (currentElement, index)) =>
              val itemNo = index + 1
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
                  List((name, s"$name $itemNo"))
              }
              (try {
                EvalStatus(acc.map(_.evalStatus)) match {
                  case status @ Failed(_, error)  =>
                    val isAssertionError = status.isAssertionError
                    val isSoftAssert = ctx.evaluate(false) { isAssertionError && AssertionMode.isSoft }
                    val failfast = ctx.evaluate(false) { GwenSettings.`gwen.feature.failfast.enabled` }
                    if (failfast && !isSoftAssert) {
                      logger.info(s"Skipping [$name] $itemNo of $noOfElements")
                      engine.transitionStep(foreachSteps(index).copy(withParams = params), Skipped, ctx)
                    } else {
                      logger.info(s"Processing [$name] $itemNo of $noOfElements")
                      engine.evaluateStep(preForeachStepDef, Step(step.sourceRef, if (index == 0) step.keyword else StepKeyword.nameOf(StepKeyword.And), doStep, Nil, None, Nil, None, Pending, params, Nil), ctx)
                    }
                  case _ =>
                    logger.info(s"Processing [$name] $itemNo of $noOfElements")
                    engine.evaluateStep(preForeachStepDef, Step(step.sourceRef, if (index == 0) step.keyword else StepKeyword.nameOf(StepKeyword.And), doStep, Nil, None, Nil, None, Pending, params, Nil), ctx)
                }
              } finally {
                ctx.topScope.popObject(name)
              }) :: acc
            } reverse
          } finally {
            ctx.topScope.set(name, null)
          }
      }
    val foreachStepDef = preForeachStepDef.copy(withSteps = steps)
    engine.afterStepDef(foreachStepDef, ctx)
    step.copy(withStepDef = Some(foreachStepDef))
  }

}
