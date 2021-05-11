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

import gwen.core.GwenSettings
import gwen.core.Errors
import gwen.core.engine.EvalContext
import gwen.core.engine.EvalEngine
import gwen.core.engine.lambda.CompositeStep
import gwen.core.model._
import gwen.core.model.gherkin.Scenario
import gwen.core.model.gherkin.Step

import scala.util.Try

abstract class ForEach[T <: EvalContext](engine: EvalEngine[T], ctx: T) extends CompositeStep[T](engine, ctx) {

  /**
    * Repeats a step for each element in list of elements of type U.
    */
  def evaluateForEach[U](elements: ()=>Seq[U], element: String, parent: Identifiable, step: Step, doStep: String): Step = {
    ctx.withEnv { env =>
      val keyword = FeatureKeyword.nameOf(FeatureKeyword.Scenario)
      val foreachSteps = elements().toList.zipWithIndex map { case (_, index) => 
        step.copy(
          withKeyword = if (index == 0) step.keyword else StepKeyword.nameOf(StepKeyword.And)
        )
      }
      val tags = List(Tag(ReservedTags.Synthetic), Tag(ReservedTags.ForEach), Tag(ReservedTags.StepDef))
      val preForeachStepDef = Scenario(None, tags, keyword, element, Nil, None, foreachSteps, Nil)
      engine.beforeStepDef(step, preForeachStepDef, env.scopes)
      val steps =
        elements() match {
          case Nil =>
            logger.info(s"For-each[$element]: none found")
            Nil
          case elems =>
            val noOfElements = elems.size
            logger.info(s"For-each[$element]: $noOfElements found")
            try {
              if(Try(ctx.getBoundReferenceValue(element)).isSuccess) {
                Errors.ambiguousCaseError(s"For-each element name '$element' already bound (use a free name instead)")
              }
              elems.zipWithIndex.foldLeft(List[Step]()) { case (acc, (currentElement, index)) =>
                val elementNumber = index + 1
                currentElement match {
                  case stringValue: String =>
                    env.topScope.set(element, stringValue)
                    if (ctx.options.dryRun) {
                      env.topScope.pushObject(element, currentElement)
                    }
                  case _ =>
                    env.topScope.pushObject(element, currentElement)
                }
                env.topScope.set(s"$element index", index.toString)
                env.topScope.set(s"$element number", elementNumber.toString)
                (try {
                  EvalStatus(acc.map(_.evalStatus)) match {
                    case status @ Failed(_, error)  =>
                      val isAssertionError = status.isAssertionError
                      val isSoftAssert = ctx.evaluate(false) { isAssertionError && AssertionMode.isSoft }
                      val failfast = ctx.evaluate(false) { GwenSettings.`gwen.feature.failfast` }
                      if (failfast && !isSoftAssert) {
                        logger.info(s"Skipping [$element] $elementNumber of $noOfElements")
                        engine.transitionStep(preForeachStepDef, foreachSteps(index), Skipped, env.scopes)
                      } else {
                        logger.info(s"Processing [$element] $elementNumber of $noOfElements")
                        engine.evaluateStep(preForeachStepDef, Step(step.sourceRef, if (index == 0) step.keyword else StepKeyword.nameOf(StepKeyword.And), doStep, Nil, None, Nil, None, Pending), ctx)
                      }
                    case _ =>
                      logger.info(s"Processing [$element] $elementNumber of $noOfElements")
                      engine.evaluateStep(preForeachStepDef, Step(step.sourceRef, if (index == 0) step.keyword else StepKeyword.nameOf(StepKeyword.And), doStep, Nil, None, Nil, None, Pending), ctx)
                  }
                } finally {
                  env.topScope.popObject(element)
                }) :: acc
              } reverse
            } finally {
              env.topScope.set(element, null)
              env.topScope.set(s"$element index", null)
              env.topScope.set(s"$element number", null)
            }
        }
      val foreachStepDef = preForeachStepDef.copy(withSteps = steps)
      engine.afterStepDef(foreachStepDef, env.scopes)
      step.copy(withStepDef = Some((foreachStepDef, Nil)))
    }
  }

}
