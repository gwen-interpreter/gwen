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

package gwen.eval.engine

import gwen._
import gwen.eval.EvalContext
import gwen.eval.EvalEngine
import gwen.model._
import gwen.model.gherkin.Scenario
import gwen.model.gherkin.Step

import scala.util.Failure
import scala.util.Success
import scala.util.Try

/**
  * Step evaluation engine.
  */
trait StepEngine[T <: EvalContext] {
  engine: EvalEngine[T] =>

  /**
    * Evaluates a step and captures the result.
    * 
    * @param step the step to evaluate
    * @param stepFunction the step evaluator function
    */
  def withStep(step: Step)(stepFunction: Step => Step): Step = {
    val start = System.nanoTime - step.evalStatus.nanos
    Try(stepFunction(step)) match {
      case Success(eStep) =>
        val status = eStep.stepDef map { case (sd, _) => sd.evalStatus }  getOrElse {
          eStep.evalStatus match {
            case Failed(_, error) => Failed(System.nanoTime - start, error)
            case _ => Passed(System.nanoTime - start)
          }
        }
        eStep.copy(withEvalStatus = status)
      case Failure(error) =>
        val failure = Failed(System.nanoTime - start, new Errors.StepFailure(step, error))
        step.copy(withEvalStatus = failure)
    }
  }

  /**
    * Repeats a step for each element in list of elements of type U.
    */
  def foreach[U](elements: ()=>Seq[U], element: String, parent: Identifiable, step: Step, doStep: String, ctx: T): Step = ctx.withEnv { env =>
    val keyword = FeatureKeyword.nameOf(FeatureKeyword.Scenario)
    val foreachSteps = elements().toList.zipWithIndex map { case (_, index) => 
      step.copy(
        withName = doStep.replaceAll(s"$ZeroChar", ""),
        withKeyword = if (index == 0) step.keyword else StepKeyword.nameOf(StepKeyword.And)
      )
    }
    val tags = List(Tag(ReservedTags.Synthetic), Tag(ReservedTags.ForEach), Tag(ReservedTags.StepDef))
    val preForeachStepDef = Scenario(None, tags, keyword, element, Nil, None, foreachSteps, Nil)
    ctx.lifecycle.beforeStepDef(step, preForeachStepDef, env.scopes)
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
                      ctx.lifecycle.transitionStep(preForeachStepDef, foreachSteps(index), Skipped, env.scopes)
                    } else {
                      logger.info(s"Processing [$element] $elementNumber of $noOfElements")
                      evaluateStep(preForeachStepDef, Step(step.sourceRef, if (index == 0) step.keyword else StepKeyword.nameOf(StepKeyword.And), doStep, Nil, None, Nil, None, Pending), ctx)
                    }
                  case _ =>
                    logger.info(s"Processing [$element] $elementNumber of $noOfElements")
                    evaluateStep(preForeachStepDef, Step(step.sourceRef, if (index == 0) step.keyword else StepKeyword.nameOf(StepKeyword.And), doStep, Nil, None, Nil, None, Pending), ctx)
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
    ctx.lifecycle.afterStepDef(foreachStepDef, env.scopes)
    step.copy(withStepDef = Some((foreachStepDef, Nil)))
  }
  
}
