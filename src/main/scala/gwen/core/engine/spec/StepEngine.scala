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

package gwen.core.engine.spec

import gwen.core._
import gwen.core.Errors
import gwen.core.engine.EvalContext
import gwen.core.engine.EvalEngine
import gwen.core.model._
import gwen.core.model.gherkin.Scenario
import gwen.core.model.gherkin.Step

import scala.util.Failure
import scala.util.Success
import scala.util.Try
import gwen.core.engine.lambda.CompositeStep
import gwen.core.engine.lambda.composite.ForEachTableRecord
import gwen.core.engine.lambda.composite.ForEachTableRecordAnnotated
import gwen.core.engine.lambda.composite.StepDefCall

/**
  * Step evaluation engine.
  */
trait StepEngine[T <: EvalContext] {
  engine: EvalEngine[T] =>

    /**
    * Evaluates a list of steps.
    */
  def evaluateSteps(parent: Identifiable, steps: List[Step], ctx: T): List[Step] = {
    var behaviorCount = 0
    try {
      steps.foldLeft(List[Step]()) {
        (acc: List[Step], step: Step) => 
          if (!StepKeyword.isAnd(step.keyword)) {
            ctx.addBehavior(BehaviorType.of(step.keyword))
            behaviorCount = behaviorCount + 1 
          }
          evaluateOrTransitionStep(parent, step, acc, ctx) :: acc
      } reverse
    } finally {
      0 until behaviorCount foreach { _ =>
        ctx.popBehavior()
      }
    }
  }

  private def evaluateOrTransitionStep(parent: Identifiable, step: Step, acc: List[Step], ctx: T): Step = {
    EvalStatus(acc.map(_.evalStatus)) match {
      case status @ Failed(_, error) =>
        ctx.evaluate(evaluateStep(parent, step, ctx)) {
          val isAssertionError = status.isAssertionError
          val isHardAssert = ctx.evaluate(false) { AssertionMode.isHard }
          if (!isAssertionError || isHardAssert) {
            transitionStep(parent, step, Skipped, ctx.scopes)
          } else {
            evaluateStep(parent, step, ctx)
          }
        }
      case _ => evaluateStep(parent, step, ctx)
    }
  }

  /**
    * Evaluates a step.
    */
  def evaluateStep(parent: Identifiable, step: Step, ctx: T): Step = {
    val iStep = interpolateStep(step, ctx)
    logger.info(s"Evaluating Step: $iStep")
    beforeStep(parent, iStep.copy(withEvalStatus = Pending), ctx.scopes)
    val eStep = Try(healthCheck(parent, iStep, ctx)) match {
      case Failure(e) => withStep(iStep) { throw e }
      case _ =>
        translateCompositeStep(iStep) orElse {
          translateStepDef(iStep, ctx)
          } map { lambda => 
          withStep(iStep.copy(withEvalStatus = Pending)) { s =>
            lambda(parent, s, ctx)
          }
        } getOrElse {
          Try(translateStep(iStep)) match {
            case Success(lambda) if (iStep.evalStatus.status != StatusKeyword.Failed) => 
              withStep(iStep) { s => s tap { _ => lambda(parent, s, ctx) } }
            case Failure(e) => 
              withStep(iStep) {
                parent match {
                  case scenario: Scenario if scenario.isStepDef && e.isInstanceOf[Errors.UndefinedStepException] =>
                    Errors.recursiveStepDefError(scenario)
                  case _ =>
                    throw e
                }
              }
            case _ =>
              iStep
          }
        }
    }
    finaliseStep(eStep, ctx) tap { fStep =>
      logStatus(fStep)
      afterStep(fStep, ctx.scopes)
    }
  }

  private def interpolateStep(step: Step, ctx: T): Step = {
    val pStep = withStep(step) { ctx.interpolateParams }
    withStep(pStep) { ctx.interpolate }
  }

  private def healthCheck(parent: Identifiable, step: Step, ctx: T): Unit = {
    if (step.index == 0 && (parent.isInstanceOf[Scenario] && !parent.asInstanceOf[Scenario].isStepDef)) {
      healthCheck(parent, step, ctx.scopes)
    }
  }

  /**
    * Evaluates a step and captures the result.
    * 
    * @param step the step to evaluate
    * @param stepFunction the step evaluator function
    */
  private def withStep(step: Step)(stepFunction: Step => Step): Step = {
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

  private def translateStepDef(step: Step, ctx: T): Option[CompositeStep[T]] = {
    ctx.getStepDef(step.name) match {
      case Some((stepDef, _)) if stepDef.isForEach && stepDef.isDataTable =>
        val dataTable = ForEachTableRecord.parseFlatTable {
          stepDef.tags.find(_.name.startsWith(s"${ReservedTags.DataTable.toString}(")) map { 
            tag => DataTable(tag, step) 
          }
        }
        Some(new ForEachTableRecordAnnotated(stepDef, step, dataTable, this))
      case Some((stepDef, params)) if !ctx.stepScope.containsScope(stepDef.name) =>
        Some(new StepDefCall(step, stepDef, params, this))
      case _ => 
        None
    }
  }

  /**
    * Binds all accumulated attachments to the given step.
    *
    * @param step the step to bind attachments to
    * @return the step with accumulated attachments
    */
  def finaliseStep(step: Step, ctx: T): Step = {
    if (step.stepDef.isEmpty) {
      step.evalStatus match {
        case failure @ Failed(_, _) if !step.attachments.exists{ case (n, _) => n == "Error details"} =>
          if (!failure.isDisabledError) {
            if (ctx.options.batch) {
              logger.error(ctx.scopes.visible.asString)
            }
            logger.error(failure.error.getMessage)
            ctx.addErrorAttachments(failure)
          }
          logger.whenDebugEnabled {
            logger.error(s"Exception: ", failure.error)
          }
        case _ => // noop
      }
    }
    val fStep = if (ctx.hasAttachments) {
      step.copy(
        withEvalStatus = step.evalStatus, 
        withAttachments = (step.attachments ++ ctx.popAttachments()).sortBy(_._2 .getName()))
    } else {
      
      step
    }
    fStep.evalStatus match {
      case status @ Failed(nanos, error) =>
        if (status.isSustainedError) {
          fStep.copy(withEvalStatus = Sustained(nanos, error))
        } else if (status.isDisabledError) {
          fStep.copy(withEvalStatus = Disabled)
        } else {
          fStep
        }
      case _ =>
        fStep
    }
  }
  
}
