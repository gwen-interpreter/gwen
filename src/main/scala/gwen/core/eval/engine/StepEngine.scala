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

package gwen.core.eval.engine

import gwen.core._
import gwen.core.Errors
import gwen.core.eval.EvalContext
import gwen.core.eval.EvalEngine
import gwen.core.eval.EvalEnvironment
import gwen.core.model._
import gwen.core.model.gherkin.Scenario
import gwen.core.model.gherkin.Step

import scala.util.Failure
import scala.util.Success
import scala.util.Try

/**
  * Step evaluation engine.
  */
trait StepEngine[T <: EvalContext] {
  engine: EvalEngine[T] =>

    /**
    * Evaluates a list of steps.
    */
  private [engine] def evaluateSteps(parent: Identifiable, steps: List[Step], ctx: T): List[Step] = {
    ctx.withEnv { env =>
      var behaviorCount = 0
      try {
        steps.foldLeft(List[Step]()) {
          (acc: List[Step], step: Step) => 
            if (!StepKeyword.isAnd(step.keyword)) {
              env.addBehavior(BehaviorType.of(step.keyword))
              behaviorCount = behaviorCount + 1 
            }
            evaluateOrTransitionStep(parent, step, acc, env, ctx) :: acc
        } reverse
      } finally {
        0 until behaviorCount foreach { _ =>
          env.popBehavior()
        }
      }
    }
  }

  private def evaluateOrTransitionStep(parent: Identifiable, step: Step, acc: List[Step], env: EvalEnvironment, ctx: T): Step = {
    EvalStatus(acc.map(_.evalStatus)) match {
      case status @ Failed(_, error) =>
        ctx.evaluate(evaluateStep(parent, step, ctx)) {
          val isAssertionError = status.isAssertionError
          val isHardAssert = ctx.evaluate(false) { AssertionMode.isHard }
          if (!isAssertionError || isHardAssert) {
            lifecycle.transitionStep(parent, step, Skipped, env.scopes)
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
    ctx.withEnv { env =>
      val start = System.nanoTime - step.evalStatus.nanos
      val pStep = withStep(step) { ctx.interpolateParams }
      val iStep = withStep(pStep) { ctx.interpolate }
      logger.info(s"Evaluating Step: $iStep")
      lifecycle.beforeStep(parent, iStep.copy(withEvalStatus = Pending), env.scopes)
      val hStep = healthCheck(parent, iStep, env, ctx, start)
      val eStep = if (hStep == iStep) {
        translateComposite(parent, iStep, env, ctx) map { UnitStep => 
          withStep(iStep.copy(withEvalStatus = Pending)) { s =>
            UnitStep(parent, s)
          }
        } getOrElse {
          Try(env.getStepDef(iStep.name)) match {
            case Success(stepDefOpt) =>
              stepDefOpt.filter { case (stepDef, _) => 
                !env.stepScope.containsScope(stepDef.name)
              } map { case (stepDef, params) =>
                withStep(iStep) { s =>
                  evaluateStepDef(s, stepDef.copy(), s, params, ctx)   
                }
              } getOrElse {
                Try(translate(parent, iStep, env, ctx)) match {
                  case Success(op) => 
                    if (iStep.evalStatus.status != StatusKeyword.Failed) {
                      withStep(iStep) { s => s tap { _ => op(parent, s) } }
                    } else {
                      iStep
                    }
                  case Failure(e) if e.isInstanceOf[Errors.UndefinedStepException] =>
                    stepDefOpt.fold(throw e) { case (stepDef, _) =>
                      Errors.recursiveStepDefError(stepDef, iStep)
                    }
                }
              }
            case Failure(error) =>
              withStep(iStep) { throw error }
          }
        }
      } else {
        hStep
      }
      finaliseStep(eStep, env, ctx) tap { fStep =>
        logStatus(fStep)
        lifecycle.afterStep(fStep, env.scopes)
      }
    }
  }

  private def healthCheck(parent: Identifiable, step: Step, env: EvalEnvironment, ctx: T, startNanos: Long): Step = {
    if (step.index == 0 && (parent.isInstanceOf[Scenario] && !parent.asInstanceOf[Scenario].isStepDef)) {
      Try(lifecycle.healthCheck(parent, step, env.scopes)) match {
        case Success(_) => step
        case Failure(e) => step.copy(withEvalStatus = Failed(System.nanoTime - startNanos, e))
      }
    } else step
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

  /**
    * Binds all accumulated attachments to the given step.
    *
    * @param step the step to bind attachments to
    * @return the step with accumulated attachments
    */
  def finaliseStep(step: Step, env: EvalEnvironment, ctx: T): Step = {
    if (step.stepDef.isEmpty) {
      step.evalStatus match {
        case failure @ Failed(_, _) if !step.attachments.exists{ case (n, _) => n == "Error details"} =>
          if (!failure.isDisabledError) {
            if (ctx.options.batch) {
              logger.error(env.scopes.visible.asString)
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
    val fStep = if (env.hasAttachments) {
      step.copy(
        withEvalStatus = step.evalStatus, 
        withAttachments = (step.attachments ++ env.popAttachments()).sortBy(_._2 .getName()))
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
