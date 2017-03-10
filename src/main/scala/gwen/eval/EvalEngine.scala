/*
 * Copyright 2014-2015 Branko Juric, Brady Wood
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

package gwen.eval

import com.typesafe.scalalogging.LazyLogging
import gwen.dsl._
import gwen.Predefs._
import gwen.errors._

import scala.util.Try
import scala.util.Success
import scala.util.Failure

/**
  * Base trait for gwen evaluation engines. An evaluation engine performs the
  * actual processing work required to evaluate individual
  * [[gwen.dsl.Step Step]] expressions. This work can be done using various
  * available frameworks and technologies. This trait serves as a base for
  * concrete engine implementations coupled to specific such frameworks or
  * technologies.
  *
  * Evaluation engines are never invoked directly, but are rather invoked by
  * the [[GwenInterpreter]].  This interpreter can mix in any evaluation engine 
  * that has this trait.
  *
  * @author Branko Juric
  */
trait EvalEngine[T <: EnvContext] extends LazyLogging {
  
  /**
    * Initialises the engine and returns a bootstrapped evaluation context.
    * This is a lifecycle method and as such it is called by the
    * [[GwenInterpreter]] at the appropriate times.
    * 
    * @param options command line options
    * @param scopes initial data scopes
    */
  private [eval] def init(options: GwenOptions, scopes: ScopedDataStack): T

  /**
    * Evaluates a given scenario.
    *
    * @param scenario the scenario to evaluate
    * @param env the environment context
    * @return the evaluated scenario
    */
  private[eval] def evaluateScenario(scenario: Scenario, env: T): Scenario = {
    if (scenario.isStepDef) {
      logger.info(s"Loading ${scenario.keyword}: ${scenario.name}")
      env.addStepDef(scenario)
      val steps =
        if (!scenario.isOutline)
          scenario.steps map { step => Step(step, Loaded, step.attachments) }
        else scenario.steps
      val examples =
        if (scenario.isOutline)
          scenario.examples map { exs =>
            Examples(exs, exs.scenarios map { s =>
              Scenario(
                s,
                s.background map { b =>
                  Background(b, b.steps map { step => Step(step, Loaded, step.attachments) })
                },
                s.steps map { step => Step(step, Loaded, step.attachments) },
                s.examples
              )
            })
          }
        else scenario.examples
      Scenario(scenario, None, steps, examples)
    } else {
      logger.info(s"Evaluating ${scenario.keyword}: $scenario")
      if (!scenario.isOutline) {
        scenario.background map (evaluateBackground(_, env)) match {
          case None =>
            Scenario(scenario, None, evaluateSteps(scenario.steps, env), scenario.examples)
          case Some(background) =>
            val steps: List[Step] = background.evalStatus match {
              case Passed(_) => evaluateSteps(scenario.steps, env)
              case Skipped if background.steps.isEmpty => evaluateSteps(scenario.steps, env)
              case _ => scenario.steps map { step =>
                Step(step, Skipped, step.attachments)
              }
            }
            Scenario(scenario, Some(background), steps, scenario.examples)
        }
      }
      else {
        Scenario(scenario, scenario.background, scenario.steps, evaluateExamples(scenario.examples, env))
      }
    } tap { scenario =>
      logStatus(scenario.keyword, scenario.toString, scenario.evalStatus)
    }
  }

  /**
    * Evaluates a given background.
    *
    * @param background the background to evaluate
    * @param env the environment context
    * @return the evaluated background
    */
  private[eval] def evaluateBackground(background: Background, env: T): Background = {
    logger.info(s"Evaluating ${Background.keyword}: $background")
    Background(background, evaluateSteps(background.steps, env)) tap { bg =>
      logStatus(Background.keyword, bg.toString, bg.evalStatus)
    }
  }
  
  /**
    * Evaluates a given step.
    * 
    * @param step the step to evaluate
    * @param env the environment context
    */
  def evaluateStep(step: Step, env: T): Step = {
    val iStep = doEvaluate(step, env) { env.interpolate }
    (if (iStep.evalStatus.status != StatusKeyword.Failed) {
      logger.info(s"Evaluating Step: $iStep")
      val start = System.nanoTime - step.evalStatus.nanos
      Try(env.getStepDef(iStep.expression)) match {
        case Failure(error) =>
          val failure = Failed(System.nanoTime - start, new StepFailure(step, error))
          env.fail(failure)
          Step(iStep, failure, env.attachments)
        case Success(stepDefOpt) =>
          (stepDefOpt match {  
            case Some((stepDef, _)) if env.paramScope.containsScope(stepDef.name) => None
            case stepdef => stepdef 
          }) match {
            case None =>
              doEvaluate(iStep, env) { step =>
                try {
                  evaluate(step, env)
                } catch {
                  case e: UndefinedStepException =>
                    stepDefOpt.fold(throw e) { case (stepDef, _) => recursiveStepDefError(stepDef, step) }
                }
                step
              }
            case (Some((stepDef, params))) => 
              evalStepDef(stepDef, iStep, params, env)
          }
      }
    } else {
      iStep
    }) tap { step =>
      logStatus("Step", step.toString, step.evalStatus)
    }
  }
  
  /**
    * Evaluates a step and captures the result.
    * 
    * @param step the step to evaluate
    * @param env the environment context
    * @param evalFunction the step evaluation function
    */
  private def doEvaluate(step: Step, env: T)(evalFunction: (Step) => Step): Step = {
    val start = System.nanoTime - step.evalStatus.nanos
    (Try(evalFunction(step)) match {
      case Success(evaluatedStep) =>
        Step(evaluatedStep, evaluatedStep.stepDef.map(_.evalStatus ).getOrElse(Passed(System.nanoTime - start)), env.attachments)
      case Failure(error) =>
        val failure = Failed(System.nanoTime - start, new StepFailure(step, error))
        env.fail(failure)
        Step(step, failure, env.attachments)
    }) tap { _ =>
      env.resetAttachments()
    }
  }
  
  private def evalStepDef(stepDef: Scenario, step: Step, params: List[(String, String)], env: T): Step = {
    logger.debug(s"Evaluating ${stepDef.keyword}: ${stepDef.name}")
    env.paramScope.push(stepDef.name, params)
    try {
      val steps = if (!stepDef.isOutline) evaluateSteps(stepDef.steps, env) else stepDef.steps
      val examples = if (stepDef.isOutline) evaluateExamples(stepDef.examples, env) else stepDef.examples
      Step(step, Scenario(stepDef, None, steps, examples)) tap { _ =>
        logger.debug(s"${stepDef.keyword} evaluated: ${stepDef.name}")
      }
    } finally {
      env.paramScope.pop
    }
  }

  private def evaluateExamples(examples: List[Examples], env: T): List[Examples] = examples map { exs =>
    Examples(
      exs,
      exs.scenarios map { scenario =>
        evaluateScenario(scenario, env)
      }
    )
  }
  
  /**
    * Evaluates a list of steps.
    * 
    * @param steps the steps to evaluate
    * @param env the environment context
    * @return the list of evaluated steps
    */
  def evaluateSteps(steps: List[Step], env: T): List[Step] = steps.foldLeft(List[Step]()) {
    (acc: List[Step], step: Step) => 
      (EvalStatus(acc.map(_.evalStatus)) match {
        case Failed(_, _) => env.execute(Step(step, Skipped, step.attachments)).getOrElse(evaluateStep(step, env))
        case _ => evaluateStep(step, env)
      }) :: acc
  } reverse

  /**
    * Should be overridden to evaluate a given step (this implementation 
    * can be used as a fallback as it simply throws an unsupported step 
    * exception)
    *
    * @param step the step to evaluate
    * @param env the environment context
    * @throws gwen.errors.UndefinedStepException unconditionally thrown by 
    *         this default implementation
    */
  def evaluate(step: Step, env: T): Unit = {
    step.expression match {
      case _ => undefinedStepError(step)
    }
  }
  
  /**
    * Adds another engine to this one to create a new hybrid engine.
    * 
    * @param otherEngine the other engine
    */
  def +[U <: EnvContext](otherEngine: EvalEngine[U]) = new HybridEvalEngine[T, U] {
    override val engineA: EvalEngine[T] = EvalEngine.this
    override val engineB: EvalEngine[U] = otherEngine
  }
  
  /**
    * Logs the evaluation status of the given node.
    * 
    * @param node the node to log the evaluation status of
    * @param name the name of the node that failed
    * @param status the evaluation status
    * @return the logged status message
    */
  private[eval] def logStatus(node: String, name: String, status: EvalStatus) = {
      logStatusMsg(s"${if (SpecType.meta.toString == node) Loaded else status} $node: $name", status)
  }
  
  private def logStatusMsg(msg: String, status: EvalStatus) = status match {
    case Loaded => 
      logger.debug(msg)
    case Passed(_) => 
      logger.info(msg)
    case Failed(_, _) => 
      logger.error(msg)
    case _ => 
      logger.warn(msg)
  }
  
}

/** Hybrid engine trait. */
trait HybridEvalEngine[A <: EnvContext, B <: EnvContext] extends EvalEngine[HybridEnvContext[A, B]] {
  
  val engineA: EvalEngine[A]
  val engineB: EvalEngine[B]
  
  override def init(options: GwenOptions, scopes: ScopedDataStack): HybridEnvContext[A, B] = 
    new HybridEnvContext(
      engineA.init(options, scopes), 
      engineB.init(options, scopes), 
      options,
      scopes)
  
  override def evaluate(step: Step, env: HybridEnvContext[A, B]) {
    try {
      engineA.evaluate(step, env.envA)
    } catch {
      case _: UndefinedStepException => 
        engineB.evaluate(step, env.envB)
    }
  }
  
}

/** Signals a step that failed to execute. */
class StepFailure(step: Step, cause: Throwable) extends RuntimeException(s"Failed step [at line ${step.pos.line}]: $step: ${cause.getMessage}", cause)

