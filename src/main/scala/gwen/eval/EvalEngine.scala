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

import com.typesafe.scalalogging.slf4j.LazyLogging
import gwen.dsl.Step
import gwen.Predefs._
import gwen.errors._
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import gwen.dsl.Passed
import gwen.dsl.Failed
import gwen.dsl.StatusKeyword
import gwen.dsl.Scenario
import gwen.dsl.EvalStatus
import gwen.dsl.Skipped
import gwen.dsl.SpecType
import gwen.dsl.Loaded

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
    * Evaluates a given step.
    * 
    * @param step the step to evaluate
    * @param env the environment context
    */
  def evaluateStep(step: Step, env: T): Step = {
    val iStep = doEvaluate(step, env) { env.interpolate(_) }
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
            case Some((stepDef, _)) if (env.localScope.containsScope(stepDef.name)) => None
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
    }) tap { step =>
      env.resetAttachments
    }
  }
  
  private def evalStepDef(stepDef: Scenario, step: Step, params: List[(String, String)], env: T): Step = {
    logger.debug(s"Evaluating StepDef: ${stepDef.name}")
    env.localScope.push(stepDef.name, params)
    try {
      Step(step, Scenario(stepDef, None, evaluateSteps(stepDef.steps, env))) tap { s =>
        logger.debug(s"StepDef evaluated: ${stepDef.name}")
      }
    } finally {
      env.localScope.pop
    }
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
    override val engineA = EvalEngine.this
    override val engineB = otherEngine
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
      logStatusMsg(s"${if (SpecType.meta.toString() == node) Loaded else status} $node: $name", status)
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
class StepFailure(step: Step, cause: Throwable) extends RuntimeException(s"Failed step [at line ${step.pos.line}]: ${step}: ${cause.getMessage()}", cause)

