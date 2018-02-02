/*
 * Copyright 2014-2018 Branko Juric, Brady Wood
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
import gwen.GwenSettings
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
    if (scenario.isStepDef || scenario.isDataTable) {
      if (!scenario.isStepDef) dataTableError(s"${Tag.StepDefTag} tag also expected where ${Tag.DataTableTag} is specified")
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
      } else {
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
    val start = System.nanoTime - step.evalStatus.nanos
    val iStep = doEvaluate(step, env) { env.interpolate }
    logger.info(s"Evaluating Step: $iStep")
    var pStep: Option[Step] = None
    doEvaluate(iStep, env) { s =>
      pStep = evaluatePriority(s, env)
      pStep.getOrElse(s)
    }
    val eStep = pStep.filter(_.evalStatus.status != StatusKeyword.Failed).getOrElse {
      if (iStep.evalStatus.status != StatusKeyword.Failed) {
        Try(env.getStepDef(iStep.name)) match {
          case Failure(error) =>
            Step(iStep, Failed(System.nanoTime - start, new StepFailure(iStep, error)))
          case Success(stepDefOpt) =>
            (stepDefOpt match {
              case Some((stepDef, _)) if env.stepScope.containsScope(stepDef.name) => None
              case stepdef => stepdef
            }) match {
              case None =>
                doEvaluate(iStep, env) { step =>
                  step tap { _ =>
                    try {
                      evaluate(step, env)
                    } catch {
                      case e: UndefinedStepException =>
                        stepDefOpt.fold(throw e) { case (stepDef, _) =>
                          recursiveStepDefError(stepDef, step)
                        }
                    }
                  }
                }
              case (Some((stepDef, params))) =>
                evalStepDef(stepDef, iStep, params, env)
            }
        }
      } else {
        iStep
      }
    }
    val fStep = eStep.evalStatus match {
      case Failed(_, e: StepFailure) if e.getCause != null && e.getCause.isInstanceOf[UndefinedStepException] =>
        pStep.getOrElse(eStep)
      case _ =>
        eStep.evalStatus match {
          case Passed(_) => eStep
          case _ => pStep.filter(s => EvalStatus.isEvaluated(s.evalStatus.status)).getOrElse(eStep)
        }
    }
    env.finaliseStep(fStep) tap { step =>
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
  private[eval] def doEvaluate(step: Step, env: T)(evalFunction: (Step) => Step): Step = {
    val start = System.nanoTime - step.evalStatus.nanos
    Try(evalFunction(step)) match {
      case Success(evaluatedStep) =>
        env.foreachStepDefs.get(step.uniqueId) match {
          case Some(foreachStepDef) =>
            env.foreachStepDefs -= step.uniqueId
            Step(evaluatedStep, if (foreachStepDef.steps.nonEmpty) foreachStepDef.evalStatus else Passed(System.nanoTime() - start), foreachStepDef.attachments, foreachStepDef)
          case _ =>
            val status = evaluatedStep.stepDef.map(_.evalStatus ).getOrElse {
              evaluatedStep.evalStatus match {
                case Failed(_, error) => Failed(System.nanoTime - start, error)
                case _ => Passed(System.nanoTime - start)
              }
            }
            Step(evaluatedStep, status)
        }
      case Failure(error) =>
        val failure = Failed(System.nanoTime - start, new StepFailure(step, error))
        Step(step, failure)
    }
  }
  
  private def evalStepDef(stepDef: Scenario, step: Step, params: List[(String, String)], env: T): Step = {
    logger.debug(s"Evaluating ${stepDef.keyword}: ${stepDef.name}")
    env.stepScope.push(stepDef.name, params)
    try {
      val dataTableOpt = stepDef.tags.find(_.name.startsWith("DataTable(")) map { tag => DataTable(tag, step) }
      dataTableOpt foreach { table =>
        env.featureScope.pushObject("table", table)
      }
      try {
        val steps = if (!stepDef.isOutline) evaluateSteps(stepDef.steps, env) else stepDef.steps
        val examples = if (stepDef.isOutline) evaluateExamples(stepDef.examples, env) else stepDef.examples
        Step(step, Scenario(stepDef, None, steps, examples)) tap { _ =>
          logger.debug(s"${stepDef.keyword} evaluated: ${stepDef.name}")
        }
      } finally {
        dataTableOpt foreach { _ =>
          env.featureScope.popObject("table")
        }
      }
    } finally {
      env.stepScope.pop
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
        case Failed(_, _) =>
          env.evaluate(evaluateStep(step, env)) {
            Step(step, Skipped, step.attachments)
          }
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
  def evaluate(step: Step, env: T): Unit = undefinedStepError(step)

  /**
    * Should be overridden to evaluate a priority step (this implementation returns None and can be overridden).
    * For example, a step that calls another step needs to execute with priority to ensure that there is no
    * match conflict between the two (which can occur if the step being called by a step is a StepDef or another step
    * that matches the entire calling step).
    *
    * @param step the step to evaluate
    * @param env the environment context
    * @return Some(step) if the step is an evaluated composite step, None otherwise
    */
  def evaluatePriority(step: Step, env: T): Option[Step] = None

  /**
    * Repeats a step for each element in list of elements of type U.
    */
  def foreach[U](elements: ()=>Seq[U], element: String, step: Step, doStep: String, env: T): Step = {
    val steps =
      elements() match {
        case Nil =>
          logger.info(s"For-each[$element]: none found")
          Nil
        case elems =>
          val noOfElements = elems.size
          logger.info(s"For-each[$element]: $noOfElements found")
          try {
            if(Try(env.getBoundReferenceValue(element)).isSuccess) {
              ambiguousCaseError(s"For-each element name '$element' already bound (use a free name instead)")
            }
            elems.zipWithIndex.foldLeft(List[Step]()) { case (acc, (currentElement, index)) =>
              val elementNumber = index + 1
              currentElement match {
                case stringValue: String =>
                  env.featureScope.set(element, stringValue)
                case _ =>
                  env.featureScope.pushObject(element, currentElement)
              }
              env.featureScope.set(s"$element number", elementNumber.toString)
              (try {
                EvalStatus(acc.map(_.evalStatus)) match {
                  case Failed(_, _) if env.evaluate(false) { GwenSettings.`gwen.feature.failfast` } =>
                    logger.info(s"Skipping [$element] $elementNumber of $noOfElements")
                    Step(step.pos, if (index == 0) step.keyword else StepKeyword.And, doStep, Skipped)
                  case _ =>
                    logger.info(s"Processing [$element] $elementNumber of $noOfElements")
                    evaluateStep(Step(step.pos, if (index == 0) step.keyword else StepKeyword.And, doStep), env)
                }
              } finally {
                env.featureScope.popObject(element)
              }) :: acc
            } reverse
          } finally {
            env.featureScope.set(element, null)
            env.featureScope.set(s"$element number", null)
          }
      }
    val foreachStepDef = new Scenario(List(Tag.StepDefTag, Tag.ForEachTag), element, Nil, None, steps, false, Nil, None)
    env.foreachStepDefs += (step.uniqueId -> foreachStepDef)
    step
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

