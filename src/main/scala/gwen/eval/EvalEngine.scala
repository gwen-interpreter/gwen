/*
 * Copyright 2014-2020 Branko Juric, Brady Wood
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

import gwen._
import gwen.eval.event.LifecycleEventDispatcher
import gwen.model._
import gwen.model.gherkin._

import scala.util.Failure
import scala.util.Try
import scala.util.Success

import com.typesafe.scalalogging.LazyLogging

import java.{util => ju}

/**
  * Base trait for gwen evaluation engines. An evaluation engine performs the
  * actual processing work required to evaluate Gherkin steps.
  *
  * @author Branko Juric
  */
trait EvalEngine[T <: EvalContext] extends LazyLogging with EvalRules {

  // semaphores for managing synchronized StepDefs
  val stepDefSemaphors: ju.concurrent.ConcurrentMap[String, ju.concurrent.Semaphore] = 
    new ju.concurrent.ConcurrentHashMap()

  // dispatches lifecycle events to listeners
  val lifecycle = new LifecycleEventDispatcher()
  
  /**
    * Initialises the engine and returns a new evaluation context.
    * 
    * @param options command line options
    * @param envOpt optional environment context to use
    */
  private [eval] def init(options: GwenOptions, envOpt: Option[EvalEnvironment] = None): T

  /**
    * Should be overridden to evaluate a composite steps (this implementation returns None and can be overridden).
    */
  def evaluateComposite(parent: Identifiable, step: Step, ctx: T): Option[Step] = None
  
  /**
    * Should be overridden to evaluate a given step (this implementation 
    * can be used as a fallback as it simply throws an unsupported step 
    * exception)
    *
    * @param step the step to evaluate
    * @param ctx the evaluation context
    * @throws gwen.Errors.UndefinedStepException unconditionally thrown by
    *         this default implementation
    */
  def evaluate(step: Step, ctx: T): Unit = Errors.undefinedStepError(step)

  /**
    * Evaluates a given scenario.
    */
  private[eval] def evaluateScenario(parent: Identifiable, scenario: Scenario, ctx: T): Scenario = ctx.withEnv { env =>
    if (scenario.isStepDef || scenario.isDataTable) {
      if (!scenario.isStepDef) Errors.dataTableError(s"${ReservedTags.StepDef} tag also expected where ${ReservedTags.DataTable} is specified")
      lifecycle.beforeStepDef(parent, scenario, env.scopes)
      logger.info(s"Loading ${scenario.keyword}: ${scenario.name}")
      env.addStepDef(scenario)
      if (ctx.options.parallel && scenario.isSynchronized) {
        stepDefSemaphors.putIfAbsent(scenario.name, new ju.concurrent.Semaphore(1))
      }
      val tSteps = lifecycle.transitionSteps(scenario, scenario.steps, Loaded, env.scopes)
      val steps = if (!scenario.isOutline) {
        tSteps
      } else {
        scenario.steps
      }
      val examples = if (scenario.isOutline) {
        scenario.examples map { exs =>
          exs.copy(
            withScenarios = exs.scenarios map { s =>
              s.copy(
                withBackground = s.background map { b =>
                  b.copy(withSteps = b.steps map { _.copy(withEvalStatus = Loaded) })
                },
                withSteps = s.steps map { _.copy(withEvalStatus = Loaded) }
              )
            }
          )
        }
      } else  {
        scenario.examples
      }
      scenario.copy(
        withBackground = None,
        withSteps = steps,
        withExamples = examples
      ) tap { s => 
        lifecycle.afterStepDef(s, env.scopes)
      }
    } else {
      lifecycle.beforeScenario(parent, scenario, env.scopes)
      logger.info(s"Evaluating ${scenario.keyword}: $scenario")
      (if (!scenario.isOutline) {
        scenario.background map (bg => evaluateBackground(scenario, bg, ctx)) match {
          case None =>
            scenario.copy(
              withBackground = None,
              withSteps = evaluateSteps(scenario, scenario.steps, ctx)
            )
          case Some(background) =>
            val steps: List[Step] = background.evalStatus match {
              case Passed(_) => evaluateSteps(scenario, scenario.steps, ctx)
              case Skipped if background.steps.isEmpty => evaluateSteps(scenario, scenario.steps, ctx)
              case _ => scenario.steps map { _.copy(withEvalStatus = Skipped) }
            }
            scenario.copy(
              withBackground = Some(background),
              withSteps = steps
            )
        }
      } else {
        val isExpanded = scenario.isExpanded
        scenario.copy(
          withSteps = scenario.steps map { step =>
            if (isExpanded) {
              step.copy(withEvalStatus = Loaded)
            } else {
              lifecycle.transitionStep(scenario, step, Loaded, env.scopes)
            }
          },
          withExamples = evaluateExamples(scenario, scenario.examples, ctx)
        )
      }) tap { scenario =>
        lifecycle.afterScenario(scenario, env.scopes)
      }
    } tap { scenario =>
      logStatus(scenario)
    }
  }

  /**
    * Evaluates a given background.
    */
  private[eval] def evaluateBackground(parent: Identifiable, background: Background, ctx: T): Background = ctx.withEnv { env =>
    lifecycle.beforeBackground(parent, background, env.scopes)
    logger.info(s"Evaluating ${background.keyword}: $background")
    background.copy(withSteps = evaluateSteps(background, background.steps, ctx)) tap { bg =>
      logStatus(bg)
      lifecycle.afterBackground(bg, env.scopes)
    }
  }
  
  /**
    * Evaluates a given step.
    */
  def evaluateStep(parent: Identifiable, step: Step, stepIndex: Int, ctx: T): Step = ctx.withEnv { env =>
    val start = System.nanoTime - step.evalStatus.nanos
    val ipStep = doEvaluate(step) { ctx.interpolateParams }
    val iStep = doEvaluate(ipStep) { ctx.interpolate }
    var cStep: Option[Step] = None
    logger.info(s"Evaluating Step: $iStep")
    lifecycle.beforeStep(parent, iStep, env.scopes)
    val hStep = if (stepIndex == 0 && (parent.isInstanceOf[Scenario] && !parent.asInstanceOf[Scenario].isStepDef)) {
      Try(lifecycle.healthCheck(parent, iStep, env.scopes)) match {
        case Success(_) => iStep
        case Failure(e) => iStep.copy(withEvalStatus = Failed(System.nanoTime - start, e))
      }
    } else iStep
    val eStep = if (hStep != iStep) {
      hStep
    } else {
      doEvaluate(iStep) { s =>
        cStep = evaluateComposite(parent, s, ctx)
        cStep.getOrElse(s)
      }
      val hasSynthetic = cStep.flatMap(s => s.stepDef map { case (sd, _) => sd.isSynthetic }).getOrElse(false)
      cStep.filter(_.evalStatus.status != StatusKeyword.Failed || hasSynthetic).getOrElse {
        if (iStep.evalStatus.status != StatusKeyword.Failed) {
          Try(env.getStepDef(iStep.name)) match {
            case Failure(error) =>
              iStep.copy(withEvalStatus = Failed(System.nanoTime - start, new Errors.StepFailure(iStep, error)))
            case Success(stepDefOpt) =>
              (stepDefOpt match {
                case Some((stepDef, _)) if env.stepScope.containsScope(stepDef.name) => None
                case stepdef => stepdef
              }) match {
                case None =>
                  doEvaluate(iStep) { step =>
                    step tap { _ =>
                      try {
                        evaluate(step, ctx)
                      } catch {
                        case e: Errors.UndefinedStepException =>
                          stepDefOpt.fold(throw e) { case (stepDef, _) =>
                            Errors.recursiveStepDefError(stepDef, step)
                          }
                      }
                    }
                  }
                case (Some((stepDef, params))) =>
                  if (stepDefSemaphors.containsKey(stepDef.name)) {
                    val semaphore = stepDefSemaphors.get(stepDef.name)
                    semaphore.acquire()
                    try {
                      logger.info(s"Synchronized StepDef execution started [StepDef: ${stepDef.name}] [thread: ${Thread.currentThread().getName}]")
                      evalStepDef(iStep, stepDef.copy(), iStep, params, ctx)
                    } finally {
                      logger.info(s"Synchronized StepDef execution finished [StepDef: ${stepDef.name}] [thread: ${Thread.currentThread().getName}]")
                      semaphore.release()
                    }
                  } else {
                    evalStepDef(iStep, stepDef.copy(), iStep, params, ctx)
                  }
              }
          }
        } else {
          iStep
        }
      }
    }
    val fStep = eStep.evalStatus match {
      case Failed(_, e: Errors.StepFailure) if e.getCause != null && e.getCause.isInstanceOf[Errors.UndefinedStepException] =>
        cStep.getOrElse(eStep)
      case _ =>
        eStep.evalStatus match {
          case Passed(_) => eStep
          case _ => cStep.filter(s => EvalStatus.isEvaluated(s.evalStatus.status)).getOrElse(eStep)
        }
    }
    ctx.finaliseStep(fStep) tap { step =>
      logStatus(step)
      lifecycle.afterStep(step, env.scopes)
    }
  }
  
  /**
    * Evaluates a step and captures the result.
    * 
    * @param step the step to evaluate
    * @param stepFunction the step evaluator function
    */
  private[eval] def doEvaluate(step: Step)(stepFunction: Step => Step): Step = {
    val start = System.nanoTime - step.evalStatus.nanos
    Try(stepFunction(step)) match {
      case Success(eStep) =>
        val status = eStep.stepDef.map(_._1.evalStatus).getOrElse {
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

  def evalStepDef(parent: Identifiable, stepDef: Scenario, step: Step, params: List[(String, String)], ctx: T): Step = ctx.withEnv { env =>
    val sdStep = step.copy(
      withStepDef = Some((stepDef, params)),
      withAttachments = stepDef.steps.flatMap(_.attachments)
    )
    logger.debug(s"Evaluating ${stepDef.keyword}: ${stepDef.name}")
    val eStep = doEvaluate(step) { s =>
      checkStepDefRules(sdStep, env)
      step
    }
    if (eStep.evalStatus.status == StatusKeyword.Failed) {
      eStep
    } else {
      env.stepScope.push(stepDef.name, params)
      try {
        val dataTableOpt = stepDef.tags.find(_.name.startsWith("DataTable(")) map { tag => DataTable(tag, step) }
        dataTableOpt foreach { table =>
          env.topScope.pushObject(DataTable.tableKey, table)
        }
        try {
          lifecycle.beforeStepDef(parent, stepDef, env.scopes)
          val steps = if (!stepDef.isOutline) {
            evaluateSteps(stepDef, stepDef.steps, ctx)
          } else {
            val isExpanded = stepDef.isExpanded
            stepDef.steps map { step =>
              if (isExpanded) {
                step.copy(withEvalStatus = Loaded)
              } else {
                lifecycle.transitionStep(stepDef, step, Loaded, env.scopes)
              }
            }
          }
          val examples = if (stepDef.isOutline) {
            evaluateExamples(stepDef, stepDef.examples, ctx)
          } else { 
            stepDef.examples
          }
          val eStepDef = stepDef.copy(
            withBackground = None,
            withSteps = steps,
            withExamples = examples)
          logger.debug(s"${stepDef.keyword} evaluated: ${stepDef.name}")
          lifecycle.afterStepDef(eStepDef, env.scopes) 
          step.copy(
            withStepDef = Some((eStepDef, params)),
            withAttachments = eStepDef.attachments,
            withEvalStatus = eStepDef.evalStatus
          )
        } finally {
          dataTableOpt foreach { _ =>
            env.topScope.popObject(DataTable.tableKey)
          }
        }
      } finally {
        env.stepScope.pop
      }
    }
  }

  private def evaluateExamples(parent: Identifiable, examples: List[Examples], ctx: T): List[Examples] = ctx.withEnv { env => 
    examples map { exs =>
      lifecycle.beforeExamples(parent, exs, env.scopes)
      exs.copy(
        withScenarios = exs.scenarios map { scenario =>
          evaluateScenario(exs, scenario, ctx)
        }
      ) tap { exs =>
        lifecycle.afterExamples(exs, env.scopes)
      }
    }
  }
  
  /**
    * Evaluates a list of steps.
    */
  def evaluateSteps(parent: Identifiable, steps: List[Step], ctx: T): List[Step] = ctx.withEnv { env =>
    var behaviorCount = 0
    try {
      steps.zipWithIndex.foldLeft(List[Step]()) {
        (acc: List[Step], stepWithIndex: (Step, Int)) => 
          val (step, stepIndex) = stepWithIndex
          if (!StepKeyword.isAnd(step.keyword)) {
            env.addBehavior(BehaviorType.of(step.keyword))
            behaviorCount = behaviorCount + 1 
          }
          (EvalStatus(acc.map(_.evalStatus)) match {
            case status @ Failed(_, error) =>
              ctx.evaluate(evaluateStep(parent, step, stepIndex, ctx)) {
                val isAssertionError = status.isAssertionError
                val isHardAssert = ctx.evaluate(false) { AssertionMode.isHard }
                if (!isAssertionError || isHardAssert) {
                  lifecycle.transitionStep(parent, step, Skipped, env.scopes)
                } else {
                  evaluateStep(parent, step, stepIndex, ctx)
                }
              }
            case _ => evaluateStep(parent, step, stepIndex, ctx)
          }) :: acc
      } reverse
    } finally {
      0 until behaviorCount foreach { _ =>
        env.popBehavior()
      }
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
    lifecycle.beforeStepDef(step, preForeachStepDef, env.scopes)
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
                      lifecycle.transitionStep(preForeachStepDef, foreachSteps(index), Skipped, env.scopes)
                    } else {
                      logger.info(s"Processing [$element] $elementNumber of $noOfElements")
                      evaluateStep(preForeachStepDef, Step(step.sourceRef, if (index == 0) step.keyword else StepKeyword.nameOf(StepKeyword.And), doStep, Nil, None, Nil, None, Pending), index, ctx)
                    }
                  case _ =>
                    logger.info(s"Processing [$element] $elementNumber of $noOfElements")
                    evaluateStep(preForeachStepDef, Step(step.sourceRef, if (index == 0) step.keyword else StepKeyword.nameOf(StepKeyword.And), doStep, Nil, None, Nil, None, Pending), index, ctx)
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
    lifecycle.afterStepDef(foreachStepDef, env.scopes)
    step.copy(withStepDef = Some((foreachStepDef, Nil)))
  }
  
  /**
    * Logs the evaluation status of the given spec.
    * 
    * @param spec the spec to log
    * @return the logged status message
    */
  private[eval] def logStatus(spec: Specification): Unit = {
    logStatus(spec.nodeType, spec.feature.name, spec.evalStatus)
  }

  /**
    * Logs the evaluation status of the given node.
    * 
    * @param node the node to log
    * @return the logged status message
    */
  private[eval] def logStatus(node: SpecNode): Unit = {
      logStatus(node.nodeType, node.name, node.evalStatus)
  }
  
  private def logStatus(nodeType: NodeType.Value, name: String, evalStatus: EvalStatus): Unit = { 
    val msg = s"$evalStatus $nodeType: $name"
    evalStatus match {
      case Loaded => logger.debug(msg)
      case Passed(_) => logger.info(msg)
      case Failed(_, _) => logger.error(msg)
      case Sustained(_, _) => logger.warn(msg)
      case _ => logger.warn(msg)
    }
  }
  
}
