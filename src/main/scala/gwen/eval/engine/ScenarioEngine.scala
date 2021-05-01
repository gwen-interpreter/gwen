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
import gwen.model._
import gwen.model.gherkin.Examples
import gwen.model.gherkin.Scenario
import gwen.model.gherkin.Step
import gwen.eval.EvalContext
import gwen.eval.EvalEngine
import gwen.eval.ParallelExecutors
import gwen.eval.SpecNormaliser

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.jdk.CollectionConverters._
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import com.typesafe.scalalogging.LazyLogging

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.ConcurrentMap
import java.util.concurrent.CopyOnWriteArrayList
import java.util.concurrent.Semaphore

/**
  * Scenario evaluation engine.
  */
trait ScenarioEngine[T <: EvalContext] 
  extends BackgroundEngine[T] with StepEngine[T] with SpecNormaliser with LazyLogging {
    engine: EvalEngine[T] =>

  // semaphores for managing synchronized StepDefs
  val stepDefSemaphors: ConcurrentMap[String, Semaphore] = new ConcurrentHashMap()

  def evaluateScenarios(parent: Identifiable, scenarios: List[Scenario], ctx: T): List[Scenario] = {
    ctx.withEnv { env =>
      val input = scenarios.map(s => if (s.isOutline) expandCSVExamples(s, ctx) else s)
      if (ctx.options.isParallelScenarios && SpecType.isFeature(env.specType) && StateLevel.scenario.equals(env.stateLevel)) {
        val stepDefOutput = input.filter(_.isStepDef).foldLeft(List[Scenario]()) {
          (acc: List[Scenario], scenario: Scenario) =>
            evaluateScenario(parent, scenario, acc, ctx) :: acc
        }
        val executor = ParallelExecutors.scenarioInstance
        implicit val ec = ExecutionContext.fromExecutorService(executor)
        val acc = new CopyOnWriteArrayList[Scenario](stepDefOutput.asJavaCollection)
        val outputFutures = input.filter(!_.isStepDef).to(LazyList).map { scenario =>
          Future {
            val envClone = ctx.withEnv(_.copy())
            val parallelCtx = engine.init(ctx.options, Some(envClone))
            try {
              evaluateScenario(parent, scenario, acc.asScala.toList, parallelCtx) tap { s =>
                acc.add(s)
              }
            } finally {
              parallelCtx.close()
            }
          }
        }
        Await.result(
          Future.sequence(outputFutures.force),
          Duration.Inf
        )
        acc.asScala.toList.sortBy(_.sourceRef.map(_.pos.line).getOrElse(0))
      } else {
        (input.foldLeft(List[Scenario]()) {
          (acc: List[Scenario], scenario: Scenario) =>
            evaluateScenario(parent, scenario, acc, ctx) :: acc
        }).reverse
      }
    }
  }

  private def evaluateScenario(parent: Identifiable, scenario: Scenario, acc: List[Scenario], ctx: T): Scenario = {
    ctx.withEnv { env =>
      if (SpecType.isFeature(env.specType) && !scenario.isStepDef) {
        if (StateLevel.scenario.equals(env.stateLevel)) {
          ctx.reset(StateLevel.scenario)
        }
        env.topScope.set("gwen.scenario.name", scenario.name)
      }
      EvalStatus(acc.map(_.evalStatus)) match {
        case status @ Failed(_, error) =>
          val isAssertionError = status.isAssertionError
          val isSoftAssert = ctx.evaluate(false) { isAssertionError && AssertionMode.isSoft }
          val failfast = ctx.evaluate(false) { GwenSettings.`gwen.feature.failfast` }
          val exitOnFail = ctx.evaluate(false) { GwenSettings.`gwen.feature.failfast.exit` }
          if (failfast && !exitOnFail && !isSoftAssert) {
            ctx.lifecycle.transitionScenario(parent, scenario, Skipped, env.scopes)
          } else if (exitOnFail && !isSoftAssert) {
            ctx.lifecycle.transitionScenario(parent, scenario, scenario.evalStatus, env.scopes)
          } else {
            evaluateScenario(parent, scenario, ctx)
          }
        case _ =>
          evaluateScenario(parent, scenario, ctx)
      }
    }
  }

   /**
    * Evaluates a given scenario.
    */
  def evaluateScenario(parent: Identifiable, scenario: Scenario, ctx: T): Scenario = ctx.withEnv { env =>
    if (scenario.isStepDef || scenario.isDataTable) {
      if (!scenario.isStepDef) Errors.dataTableError(s"${ReservedTags.StepDef} tag also expected where ${ReservedTags.DataTable} is specified")
      ctx.lifecycle.beforeStepDef(parent, scenario, env.scopes)
      logger.info(s"Loading ${scenario.keyword}: ${scenario.name}")
      env.addStepDef(scenario)
      if (ctx.options.parallel && scenario.isSynchronized) {
        stepDefSemaphors.putIfAbsent(scenario.name, new Semaphore(1))
      }
      val tSteps = ctx.lifecycle.transitionSteps(scenario, scenario.steps, Loaded, env.scopes)
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
        ctx.lifecycle.afterStepDef(s, env.scopes)
      }
    } else {
      ctx.lifecycle.beforeScenario(parent, scenario, env.scopes)
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
              ctx.lifecycle.transitionStep(scenario, step, Loaded, env.scopes)
            }
          },
          withExamples = evaluateExamples(scenario, scenario.examples, ctx)
        )
      }) tap { scenario =>
        ctx.lifecycle.afterScenario(scenario, env.scopes)
      }
    } tap { scenario =>
      scenario.logStatus()
    }
  }

  def evaluateExamples(parent: Identifiable, examples: List[Examples], ctx: T): List[Examples] = ctx.withEnv { env => 
    examples map { exs =>
      ctx.lifecycle.beforeExamples(parent, exs, env.scopes)
      exs.copy(
        withScenarios = exs.scenarios map { scenario =>
          evaluateScenario(exs, scenario, ctx)
        }
      ) tap { exs =>
        ctx.lifecycle.afterExamples(exs, env.scopes)
      }
    }
  }

  /**
    * Evaluates a list of steps.
    */
  def evaluateSteps(parent: Identifiable, steps: List[Step], ctx: T): List[Step] = ctx.withEnv { env =>
    var behaviorCount = 0
    try {
      steps.foldLeft(List[Step]()) {
        (acc: List[Step], step: Step) => 
          if (!StepKeyword.isAnd(step.keyword)) {
            env.addBehavior(BehaviorType.of(step.keyword))
            behaviorCount = behaviorCount + 1 
          }
          (EvalStatus(acc.map(_.evalStatus)) match {
            case status @ Failed(_, error) =>
              ctx.evaluate(evaluateStep(parent, step, ctx)) {
                val isAssertionError = status.isAssertionError
                val isHardAssert = ctx.evaluate(false) { AssertionMode.isHard }
                if (!isAssertionError || isHardAssert) {
                  ctx.lifecycle.transitionStep(parent, step, Skipped, env.scopes)
                } else {
                  evaluateStep(parent, step, ctx)
                }
              }
            case _ => evaluateStep(parent, step, ctx)
          }) :: acc
      } reverse
    } finally {
      0 until behaviorCount foreach { _ =>
        env.popBehavior()
      }
    }
  }

  /**
    * Evaluates a step.
    */
  def evaluateStep(parent: Identifiable, step: Step, ctx: T): Step = ctx.withEnv { env =>
    val start = System.nanoTime - step.evalStatus.nanos
    val ipStep = withStep(step) { ctx.interpolateParams }
    val iStep = withStep(ipStep) { ctx.interpolate }
    var cStep: Option[Step] = None
    logger.info(s"Evaluating Step: $iStep")
    ctx.lifecycle.beforeStep(parent, iStep, env.scopes)
    val hStep = if (step.index == 0 && (parent.isInstanceOf[Scenario] && !parent.asInstanceOf[Scenario].isStepDef)) {
      Try(ctx.lifecycle.healthCheck(parent, iStep, env.scopes)) match {
        case Success(_) => iStep
        case Failure(e) => iStep.copy(withEvalStatus = Failed(System.nanoTime - start, e))
      }
    } else iStep
    val eStep = if (hStep != iStep) {
      hStep
    } else {
      withStep(iStep) { s =>
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
                  withStep(iStep) { step =>
                    step tap { _ =>
                      try {
                        evaluate(step, ctx)
                      } catch {
                        case e: Errors.UndefinedStepException =>
                          e.printStackTrace()
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
      step.logStatus()
      ctx.lifecycle.afterStep(step, env.scopes)
    }
  }

  def evalStepDef(parent: Identifiable, stepDef: Scenario, step: Step, params: List[(String, String)], ctx: T): Step = ctx.withEnv { env =>
    val sdStep = step.copy(
      withStepDef = Some((stepDef, params)),
      withAttachments = stepDef.steps.flatMap(_.attachments)
    )
    logger.debug(s"Evaluating ${stepDef.keyword}: ${stepDef.name}")
    val eStep = withStep(step) { s =>
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
          ctx.lifecycle.beforeStepDef(parent, stepDef, env.scopes)
          val steps = if (!stepDef.isOutline) {
            evaluateSteps(stepDef, stepDef.steps, ctx)
          } else {
            val isExpanded = stepDef.isExpanded
            stepDef.steps map { step =>
              if (isExpanded) {
                step.copy(withEvalStatus = Loaded)
              } else {
                ctx.lifecycle.transitionStep(stepDef, step, Loaded, env.scopes)
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
          ctx.lifecycle.afterStepDef(eStepDef, env.scopes) 
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

}
