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
import gwen.eval.ParallelExecutors
import gwen.eval.SpecNormaliser
import gwen.model._
import gwen.model.gherkin.Scenario
import gwen.model.gherkin.Step

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.jdk.CollectionConverters._

import com.typesafe.scalalogging.LazyLogging

import java.util.concurrent.CopyOnWriteArrayList

/**
  * Scenario evaluation engine.
  */
trait ScenarioEngine[T <: EvalContext] extends SpecNormaliser with LazyLogging {
    engine: EvalEngine[T] =>

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
      loadStepDef(parent, scenario, ctx)
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

}
