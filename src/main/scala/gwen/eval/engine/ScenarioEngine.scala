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
import gwen.model.gherkin.Background
import gwen.eval.EvalEnvironment

/**
  * Scenario evaluation engine.
  */
trait ScenarioEngine[T <: EvalContext] extends SpecNormaliser with LazyLogging {
    engine: EvalEngine[T] =>

  def evaluateScenarios(parent: Identifiable, scenarios: List[Scenario], ctx: T): List[Scenario] = {
    ctx.withEnv { env =>
      val input = scenarios.map(s => if (s.isOutline) expandCSVExamples(s, ctx) else s)
      if (ctx.options.isParallelScenarios && SpecType.isFeature(env.specType) && StateLevel.scenario.equals(env.stateLevel)) {
        evaluateParallelScenarios(parent, input, env, ctx)
      } else {
        evaluateSequentialScenarios(parent, input, env, ctx)
      }
    }
  }

  private def evaluateSequentialScenarios(parent: Identifiable, scenarios: List[Scenario], env: EvalEnvironment, ctx: T): List[Scenario] = {
    scenarios.foldLeft(List[Scenario]()) {
      (acc: List[Scenario], scenario: Scenario) =>
        evaluateOrTransitionScenario(parent, scenario, env, ctx, acc) :: acc
    } reverse
  }

  private def evaluateParallelScenarios(parent: Identifiable, scenarios: List[Scenario], env: EvalEnvironment, ctx: T): List[Scenario] = {
    val stepDefs = scenarios.filter(_.isStepDef).foldLeft(List[Scenario]()) {
      (acc: List[Scenario], stepDef: Scenario) =>
        evaluateOrTransitionScenario(parent, stepDef, env, ctx, acc) :: acc
    }
    val executor = ParallelExecutors.scenarioInstance
    implicit val ec = ExecutionContext.fromExecutorService(executor)
    val acc = new CopyOnWriteArrayList[Scenario](stepDefs.asJavaCollection)
    val futures = scenarios.filter(!_.isStepDef).to(LazyList).map { scenario =>
      Future {
        val envClone = ctx.withEnv(_.copy())
        val ctxClone = engine.init(ctx.options, Some(envClone))
        try {
          evaluateOrTransitionScenario(parent, scenario, env, ctxClone, acc.asScala.toList) tap { s =>
            acc.add(s)
          }
        } finally {
          ctxClone.close()
        }
      }
    }
    Await.result(
      Future.sequence(futures.force),
      Duration.Inf
    )
    acc.asScala.toList.sortBy(_.sourceRef.map(_.pos.line).getOrElse(0))
  }

  private def evaluateOrTransitionScenario(parent: Identifiable, scenario: Scenario, env: EvalEnvironment, ctx: T, acc: List[Scenario]): Scenario = {
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

   /**
    * Evaluates a given scenario.
    */
  def evaluateScenario(parent: Identifiable, scenario: Scenario, ctx: T): Scenario = {
    ctx.withEnv { env =>
      if (scenario.isStepDef || scenario.isDataTable) {
        if (!scenario.isStepDef) Errors.dataTableError(s"${ReservedTags.StepDef} tag also expected where ${ReservedTags.DataTable} is specified")
        loadStepDef(parent, scenario, ctx)
      } else {
        ctx.lifecycle.beforeScenario(parent, scenario, env.scopes)
        logger.info(s"Evaluating ${scenario.keyword}: $scenario")
        (if (scenario.isOutline) {
          evaluateScenarioOutline(scenario, env, ctx)
        } else {
          scenario.background map  { background => 
            evaluateScenarioWithBackground(scenario, background, ctx)
          } getOrElse {
            evaluateScenarioWithoutBackground(scenario, ctx)
          }
        }) tap { s =>
          ctx.lifecycle.afterScenario(s, env.scopes)
        }
      } tap { s =>
        s.logStatus()
      }
    }
  }

  private def evaluateScenarioWithBackground(scenario: Scenario, background: Background, ctx: T): Scenario = {
    val bg = evaluateBackground(scenario, background, ctx)
    val steps: List[Step] = bg.evalStatus match {
      case Passed(_) => evaluateSteps(scenario, scenario.steps, ctx)
      case Skipped if bg.steps.isEmpty => evaluateSteps(scenario, scenario.steps, ctx)
      case _ => scenario.steps map { _.copy(withEvalStatus = Skipped) }
    }
    scenario.copy(
      withBackground = Some(bg),
      withSteps = steps
    )
  }

  private def evaluateScenarioWithoutBackground(scenario: Scenario, ctx: T): Scenario = {
    val steps = evaluateSteps(scenario, scenario.steps, ctx)
    scenario.copy(
      withBackground = None,
      withSteps = steps
    )
  }

  private def evaluateScenarioOutline(outline: Scenario, env: EvalEnvironment, ctx: T): Scenario = {
    val steps = outline.steps map { step =>
      if (outline.isExpanded) {
        step.copy(withEvalStatus = Loaded)
      } else {
        ctx.lifecycle.transitionStep(outline, step, Loaded, env.scopes)
      }
    }
    val examples = evaluateExamples(outline, outline.examples, ctx)
    outline.copy(
      withSteps = steps,
      withExamples = examples
    )
  }

}
