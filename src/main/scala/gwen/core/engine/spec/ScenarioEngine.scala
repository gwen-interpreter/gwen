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
import gwen.core.engine.EvalContext
import gwen.core.engine.EvalEngine
import gwen.core.engine.ParallelExecutors
import gwen.core.engine.SpecNormaliser
import gwen.core.model._
import gwen.core.model.gherkin.Scenario
import gwen.core.model.gherkin.Step

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.jdk.CollectionConverters._

import com.typesafe.scalalogging.LazyLogging

import java.util.concurrent.CopyOnWriteArrayList
import gwen.core.model.gherkin.Background

/**
  * Scenario evaluation engine.
  */
trait ScenarioEngine[T <: EvalContext] extends SpecNormaliser with LazyLogging {
    engine: EvalEngine[T] =>

  private [spec] def evaluateScenarios(parent: Identifiable, scenarios: List[Scenario], ctx: T): List[Scenario] = {
    val input = scenarios.map(s => if (s.isOutline) expandCSVExamples(s, ctx) else s)
    if (ctx.options.isParallelScenarios && SpecType.isFeature(ctx.specType) && StateLevel.scenario.equals(ctx.stateLevel)) {
      evaluateParallelScenarios(parent, input, ctx)
    } else {
      evaluateSequentialScenarios(parent, input, ctx)
    }
  }

  private def evaluateSequentialScenarios(parent: Identifiable, scenarios: List[Scenario], ctx: T): List[Scenario] = {
    scenarios.foldLeft(List[Scenario]()) {
      (acc: List[Scenario], scenario: Scenario) =>
        evaluateOrTransitionScenario(parent, scenario, ctx, acc) :: acc
    } reverse
  }

  private def evaluateParallelScenarios(parent: Identifiable, scenarios: List[Scenario], ctx: T): List[Scenario] = {
    val stepDefs = scenarios.filter(_.isStepDef).foldLeft(List[Scenario]()) {
      (acc: List[Scenario], stepDef: Scenario) =>
        evaluateOrTransitionScenario(parent, stepDef, ctx, acc) :: acc
    }
    val executor = ParallelExecutors.scenarioInstance
    implicit val ec = ExecutionContext.fromExecutorService(executor)
    val acc = new CopyOnWriteArrayList[Scenario](stepDefs.asJavaCollection)
    val futures = scenarios.filter(!_.isStepDef).to(LazyList).map { scenario =>
      Future {
        val ctxClone = engine.init(ctx.options, ctx.cloneState)
        try {
          evaluateOrTransitionScenario(parent, scenario, ctxClone, acc.asScala.toList) tap { s =>
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

  private def evaluateOrTransitionScenario(parent: Identifiable, scenario: Scenario, ctx: T, acc: List[Scenario]): Scenario = {
    if (SpecType.isFeature(ctx.specType) && !scenario.isStepDef) {
      if (StateLevel.scenario.equals(ctx.stateLevel)) {
        ctx.reset(StateLevel.scenario)
      }
      ctx.topScope.set("gwen.scenario.name", scenario.name)
    }
    EvalStatus(acc.map(_.evalStatus)) match {
      case status @ Failed(_, error) =>
        val isAssertionError = status.isAssertionError
        val isSoftAssert = ctx.evaluate(false) { isAssertionError && AssertionMode.isSoft }
        val failfast = ctx.evaluate(false) { GwenSettings.`gwen.feature.failfast` }
        val exitOnFail = ctx.evaluate(false) { GwenSettings.`gwen.feature.failfast.exit` }
        if (failfast && !exitOnFail && !isSoftAssert) {
          transitionScenario(parent, scenario, Skipped, ctx.scopes)
        } else if (exitOnFail && !isSoftAssert) {
          transitionScenario(parent, scenario, scenario.evalStatus, ctx.scopes)
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
  private [spec] def evaluateScenario(parent: Identifiable, scenario: Scenario, ctx: T): Scenario = {
    if (scenario.isStepDef || scenario.isDataTable) {
      if (!scenario.isStepDef) Errors.dataTableError(s"${ReservedTags.StepDef} tag also expected where ${ReservedTags.DataTable} is specified")
      loadStepDef(parent, scenario, ctx)
    } else {
      beforeScenario(parent, scenario, ctx.scopes)
      logger.info(s"Evaluating ${scenario.keyword}: $scenario")
      (if (scenario.isOutline) {
        evaluateScenarioOutline(scenario, ctx)
      } else {
        scenario.background map  { background => 
          evaluateScenarioWithBackground(scenario, background, ctx)
        } getOrElse {
          evaluateScenarioWithoutBackground(scenario, ctx)
        }
      }) tap { s =>
        afterScenario(s, ctx.scopes)
      }
    } tap { s =>
      logStatus(s)
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

  private def evaluateScenarioOutline(outline: Scenario, ctx: T): Scenario = {
    val steps = outline.steps map { step =>
      if (outline.isExpanded) {
        step.copy(withEvalStatus = Loaded)
      } else {
        transitionStep(outline, step, Loaded, ctx.scopes)
      }
    }
    val examples = evaluateExamples(outline, outline.examples, ctx)
    outline.copy(
      withSteps = steps,
      withExamples = examples
    )
  }

}
