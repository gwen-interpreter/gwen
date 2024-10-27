/*
 * Copyright 2021-2024 Branko Juric, Brady Wood
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
import gwen.core.data.DataRecord
import gwen.core.eval.EvalContext
import gwen.core.eval.EvalEngine
import gwen.core.eval.ParallelExecutors
import gwen.core.node.GwenNode
import gwen.core.node.gherkin.Background
import gwen.core.node.gherkin.Dialect
import gwen.core.node.gherkin.Annotations
import gwen.core.node.gherkin.Examples
import gwen.core.node.gherkin.Scenario
import gwen.core.node.gherkin.Spec
import gwen.core.node.gherkin.SpecNormaliser
import gwen.core.node.gherkin.SpecType
import gwen.core.node.gherkin.Step
import gwen.core.state.StateLevel
import gwen.core.status._

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.jdk.CollectionConverters._
import scala.util.chaining._

import com.typesafe.scalalogging.LazyLogging

import java.util.concurrent.CopyOnWriteArrayList

/**
  * Scenario evaluation engine.
  */
trait ScenarioEngine[T <: EvalContext] extends SpecNormaliser with LazyLogging with ImplicitValueKeys {
  engine: EvalEngine[T] =>

  private [engine] def evaluateScenarios(parent: GwenNode, scenarios: List[Scenario], dataRecord: Option[DataRecord], ctx: T): List[Scenario] = {
    val loadingMeta = if (parent.isInstanceOf[Spec]) {
      parent.asInstanceOf[Spec].isMeta
    } else {
      false
    }
    val input = scenarios.map(s => if (s.isOutline && (!loadingMeta || !s.isStepDef)) expandExamples(s, dataRecord, ctx) else s)
    val parallelScenarios = ctx.options.parallelScenarios(ctx.stateLevel) && ctx.specType.isFeature
    val parallelExamples = parent.isInstanceOf[Examples] && parent.asInstanceOf[Examples].isParallel
    if (parallelScenarios || parallelExamples) {
      evaluateParallelScenarios(parent, input, parallelExamples, ctx)
    } else {
      evaluateSequentialScenarios(parent, input, ctx)
    }
  }

  private def evaluateSequentialScenarios(parent: GwenNode, scenarios: List[Scenario], ctx: T): List[Scenario] = {
    scenarios.foldLeft(List[Scenario]()) {
      (acc: List[Scenario], scenario: Scenario) =>
        evaluateOrTransitionScenario(parent, scenario, ctx, acc) :: acc
    } reverse
  }

  private def evaluateParallelScenarios(parent: GwenNode, scenarios: List[Scenario], parallelExamples: Boolean, ctx: T): List[Scenario] = {
    val stepDefs = scenarios.filter(_.isStepDef).foldLeft(List[Scenario]()) {
      (acc: List[Scenario], stepDef: Scenario) =>
        evaluateOrTransitionScenario(parent, stepDef, ctx, acc) :: acc
    }
    val executor = ParallelExecutors.scenarioInstance
    implicit val ec = ExecutionContext.fromExecutorService(executor)
    val acc = new CopyOnWriteArrayList[Scenario](stepDefs.asJavaCollection)
    val futures = scenarios.filter(!_.isStepDef).to(LazyList).map { scenario =>
      Future {
        val stateClone = if (parallelExamples) ctx.deepCloneState else ctx.shallowCloneState
        val ctxClone = engine.init(ctx.options.copy(parallel = true), stateClone)
        try {
          val language = ctx.featureScope.get(`gwen.feature.language`)
          Dialect.withLanguage(language) {
            val scenarioResult = evaluateOrTransitionScenario(parent, scenario, ctxClone, acc.asScala.toList) tap { s =>
              acc.add(s)
            }
            (scenarioResult, ctxClone)
          }
        } finally {
          ctxClone.close()
        }
      }
    }
    Await.result(
      Future.sequence(futures.force),
      Duration.Inf
    ) sortBy { (s, _) => 
      s.sourceRef.map(_.line).getOrElse(0L)
    } foreach { (_, c) => 
      c.getVideos foreach ctx.addVideo
    } 
    acc.asScala.toList.sortBy(_.sourceRef.map(_.line).getOrElse(0L))
  }

  private def evaluateOrTransitionScenario(parent: GwenNode, scenario: Scenario, ctx: T, acc: List[Scenario]): Scenario = {
    if (ctx.specType.isFeature && !scenario.isStepDef) {
      if (StateLevel.scenario.equals(ctx.stateLevel)) {
        ctx.reset(StateLevel.scenario)
      }
    }
    EvalStatus(acc.map(_.evalStatus)) match {
      case status @ Failed(_, error) =>
        val isSoftAssert = ctx.evaluate(false) { status.isSoftAssertionError }
        val failfast = ctx.evaluate(false) { GwenSettings.`gwen.feature.failfast.enabled` }
        val exitOnFail = ctx.evaluate(false) { GwenSettings.`gwen.feature.failfast.exit` }
        if (failfast && !exitOnFail && !isSoftAssert) {
          transitionScenario(scenario, Skipped, ctx)
        } else if (exitOnFail && !isSoftAssert) {
          transitionScenario(scenario, scenario.evalStatus, ctx)
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
  private [engine] def evaluateScenario(parent: GwenNode, scenario: Scenario, ctx: T): Scenario = {
    if (scenario.isStepDef || scenario.isDataTable) {
      if (!scenario.isStepDef) Errors.dataTableError(s"${Annotations.StepDef} tag also expected where ${Annotations.DataTable} is specified")
      loadStepDef(parent, scenario, ctx)
    } else {
      ctx.scenarioScope.boundary(scenario.name, Nil) {
        beforeScenario(scenario, ctx)
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
          afterScenario(s, ctx)
        }
      }
    } tap { s =>
      logStatus(ctx.options, s)
    }
  }

  private def evaluateScenarioWithBackground(scenario: Scenario, background: Background, ctx: T): Scenario = {
    val bg = evaluateBackground(scenario, background, ctx)
    val steps: List[Step] = bg.evalStatus match {
      case _: Passed => evaluateSteps(scenario, scenario.steps, ctx)
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
        transitionStep(step, Loaded, ctx)
      }
    }
    val examples = evaluateExamples(outline, outline.examples, ctx)
    outline.copy(
      withSteps = steps,
      withExamples = examples
    )
  }

}
