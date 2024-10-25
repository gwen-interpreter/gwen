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

import gwen.core.Errors
import gwen.core.ImplicitValueKeys
import gwen.core.eval.EvalContext
import gwen.core.eval.EvalEngine
import gwen.core.node.GwenNode
import gwen.core.node.gherkin.Annotations
import gwen.core.node.gherkin.Scenario
import gwen.core.node.gherkin.SpecNormaliser
import gwen.core.node.gherkin.Step
import gwen.core.node.gherkin.table.DataTable
import gwen.core.status.Loaded
import gwen.core.status.Passed

import scala.util.chaining._
import scala.util.Try

import com.typesafe.scalalogging.LazyLogging

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.ConcurrentMap
import java.util.concurrent.Semaphore
import gwen.core.status.StatusKeyword

/**
  * StepDef evaluation engine.
  */
trait StepDefEngine[T <: EvalContext] extends SpecNormaliser with LazyLogging with ImplicitValueKeys {
  engine: EvalEngine[T] =>

  // Lock for managing synchronized StepDefs
  private val stepDefLock: ConcurrentMap[String, Semaphore] = new ConcurrentHashMap()

  /**
    * Loads a stepdef to memory.
    */
  private [engine] def loadStepDef(parent: GwenNode, stepDef: Scenario, ctx: T): Scenario = {
    beforeStepDef(stepDef, ctx)
    logger.info(s"Loading ${stepDef.keyword}: ${stepDef.name}")
    ctx.addStepDef(stepDef)
    if (stepDef.isSynchronized) {
      stepDefLock.putIfAbsent(stepDef.name, new Semaphore(1))
    }
    val loadedSteps = transitionSteps(stepDef.steps, Loaded, ctx)
    val steps = if (stepDef.isOutline) stepDef.steps else loadedSteps
    val examples = if (stepDef.isOutline) {
      stepDef.examples map { exs =>
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
      stepDef.examples
    }
    stepDef.copy(
      withBackground = None,
      withSteps = steps,
      withExamples = examples
    ) tap { s =>
      afterStepDef(s, ctx)
    }
  }

  def callStepDef(parent: GwenNode, iStepDef: Scenario, step: Step, ctx: T): Step = {
    var parallelAncestor = ctx.nodeChain.nodes.reverse.find(_.isInstanceOf[Scenario]).map(_.asInstanceOf[Scenario].isParallel).getOrElse(false)
    if (parallelAncestor && iStepDef.isParallel) {
      Errors.illegalNestedParallelExceutionError(step.sourceRef)
    }
    val stepDef = iStepDef.withCallerParams(step)
    val lock = if (stepDefLock.containsKey(stepDef.name)) {
      Some(stepDefLock.get(stepDef.name))
    } else None
    lock.foreach { l =>
      l.acquire()
      logger.info(s"Synchronized StepDef execution started [StepDef: ${stepDef.name}] [thread: ${Thread.currentThread().getName}]")
    }
    try {
      val sdStep = step.copy(
        withStepDef = Some(stepDef)
      )
      checkStepDefRules(sdStep, ctx)
      ctx.paramScope.push(stepDef.name, stepDef.params)
      try {
        val dataTableOpt = stepDef.tags.find(_.name.startsWith(Annotations.DataTable.toString)) map { tag => DataTable(tag, step) }
        val nonEmptyDataTableOpt = dataTableOpt.filter(_.records.nonEmpty)
        nonEmptyDataTableOpt foreach { table =>
          ctx.topScope.pushObject(DataTable.tableKey, table)
        }
        try {
          if (dataTableOpt.nonEmpty && nonEmptyDataTableOpt.isEmpty) {
            transitionStep(step, Passed(0, abstained = !ctx.options.dryRun), ctx)
          } else {
            evaluateStepDef(parent, stepDef, step, ctx)
          }
        } finally {
          nonEmptyDataTableOpt foreach { _ =>
            ctx.topScope.popObject(DataTable.tableKey)
          }
        }
      } finally {
        ctx.paramScope.pop()
      }
    } finally {
      lock.foreach { l =>
        l.release()
        logger.info(s"Synchronized StepDef execution finished [StepDef: ${stepDef.name}] [thread: ${Thread.currentThread().getName}]")
      }
    }
  }

  private def evaluateStepDef(parent: GwenNode, stepDef: Scenario, step: Step, ctx: T): Step = {
    beforeStepDef(stepDef, ctx)
    logger.debug(s"Evaluating ${stepDef.keyword}: ${stepDef.name}")
    val steps = if (!stepDef.isOutline) {
      evaluateSteps(stepDef, stepDef.steps, ctx)
    } else {
      stepDef.steps map { step =>
        if (stepDef.isExpanded) {
          step.copy(withEvalStatus = Loaded)
        } else {
          transitionStep(step, Loaded, ctx)
        }
      }
    }
    val examples = if (stepDef.isOutline) {
      val exs = if (!stepDef.isExpanded) {
        expandExamples(stepDef, None, ctx).examples
      } else {
        stepDef.examples
      }
      evaluateExamples(stepDef, exs, ctx)
    } else {
      stepDef.examples
    }
    val eStepDef = stepDef.copy(
      withBackground = None,
      withSteps = steps,
      withExamples = examples)
    logger.debug(s"${stepDef.keyword} evaluated: ${stepDef.name}")
    afterStepDef(eStepDef, ctx)
    step.copy(
      withStepDef = Some(eStepDef),
      withEvalStatus = eStepDef.evalStatus
    )
  }

}
