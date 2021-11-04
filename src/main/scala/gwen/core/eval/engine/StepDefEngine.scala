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

package gwen.core.eval.engine

import gwen.core.eval.EvalContext
import gwen.core.eval.EvalEngine
import gwen.core.node.GwenNode
import gwen.core.node.gherkin.Scenario
import gwen.core.node.gherkin.SpecNormaliser
import gwen.core.node.gherkin.Step
import gwen.core.node.gherkin.table.DataTable
import gwen.core.status.Loaded

import scala.util.chaining._

import com.typesafe.scalalogging.LazyLogging

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.ConcurrentMap
import java.util.concurrent.Semaphore
import gwen.core.node.gherkin.ReservedTags

/**
  * StepDef evaluation engine.
  */
trait StepDefEngine[T <: EvalContext] extends SpecNormaliser with LazyLogging {
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
    if (ctx.options.parallel && stepDef.isSynchronized) {
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
        val dataTableOpt = stepDef.tags.find(_.name.startsWith(ReservedTags.DataTable.toString)) map { tag => DataTable(tag, step) }
        dataTableOpt foreach { table =>
          ctx.topScope.pushObject(DataTable.tableKey, table)
        }
        try {
          evaluateStepDef(parent, stepDef, step, ctx)
        } finally {
          dataTableOpt foreach { _ =>
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
      evaluateExamples(stepDef, stepDef.examples, ctx)
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
