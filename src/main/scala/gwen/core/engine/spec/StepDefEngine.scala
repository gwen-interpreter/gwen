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

import gwen.core.engine.EvalContext
import gwen.core.engine.EvalEngine
import gwen.core.engine.SpecNormaliser
import gwen.core.model._
import gwen.core.model.node.Scenario
import gwen.core.model.node.Step

import com.typesafe.scalalogging.LazyLogging

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.ConcurrentMap
import java.util.concurrent.Semaphore

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
  private [spec] def loadStepDef(parent: Identifiable, stepDef: Scenario, ctx: T): Scenario = {
    beforeStepDef(parent, stepDef, ctx.scopes)
    logger.info(s"Loading ${stepDef.keyword}: ${stepDef.name}")
    ctx.addStepDef(stepDef)
    if (ctx.options.parallel && stepDef.isSynchronized) {
      stepDefLock.putIfAbsent(stepDef.name, new Semaphore(1))
    }
    val loadedSteps = transitionSteps(stepDef, stepDef.steps, Loaded, ctx.scopes)
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
      afterStepDef(s, ctx.scopes)
    }
  }

  def callStepDef(parent: Identifiable, stepDef: Scenario, step: Step, ctx: T): Step = {
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
        val dataTableOpt = stepDef.tags.find(_.name.startsWith("DataTable(")) map { tag => DataTable(tag, step) }
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

  private def evaluateStepDef(parent: Identifiable, stepDef: Scenario, step: Step, ctx: T): Step = {
    beforeStepDef(parent, stepDef, ctx.scopes)
    logger.debug(s"Evaluating ${stepDef.keyword}: ${stepDef.name}")
    val steps = if (!stepDef.isOutline) {
      evaluateSteps(stepDef, stepDef.steps, ctx)
    } else {
      stepDef.steps map { step =>
        if (stepDef.isExpanded) {
          step.copy(withEvalStatus = Loaded)
        } else {
          transitionStep(stepDef, step, Loaded, ctx.scopes)
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
    afterStepDef(eStepDef, ctx.scopes) 
    step.copy(
      withStepDef = Some(eStepDef),
      withEvalStatus = eStepDef.evalStatus
    )
  }

}
