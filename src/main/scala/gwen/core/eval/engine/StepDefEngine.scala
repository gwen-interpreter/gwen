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
import gwen.core.eval.EvalEnvironment
import gwen.core.eval.SpecNormaliser
import gwen.core.model._
import gwen.core.model.gherkin.Scenario
import gwen.core.model.gherkin.Step

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
  private [engine] def loadStepDef(parent: Identifiable, stepDef: Scenario, ctx: T): Scenario = {
    ctx.withEnv { env =>
      lifecycle.beforeStepDef(parent, stepDef, env.scopes)
      logger.info(s"Loading ${stepDef.keyword}: ${stepDef.name}")
      env.addStepDef(stepDef)
      if (ctx.options.parallel && stepDef.isSynchronized) {
        stepDefLock.putIfAbsent(stepDef.name, new Semaphore(1))
      }
      val loadedSteps = lifecycle.transitionSteps(stepDef, stepDef.steps, Loaded, env.scopes)
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
        lifecycle.afterStepDef(s, env.scopes)
      }
    }
  }


  private [engine] def evaluateStepDef(parent: Identifiable, stepDef: Scenario, step: Step, params: List[(String, String)], ctx: T): Step = {
    val lock = if (stepDefLock.containsKey(stepDef.name)) {
      Some(stepDefLock.get(stepDef.name))
    } else None
    lock.foreach { l => 
      l.acquire()
      logger.info(s"Synchronized StepDef execution started [StepDef: ${stepDef.name}] [thread: ${Thread.currentThread().getName}]")
    }
    try {
      ctx.withEnv { env =>
        val sdStep = step.copy(
          withStepDef = Some((stepDef, params)),
          withAttachments = stepDef.steps.flatMap(_.attachments)
        )
        checkStepDefRules(sdStep, env)
        env.stepScope.push(stepDef.name, params)
        try {
          val dataTableOpt = stepDef.tags.find(_.name.startsWith("DataTable(")) map { tag => DataTable(tag, step) }
          dataTableOpt foreach { table =>
            env.topScope.pushObject(DataTable.tableKey, table)
          }
          try {
            evaluateStepDef(parent, stepDef, step, params, env, ctx)
          } finally {
            dataTableOpt foreach { _ =>
              env.topScope.popObject(DataTable.tableKey)
            }
          }
        } finally {
          env.stepScope.pop
        }
      }
    } finally {
      lock.foreach { l => 
        l.release()
        logger.info(s"Synchronized StepDef execution finished [StepDef: ${stepDef.name}] [thread: ${Thread.currentThread().getName}]")
      }
    }
  }

  private def evaluateStepDef(parent: Identifiable, stepDef: Scenario, step: Step, params: List[(String, String)], env: EvalEnvironment, ctx: T): Step = {
    lifecycle.beforeStepDef(parent, stepDef, env.scopes)
    logger.debug(s"Evaluating ${stepDef.keyword}: ${stepDef.name}")
    val steps = if (!stepDef.isOutline) {
      evaluateSteps(stepDef, stepDef.steps, ctx)
    } else {
      stepDef.steps map { step =>
        if (stepDef.isExpanded) {
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
  }

}
