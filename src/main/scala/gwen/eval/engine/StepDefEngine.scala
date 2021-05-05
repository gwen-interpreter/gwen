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

import gwen.eval.EvalContext
import gwen.eval.EvalEngine
import gwen.eval.SpecNormaliser
import gwen.model._
import gwen.model.gherkin.Scenario
import gwen.model.gherkin.Step

import com.typesafe.scalalogging.LazyLogging

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.ConcurrentMap
import java.util.concurrent.Semaphore
import gwen.eval.EvalEnvironment

/**
  * StepDef evaluation engine.
  */
trait StepDefEngine[T <: EvalContext] extends SpecNormaliser with LazyLogging {
    engine: EvalEngine[T] =>

  // semaphores for managing synchronized StepDefs
  val stepDefSemaphors: ConcurrentMap[String, Semaphore] = new ConcurrentHashMap()

  /**
    * Loads a stepdef to memory.
    */
  def loadStepDef(parent: Identifiable, stepDef: Scenario, ctx: T): Scenario = {
    ctx.withEnv { env =>
      ctx.lifecycle.beforeStepDef(parent, stepDef, env.scopes)
      logger.info(s"Loading ${stepDef.keyword}: ${stepDef.name}")
      env.addStepDef(stepDef)
      if (ctx.options.parallel && stepDef.isSynchronized) {
        stepDefSemaphors.putIfAbsent(stepDef.name, new Semaphore(1))
      }
      val loadedSteps = ctx.lifecycle.transitionSteps(stepDef, stepDef.steps, Loaded, env.scopes)
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
        ctx.lifecycle.afterStepDef(s, env.scopes)
      }
    }
  }

  def evaluateStepDef(parent: Identifiable, stepDef: Scenario, step: Step, params: List[(String, String)], ctx: T): Step = {
    ctx.withEnv { env =>
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
    }
  }

  private def evaluateStepDef(parent: Identifiable, stepDef: Scenario, step: Step, params: List[(String, String)], env: EvalEnvironment, ctx: T): Step = {
    ctx.lifecycle.beforeStepDef(parent, stepDef, env.scopes)
    val steps = if (!stepDef.isOutline) {
      evaluateSteps(stepDef, stepDef.steps, ctx)
    } else {
      stepDef.steps map { step =>
        if (stepDef.isExpanded) {
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
  }

}
