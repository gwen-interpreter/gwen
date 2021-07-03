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

package gwen.core.node.event

import gwen.core._
import gwen.core.node._
import gwen.core.node.gherkin._
import gwen.core.result.SpecResult
import gwen.core.state.Environment
import gwen.core.status.EvalStatus

import scala.collection.mutable
import scala.util.chaining._

import com.typesafe.scalalogging.LazyLogging

import scala.util.Try
import scala.util.Failure

class NodeEventDispatcher extends LazyLogging {

  private val listeners = new mutable.Queue[NodeEventListener]()

  def addListener(listener: NodeEventListener): Unit = {
    listeners += listener
    logger.debug(s"Node event listener registered: ${listener.name}")
  }

  def removeListener(listener: NodeEventListener): Unit = {
    listeners -= listener
    logger.debug(s"Node event listener removed: ${listener.name}")
  }

  def beforeUnit(unit: FeatureUnit, env: Environment): Unit =
    dispatchBeforeEvent(unit, env) { (listener, event) => listener.beforeUnit(event) }
  def afterUnit(unit: FeatureUnit, env: Environment): Unit =
    dispatchAfterEvent(unit, env) { (listener, event) => listener.afterUnit(event) }
  def beforeSpec(spec: Spec, env: Environment): Unit =
    dispatchBeforeEvent(spec, env) { (listener, event) => listener.beforeSpec(event) }
  def afterSpec(result: SpecResult, env: Environment): Unit =
    dispatchAfterEvent(result, env) { (listener, event) => listener.afterSpec(event) }
  def beforeBackground(background: Background, env: Environment): Unit =
    dispatchBeforeEvent(background, env) { (listener, event) => listener.beforeBackground(event) }
  def afterBackground(background: Background, env: Environment): Unit =
    dispatchAfterEvent(background, env) { (listener, event) => listener.afterBackground(event) }
  def beforeScenario(scenario: Scenario, env: Environment): Unit =
    dispatchBeforeEvent(scenario, env) { (listener, event) => listener.beforeScenario(event) }
  def afterScenario(scenario: Scenario, env: Environment): Unit =
    dispatchAfterEvent(scenario, env) { (listener, event) => listener.afterScenario(event) }
  def beforeExamples(examples: Examples, env: Environment): Unit =
    dispatchBeforeEvent(examples, env) { (listener, event) => listener.beforeExamples(event) }
  def afterExamples(examples: Examples, env: Environment): Unit =
    dispatchAfterEvent(examples, env) { (listener, event) => listener.afterExamples(event) }
  def beforeRule(rule: Rule, env: Environment): Unit =
    dispatchBeforeEvent(rule, env) { (listener, event) => listener.beforeRule(event) }
  def afterRule(rule: Rule, env: Environment): Unit =
    dispatchAfterEvent(rule, env) { (listener, event) => listener.afterRule(event) }
  def beforeStepDef(stepDef: Scenario, env: Environment): Unit =
    dispatchBeforeEvent(stepDef, env) { (listener, event) => listener.beforeStepDef(event) }
  def afterStepDef(stepDef: Scenario, env: Environment): Unit =
    dispatchAfterEvent(stepDef, env) { (listener, event) => listener.afterStepDef(event) }
  def beforeStep(step: Step, env: Environment): Unit =
    dispatchBeforeEvent(step, env) { (listener, event) => listener.beforeStep(event) }
  def afterStep(step: Step, env: Environment): Unit = {
    // to guarantee at least 1 millisecond delay for durations less than 1 msec
    if (step.evalStatus.duration.toMillis < 1) {
      Thread.sleep(1)
    }
    dispatchAfterEvent(step, env) { (listener, event) => listener.afterStep(event) }
  }
  def healthCheck(step: Step, env: Environment): Unit = {
    dispatchHealthCheckEvent(step, env) { (listener, event) =>
      Try(listener.healthCheck(event)) match {
        case Failure(e) =>
          Settings.setLocal("gwen.feature.failfast", "true")
          Settings.setLocal("gwen.feature.failfast.exit", "false")
          throw e
        case _ => // noop
      }
    }
  }

  def transitionBackground(background: Background, toStatus: EvalStatus, env: Environment): Background = {
    beforeBackground(background, env)
    val steps = transitionSteps(background.steps, toStatus, env)
    background.copy(withSteps = steps) tap { b => afterBackground(b, env) }
  }

  def transitionScenario(scenario: Scenario, toStatus: EvalStatus, env: Environment): Scenario = {
    beforeScenario(scenario, env)
    val background = scenario.background map { background =>
      transitionBackground(background, toStatus, env)
    }
    val steps = transitionSteps(scenario.steps, toStatus, env)
    val examples = scenario.examples map { exs =>
      transitionExamples(exs.copy(withScenarios = Nil), toStatus, env)
    }
    scenario.copy(
      withBackground = background,
      withSteps = steps,
      withExamples = examples
    ) tap { s => afterScenario(s, env) }
  }

  def transitionExamples(examples: Examples, toStatus: EvalStatus, env: Environment): Examples = {
    beforeExamples(examples, env)
    examples.copy() tap { exs => afterExamples(exs, env) }
  }

  def transitionStep(step: Step, toStatus: EvalStatus, env: Environment): Step = {
    beforeStep(step, env)
    step.copy(withEvalStatus = toStatus) tap { s => afterStep(s, env) }
  }

  def transitionSteps(steps: List[Step], toStatus: EvalStatus, env: Environment): List[Step] = {
    steps.map { step =>
      transitionStep(step, toStatus, env)
    }
  }

  def transitionRule(rule: Rule, toStatus: EvalStatus, env: Environment): Rule = {
    beforeRule(rule, env)
    val background = rule.background.map { background =>
      transitionBackground(background, toStatus, env)
    }
    val scenarios = rule.scenarios.map { scenario =>
      transitionScenario(scenario, toStatus, env)
    }
    rule.copy(
      withBackground = background,
      withScenarios = scenarios
    ) tap { r => afterRule(r, env) }
  }

  private def dispatchBeforeEvent[T <: GwenNode](
      source: T,
      env: Environment)
      (dispatch: (NodeEventListener, NodeEvent[T]) => Unit): Unit = {
    val callChain = env.pushNode(source)
    listeners foreach { listener =>
      if (!listener.isPaused && listener.bypass.contains(source.nodeType)) {
        listener.pause(source)
      } else {
        dispatchEvent(listener, NodePhase.before, callChain, source, env) { dispatch }
      }
    }
  }

  private def dispatchAfterEvent[T <: GwenNode](source: T, env: Environment)
      (dispatch: (NodeEventListener, NodeEvent[T]) => Unit): Unit = {
    val callChain = env.nodeChain
    val node = callChain.last
    listeners foreach { listener =>
      dispatchEvent(listener, NodePhase.after, callChain, source, env) { dispatch } tap { _ =>
        if (listener.isPausedOn(node)) {
          listener.resume()
        }
      }
    }
    env.popNode()
  }

  private def dispatchHealthCheckEvent[T <: GwenNode](source: T, env: Environment)
      (dispatch: (NodeEventListener, NodeEvent[T]) => Unit): Unit = {
    listeners foreach { listener =>
      dispatchEvent(listener, NodePhase.healthCheck, env.nodeChain, source, env) { dispatch }
    }
  }

  private def dispatchEvent[T <: GwenNode](
      listener: NodeEventListener,
      phase: NodePhase,
      callChain: NodeChain,
      source: T,
      env: Environment)
      (dispatch: (NodeEventListener, NodeEvent[T]) => Unit): Option[NodeEvent[T]] = {

    if (!listener.isPaused) {
      val event = NodeEvent(phase, callChain, source, env.scopes)
      logger.debug(s"Dispatching event to ${listener.name}: $event")
      dispatch(listener, event)
      Some(event)
    } else {
      None
    }

  }

}
