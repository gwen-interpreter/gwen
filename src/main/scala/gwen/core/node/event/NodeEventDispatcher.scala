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
import gwen.core.state.ScopedDataStack
import gwen.core.status.EvalStatus

import scala.collection.mutable

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

  def beforeUnit(unit: FeatureUnit, scopes: ScopedDataStack): Unit =
    dispatchBeforeEvent(unit.parent, unit, scopes) { (listener, event) => listener.beforeUnit(event) }
  def afterUnit(unit: FeatureUnit, scopes: ScopedDataStack): Unit =
    dispatchAfterEvent(unit, scopes) { (listener, event) => listener.afterUnit(event) }
  def beforeSpec(parent: GwenNode, spec: Spec, scopes: ScopedDataStack): Unit =
    dispatchBeforeEvent(parent, spec, scopes) { (listener, event) => listener.beforeSpec(event) }
  def afterSpec(result: SpecResult, scopes: ScopedDataStack): Unit =
    dispatchAfterEvent(result, scopes) { (listener, event) => listener.afterSpec(event) }
  def beforeBackground(parent: GwenNode, background: Background, scopes: ScopedDataStack): Unit =
    dispatchBeforeEvent(parent, background, scopes) { (listener, event) => listener.beforeBackground(event) }
  def afterBackground(background: Background, scopes: ScopedDataStack): Unit =
    dispatchAfterEvent(background, scopes) { (listener, event) => listener.afterBackground(event) }
  def beforeScenario(parent: GwenNode, scenario: Scenario, scopes: ScopedDataStack): Unit =
    dispatchBeforeEvent(parent, scenario, scopes) { (listener, event) => listener.beforeScenario(event) }
  def afterScenario(scenario: Scenario, scopes: ScopedDataStack): Unit =
    dispatchAfterEvent(scenario, scopes) { (listener, event) => listener.afterScenario(event) }
  def beforeExamples(parent: GwenNode, examples: Examples, scopes: ScopedDataStack): Unit =
    dispatchBeforeEvent(parent, examples, scopes) { (listener, event) => listener.beforeExamples(event) }
  def afterExamples(examples: Examples, scopes: ScopedDataStack): Unit =
    dispatchAfterEvent(examples, scopes) { (listener, event) => listener.afterExamples(event) }
  def beforeRule(parent: GwenNode, rule: Rule, scopes: ScopedDataStack): Unit =
    dispatchBeforeEvent(parent, rule, scopes) { (listener, event) => listener.beforeRule(event) }
  def afterRule(rule: Rule, scopes: ScopedDataStack): Unit =
    dispatchAfterEvent(rule, scopes) { (listener, event) => listener.afterRule(event) }
  def beforeStepDef(parent: GwenNode, stepDef: Scenario, scopes: ScopedDataStack): Unit =
    dispatchBeforeEvent(parent, stepDef, scopes) { (listener, event) => listener.beforeStepDef(event) }
  def afterStepDef(stepDef: Scenario, scopes: ScopedDataStack): Unit = 
    dispatchAfterEvent(stepDef, scopes) { (listener, event) => listener.afterStepDef(event) }
  def beforeStep(parent: GwenNode, step: Step, scopes: ScopedDataStack): Unit =
    dispatchBeforeEvent(parent, step, scopes) { (listener, event) => listener.beforeStep(event) }
  def afterStep(step: Step, scopes: ScopedDataStack): Unit = {
    // to gurantee at least 1 millisecond delay for durations less than 1 msec
    if (step.evalStatus.duration.toMillis < 1) {
      Thread.sleep(1)
    }
    dispatchAfterEvent(step, scopes) { (listener, event) => listener.afterStep(event) }
  }
  def healthCheck(parent: GwenNode, step: Step, scopes: ScopedDataStack): Unit = { 
    dispatchHealthCheckEvent(parent, step, scopes) { (listener, event) => 
      Try(listener.healthCheck(event)) match {
        case Failure(e) => 
          Settings.setLocal("gwen.feature.failfast", "true")
          Settings.setLocal("gwen.feature.failfast.exit", "false")
          throw e
        case _ => // noop
      }
    }
  }

  def transitionBackground(parent: GwenNode, background: Background, toStatus: EvalStatus, scopes: ScopedDataStack): Background = {
    beforeBackground(parent, background, scopes)
    val steps = transitionSteps(background, background.steps, toStatus, scopes)
    background.copy(withSteps = steps) tap { b => afterBackground(b, scopes) }
  }

  def transitionScenario(parent: GwenNode, scenario: Scenario, toStatus: EvalStatus, scopes: ScopedDataStack): Scenario = {
    beforeScenario(parent, scenario, scopes)
    val background = scenario.background map { background => 
      transitionBackground(scenario, background, toStatus, scopes)
    }
    val steps = transitionSteps(scenario, scenario.steps, toStatus, scopes)
    val examples = scenario.examples map { exs =>
      transitionExamples(scenario, exs.copy(withScenarios = Nil), toStatus, scopes)
    }
    scenario.copy(
      withBackground = background,
      withSteps = steps,
      withExamples = examples
    ) tap { s => afterScenario(s, scopes) }
  }

  def transitionExamples(parent: GwenNode, examples: Examples, toStatus: EvalStatus, scopes: ScopedDataStack): Examples = {
    beforeExamples(parent, examples, scopes)
    examples.copy() tap { exs => afterExamples(exs, scopes) }
  }

  def transitionStep(parent: GwenNode, step: Step, toStatus: EvalStatus, scopes: ScopedDataStack): Step = {
    beforeStep(parent, step, scopes)
    step.copy(withEvalStatus = toStatus) tap { s => afterStep(s, scopes) }
  }

  def transitionSteps(parent: GwenNode, steps: List[Step], toStatus: EvalStatus, scopes: ScopedDataStack): List[Step] = {
    steps.map { step => 
      transitionStep(parent, step, toStatus, scopes) 
    }
  }

  def transitionRule(parent: GwenNode, rule: Rule, toStatus: EvalStatus, scopes: ScopedDataStack): Rule = {
    beforeRule(parent, rule, scopes)
    val background = rule.background.map { background => 
      transitionBackground(rule, background, toStatus, scopes)
    }
    val scenarios = rule.scenarios.map { scenario => 
      transitionScenario(rule, scenario, toStatus, scopes)
    }
    rule.copy(
      withBackground = background,
      withScenarios = scenarios
    ) tap { r => afterRule(r, scopes) }
  }

  private def dispatchBeforeEvent[T <: GwenNode]( 
      parent: GwenNode,
      source: T,
      scopes: ScopedDataStack)
      (dispatch: (NodeEventListener, NodeEvent[T]) => Unit): Unit = {
    listeners foreach { listener => 
      listener.pushParent(source)
      if (!listener.isPaused && listener.bypass.contains(source.nodeType)) {
        listener.pause(source)
      } else {
        dispatchEvent(listener, NodePhase.before, parent, source, scopes) { dispatch }
      }
    }
  }

  private def dispatchAfterEvent[T <: GwenNode](source: T, scopes: ScopedDataStack)
      (dispatch: (NodeEventListener, NodeEvent[T]) => Unit): Unit = {
    listeners foreach { listener => 
      val parent = listener.popParent()
      dispatchEvent(listener, NodePhase.after, parent, source, scopes) { dispatch } tap { _ =>
        if (listener.isPausedOn(parent)) { 
          listener.resume()
        }
      }
    }
  }

  private def dispatchHealthCheckEvent[T <: GwenNode](parent: GwenNode, source: T, scopes: ScopedDataStack)
      (dispatch: (NodeEventListener, NodeEvent[T]) => Unit): Unit = {
    listeners foreach { listener => 
      dispatchEvent(listener, NodePhase.healthCheck, parent, source, scopes) { dispatch }
    }
  }

  private def dispatchEvent[T <: GwenNode](
      listener: NodeEventListener,
      phase: NodePhase.Value, 
      parent: GwenNode,
      source: T,
      scopes: ScopedDataStack)
      (dispatch: (NodeEventListener, NodeEvent[T]) => Unit): Option[NodeEvent[T]] = {

    if (!listener.isPaused) {
      val event = NodeEvent(phase, parent, source, scopes)
      logger.debug(s"Dispatching event to ${listener.name}: $event")
      dispatch(listener, event)
      Some(event)
    } else {
      None
    }

  }

}
