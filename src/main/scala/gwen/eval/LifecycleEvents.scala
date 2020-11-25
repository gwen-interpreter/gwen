/*
 * Copyright 2020 Branko Juric, Brady Wood
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

package gwen.eval

import gwen._
import gwen.dsl._

import scala.collection.mutable

import com.typesafe.scalalogging.LazyLogging

object LifecyclePhase extends Enumeration {
  type LifecyclePhase = Value
  val before, after = Value
}

case class LifecycleEvent[T <: Identifiable](phase: LifecyclePhase.Value, parentUuid: String, source: T) {
  override def toString: String = 
    s"${phase}${source.nodeType} ${this.getClass.getSimpleName}[${source.getClass.getSimpleName}]($source,$parentUuid,${source.uuid})"
}

/**
  * Lifecycle event listener.
  *
  * @param name arbitrary listener name
  * @param bypass set of node types to bypass (events on the specified node types includes their child nodes will not be dispatced to the listener)
  */
class LifecycleEventListener(val name: String, val bypass: Set[NodeType.Value] = Set[NodeType.Value]()) {
  
  private val paused = ThreadLocal.withInitial[Option[String]] { () => None }
  private val parentUuids = ThreadLocal.withInitial[mutable.Queue[String]] { () => mutable.Queue[String]() }

  private [eval] def isPaused: Boolean = paused.get.nonEmpty
  private [eval] def isPausedOn(uuid: String): Boolean = paused.get.contains(uuid)
  private [eval] def pause(uuid: String): Unit = { paused.set(Some(uuid)) }
  private [eval] def resume(): Unit = { paused.set(None) }
  private [eval] def pushUuid(uuid: String): Unit = { parentUuids.get += uuid }
  private [eval] def popUuid(): String = parentUuids.get.removeLast()
  
  def beforeUnit(event: LifecycleEvent[FeatureUnit]): Unit = { }
  def afterUnit(event: LifecycleEvent[FeatureUnit]): Unit = { }
  def beforeFeature(event: LifecycleEvent[FeatureSpec]): Unit = { }
  def afterFeature(event: LifecycleEvent[FeatureResult]): Unit = { }
  def beforeBackground(event: LifecycleEvent[Background]): Unit = { }
  def afterBackground(event: LifecycleEvent[Background]): Unit = { }
  def beforeScenario(event: LifecycleEvent[Scenario]): Unit = { }
  def afterScenario(event: LifecycleEvent[Scenario]): Unit = { }
  def beforeExamples(event: LifecycleEvent[Examples]): Unit = { }
  def afterExamples(event: LifecycleEvent[Examples]): Unit = { }
  def beforeRule(event: LifecycleEvent[Rule]): Unit = { }
  def afterRule(event: LifecycleEvent[Rule]): Unit = { }
  def beforeStepDef(event: LifecycleEvent[Scenario]): Unit = { }
  def afterStepDef(event: LifecycleEvent[Scenario]): Unit = { }
  def beforeStep(event: LifecycleEvent[Step]): Unit = { }
  def afterStep(event: LifecycleEvent[Step]): Unit = { }

}

class LifecycleEventDispatcher extends LazyLogging {

  private val listeners = new mutable.Queue[LifecycleEventListener]()

  def addListener(listener: LifecycleEventListener): Unit = { 
    listeners += listener
    logger.info(s"Lifecycle event listener registered: ${listener.name}")
  }

  def beforeUnit(unit: FeatureUnit): Unit =
    dispatchBeforeEvent(Root, unit) { (listener, event) => listener.beforeUnit(event) }
  def afterUnit(unit: FeatureUnit): Unit =
    dispatchAfterEvent(unit) { (listener, event) => listener.afterUnit(event) }
  def beforeFeature(parent: Identifiable, featureSpec: FeatureSpec): Unit =
    dispatchBeforeEvent(parent, featureSpec) { (listener, event) => listener.beforeFeature(event) }
  def afterFeature(featureResult: FeatureResult): Unit =
    dispatchAfterEvent(featureResult) { (listener, event) => listener.afterFeature(event) }
  def beforeBackground(parent: Identifiable, background: Background): Unit =
    dispatchBeforeEvent(parent, background) { (listener, event) => listener.beforeBackground(event) }
  def afterBackground(background: Background): Unit =
    dispatchAfterEvent(background) { (listener, event) => listener.afterBackground(event) }
  def beforeScenario(parent: Identifiable, scenario: Scenario): Unit =
    dispatchBeforeEvent(parent, scenario) { (listener, event) => listener.beforeScenario(event) }
  def afterScenario(scenario: Scenario): Unit =
    dispatchAfterEvent(scenario) { (listener, event) => listener.afterScenario(event) }
  def beforeExamples(parent: Identifiable, examples: Examples): Unit =
    dispatchBeforeEvent(parent, examples) { (listener, event) => listener.beforeExamples(event) }
  def afterExamples(examples: Examples): Unit =
    dispatchAfterEvent(examples) { (listener, event) => listener.afterExamples(event) }
  def beforeRule(parent: Identifiable, rule: Rule): Unit =
    dispatchBeforeEvent(parent, rule) { (listener, event) => listener.beforeRule(event) }
  def afterRule(rule: Rule): Unit =
    dispatchAfterEvent(rule) { (listener, event) => listener.afterRule(event) }
  def beforeStepDef(parent: Identifiable, stepDef: Scenario): Unit =
    dispatchBeforeEvent(parent, stepDef) { (listener, event) => listener.beforeStepDef(event) }
  def afterStepDef(stepDef: Scenario): Unit =
    dispatchAfterEvent(stepDef) { (listener, event) => listener.afterStepDef(event) }
  def beforeStep(parent: Identifiable, step: Step): Unit =
    dispatchBeforeEvent(parent, step) { (listener, event) => listener.beforeStep(event) }
  def afterStep(step: Step): Unit =
    dispatchAfterEvent(step) { (listener, event) => listener.afterStep(event) }

  def transitionBackground(parent: Identifiable, background: Background, toStatus: EvalStatus): Background = {
    beforeBackground(parent, background)
    val steps = transitionSteps(background, background.steps, toStatus)
    background.copy(withSteps = steps) tap { b => afterBackground(b) }
  }

  def transitionScenario(parent: Identifiable, scenario: Scenario, toStatus: EvalStatus): Scenario = {
    beforeScenario(parent, scenario)
    val background = scenario.background map { background => 
      transitionBackground(scenario, background, toStatus)
    }
    val steps = transitionSteps(scenario, scenario.steps, toStatus)
    val examples = scenario.examples map { exs =>
      transitionExamples(scenario, exs.copy(withScenarios = Nil), toStatus)
    }
    scenario.copy(
      withBackground = background,
      withSteps = steps,
      withExamples = examples
    ) tap { s => afterScenario(s) }
  }

  def transitionExamples(parent: Identifiable, examples: Examples, toStatus: EvalStatus): Examples = {
    beforeExamples(parent, examples)
    examples.copy() tap { exs => afterExamples(exs) }
  }

  def transitionStep(parent: Identifiable, step: Step, toStatus: EvalStatus): Step = {
    beforeStep(parent, step)
    step.copy(withEvalStatus = toStatus) tap { s => afterStep(s) }
  }

  def transitionSteps(parent: Identifiable, steps: List[Step], toStatus: EvalStatus): List[Step] = {
    steps.map { step => 
      transitionStep(parent, step, toStatus) 
    }
  }

  def transitionRule(parent: Identifiable, rule: Rule, toStatus: EvalStatus): Rule = {
    beforeRule(parent, rule)
    val background = rule.background.map { background => 
      transitionBackground(rule, background, toStatus)
    }
    val scenarios = rule.scenarios.map { scenario => 
      transitionScenario(rule, scenario, toStatus)
    }
    rule.copy(
      withBackground = background,
      withScenarios = scenarios
    ) tap { r => afterRule(r) }
  }

  private def dispatchBeforeEvent[T <: Identifiable]( 
      parent: Identifiable,
      source: T)
      (dispatch: (LifecycleEventListener, LifecycleEvent[T]) => Unit): Unit = {

    listeners foreach { listener => 
      listener.pushUuid(source.uuid)
      if (!listener.isPaused && listener.bypass.contains(source.nodeType)) {
        listener.pause(source.uuid)
      } else {
        dispatchEvent(listener, LifecyclePhase.before, parent.uuid, source) { dispatch }
      }
    }

  }

  private def dispatchAfterEvent[T <: Identifiable](source: T)
      (dispatch: (LifecycleEventListener, LifecycleEvent[T]) => Unit): Unit = {

    listeners foreach { listener => 
      val parentUuid = listener.popUuid()
      dispatchEvent(listener, LifecyclePhase.after, parentUuid, source) { dispatch } tap { _ =>
        if (listener.isPausedOn(parentUuid)) { 
          listener.resume()
        }
      }
    }

  }

  private def dispatchEvent[T <: Identifiable](
      listener: LifecycleEventListener,
      phase: LifecyclePhase.Value, 
      parentUuid: String,
      source: T)
      (dispatch: (LifecycleEventListener, LifecycleEvent[T]) => Unit): Option[LifecycleEvent[T]] = {

    if (!listener.isPaused) {
      val event = LifecycleEvent(phase, parentUuid, source)
      logger.debug(s"Dispatching event to ${listener.name}: $event")
      dispatch(listener, event)
      Some(event)
    } else {
      None
    }

  }

}
