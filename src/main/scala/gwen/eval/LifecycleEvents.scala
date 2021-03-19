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

import java.{util => ju}

object LifecyclePhase extends Enumeration {
  type LifecyclePhase = Value
  val before, after = Value
}

case class LifecycleEvent[T <: Identifiable](phase: LifecyclePhase.Value, parentUuid: String, source: T, callTrail: List[Step], scopes: ScopedDataStack) {
  val time: ju.Date = ju.Calendar.getInstance.getTime
  override def toString: String = 
    s"${phase}${source.nodeType} $time ${this.getClass.getSimpleName}[${source.getClass.getSimpleName}]($source,$parentUuid,${source.uuid})"
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
  private val callTrail = ThreadLocal.withInitial[mutable.Queue[Step]] { () => mutable.Queue[Step]() }
  private val isVirtual = ThreadLocal.withInitial[mutable.Queue[Boolean]] { () => mutable.Queue[Boolean]() }
  private val virtualParents = ThreadLocal.withInitial[mutable.Queue[Identifiable]] { () => mutable.Queue[Identifiable]() }

  private def pushCallTrail(step: Step): List[Step] = { 
    callTrail.get += step 
    callTrail.get.toList
  }

  private def popCallTrail(): Option[Step] = { 
    callTrail.get.removeLastOption(false)
  }

  def addListener(listener: LifecycleEventListener): Unit = { 
    listeners += listener
    logger.debug(s"Lifecycle event listener registered: ${listener.name}")
  }

  def removeListener(listener: LifecycleEventListener): Unit = { 
    listeners -= listener
    logger.debug(s"Lifecycle event listener removed: ${listener.name}")
  }

  def beforeUnit(unit: FeatureUnit, scopes: ScopedDataStack): Unit =
    dispatchBeforeEvent(unit.parent, unit, scopes) { (listener, event) => listener.beforeUnit(event) }
  def afterUnit(unit: FeatureUnit, scopes: ScopedDataStack): Unit =
    dispatchAfterEvent(unit, scopes) { (listener, event) => listener.afterUnit(event) }
  def beforeFeature(parent: Identifiable, featureSpec: FeatureSpec, scopes: ScopedDataStack): Unit =
    dispatchBeforeEvent(parent, featureSpec, scopes) { (listener, event) => listener.beforeFeature(event) }
  def afterFeature(featureResult: FeatureResult, scopes: ScopedDataStack): Unit =
    dispatchAfterEvent(featureResult,scopes) { (listener, event) => listener.afterFeature(event) }
  def beforeBackground(parent: Identifiable, background: Background, scopes: ScopedDataStack): Unit =
    dispatchBeforeEvent(parent, background, scopes) { (listener, event) => listener.beforeBackground(event) }
  def afterBackground(background: Background, scopes: ScopedDataStack): Unit =
    dispatchAfterEvent(background, scopes) { (listener, event) => listener.afterBackground(event) }
  def beforeScenario(parent: Identifiable, scenario: Scenario, scopes: ScopedDataStack): Unit =
    dispatchBeforeEvent(parent, scenario, scopes) { (listener, event) => listener.beforeScenario(event) }
  def afterScenario(scenario: Scenario, scopes: ScopedDataStack): Unit =
    dispatchAfterEvent(scenario, scopes) { (listener, event) => listener.afterScenario(event) }
  def beforeExamples(parent: Identifiable, examples: Examples, scopes: ScopedDataStack): Unit =
    dispatchBeforeEvent(parent, examples, scopes) { (listener, event) => listener.beforeExamples(event) }
  def afterExamples(examples: Examples, scopes: ScopedDataStack): Unit =
    dispatchAfterEvent(examples, scopes) { (listener, event) => listener.afterExamples(event) }
  def beforeRule(parent: Identifiable, rule: Rule, scopes: ScopedDataStack): Unit =
    dispatchBeforeEvent(parent, rule, scopes) { (listener, event) => listener.beforeRule(event) }
  def afterRule(rule: Rule, scopes: ScopedDataStack): Unit =
    dispatchAfterEvent(rule, scopes) { (listener, event) => listener.afterRule(event) }
  def beforeStepDef(parent: Identifiable, stepDef: Scenario, scopes: ScopedDataStack): Unit = {
    if (!isVirtual.get.lastOption.getOrElse(false) && stepDef.isVirtual) {
      virtualParents.get += callTrail.get.last
    }
    if (!stepDef.isVirtual) {
      val parentNode = {
        if(isVirtual.get.lastOption.getOrElse(false)) {
          virtualParents.get.lastOption.getOrElse(parent)
        } else parent
      }
      dispatchBeforeEvent(parentNode, stepDef, scopes) { (listener, event) => listener.beforeStepDef(event) }
    }
    isVirtual.get += stepDef.isVirtual
  }
  def afterStepDef(stepDef: Scenario, scopes: ScopedDataStack): Unit = {
    if (!stepDef.isVirtual) {
      dispatchAfterEvent(stepDef, scopes) { (listener, event) => listener.afterStepDef(event) }
    }
    else if (isVirtual.get.nonEmpty) {
      virtualParents.get.removeLast()
    }
    isVirtual.get.removeLast()
  }
  def beforeStep(parent: Identifiable, step: Step, scopes: ScopedDataStack): Unit = {
    if (!isVirtual.get.lastOption.getOrElse(false)) {
      pushCallTrail(step)
      dispatchBeforeEvent(parent, step, scopes) { (listener, event) => listener.beforeStep(event) }
    }
  }
  def afterStep(step: Step, scopes: ScopedDataStack): Unit = {
    def virtualOverride = step.evalStatus match {
      case status @ Failed(_, error)  =>
        val isAssertionError = status.isAssertionError
        val isSoftAssert = isAssertionError && AssertionMode.isSoft
        val failfast = GwenSettings.`gwen.feature.failfast`
        isSoftAssert || !failfast
      case _ => true
    }
    if (!isVirtual.get.lastOption.getOrElse(false) && (!step.isVirtual || virtualOverride)) {
      dispatchAfterEvent(step, scopes) { (listener, event) => listener.afterStep(event) }
      popCallTrail()
    }
  }
  def transitionBackground(parent: Identifiable, background: Background, toStatus: EvalStatus, scopes: ScopedDataStack): Background = {
    beforeBackground(parent, background, scopes)
    val steps = transitionSteps(background, background.steps, toStatus, scopes)
    background.copy(withSteps = steps) tap { b => afterBackground(b, scopes) }
  }

  def transitionScenario(parent: Identifiable, scenario: Scenario, toStatus: EvalStatus, scopes: ScopedDataStack): Scenario = {
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

  def transitionExamples(parent: Identifiable, examples: Examples, toStatus: EvalStatus, scopes: ScopedDataStack): Examples = {
    beforeExamples(parent, examples, scopes)
    examples.copy() tap { exs => afterExamples(exs, scopes) }
  }

  def transitionStep(parent: Identifiable, step: Step, toStatus: EvalStatus, scopes: ScopedDataStack): Step = {
    beforeStep(parent, step, scopes)
    step.copy(withEvalStatus = toStatus) tap { s => afterStep(s, scopes) }
  }

  def transitionSteps(parent: Identifiable, steps: List[Step], toStatus: EvalStatus, scopes: ScopedDataStack): List[Step] = {
    steps.map { step => 
      transitionStep(parent, step, toStatus, scopes) 
    }
  }

  def transitionRule(parent: Identifiable, rule: Rule, toStatus: EvalStatus, scopes: ScopedDataStack): Rule = {
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

  private def dispatchBeforeEvent[T <: Identifiable]( 
      parent: Identifiable,
      source: T,
      scopes: ScopedDataStack)
      (dispatch: (LifecycleEventListener, LifecycleEvent[T]) => Unit): Unit = {

    listeners foreach { listener => 
      listener.pushUuid(source.uuid)
      if (!listener.isPaused && listener.bypass.contains(source.nodeType)) {
        listener.pause(source.uuid)
      } else {
        dispatchEvent(listener, LifecyclePhase.before, parent.uuid, source, scopes) { dispatch }
      }
    }

  }

  private def dispatchAfterEvent[T <: Identifiable](source: T, scopes: ScopedDataStack)
      (dispatch: (LifecycleEventListener, LifecycleEvent[T]) => Unit): Unit = {

    listeners foreach { listener => 
      val parentUuid = listener.popUuid()
      dispatchEvent(listener, LifecyclePhase.after, parentUuid, source, scopes) { dispatch } tap { _ =>
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
      source: T,
      scopes: ScopedDataStack)
      (dispatch: (LifecycleEventListener, LifecycleEvent[T]) => Unit): Option[LifecycleEvent[T]] = {

    if (!listener.isPaused) {
      val event = LifecycleEvent(phase, parentUuid, source, callTrail.get.toList, scopes)
      logger.debug(s"Dispatching event to ${listener.name}: $event")
      dispatch(listener, event)
      Some(event)
    } else {
      None
    }

  }

}
