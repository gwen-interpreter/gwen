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

package gwen.core.model.event

import gwen.core.model._
import gwen.core.model.gherkin._

import scala.collection.mutable

/**
  * Lifecycle event listener.
  *
  * @param name arbitrary listener name
  * @param bypass set of node types to bypass (events on the specified node types includes their child nodes will not be dispatced to the listener)
  */
class LifecycleEventListener(val name: String, val bypass: Set[NodeType.Value] = Set[NodeType.Value]()) {
  
  private val paused = ThreadLocal.withInitial[Option[String]] { () => None }
  private val parentUuids = ThreadLocal.withInitial[mutable.Queue[String]] { () => mutable.Queue[String]() }

  private [event] def isPaused: Boolean = paused.get.nonEmpty
  private [event] def isPausedOn(uuid: String): Boolean = paused.get.contains(uuid)
  private [event] def pause(uuid: String): Unit = { paused.set(Some(uuid)) }
  private [event] def resume(): Unit = { paused.set(None) }
  private [event] def pushUuid(uuid: String): Unit = { parentUuids.get += uuid }
  private [event] def popUuid(): String = parentUuids.get.removeLast()
  
  def beforeUnit(event: LifecycleEvent[FeatureUnit]): Unit = { }
  def afterUnit(event: LifecycleEvent[FeatureUnit]): Unit = { }
  def beforeSpec(event: LifecycleEvent[Spec]): Unit = { }
  def afterSpec(event: LifecycleEvent[SpecResult]): Unit = { }
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
  def healthCheck(event: LifecycleEvent[Step]): Unit = { }

}
