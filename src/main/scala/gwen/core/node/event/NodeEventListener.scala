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

import gwen.core.node._
import gwen.core.node.gherkin._
import gwen.core.result.SpecResult

import scala.collection.mutable

/**
  * Node event listener.
  *
  * @param name arbitrary listener name
  * @param bypass set of node types to bypass (events on the specified node types includes their child nodes will not be dispatced to the listener)
  */
class NodeEventListener(val name: String, val bypass: Set[NodeType.Value] = Set[NodeType.Value]()) {
  
  private val paused = ThreadLocal.withInitial[Option[String]] { () => None }
  private val parents = ThreadLocal.withInitial[mutable.Queue[GwenNode]] { () => mutable.Queue[GwenNode]() }

  private [event] def isPaused: Boolean = paused.get.nonEmpty
  private [event] def isPausedOn(parent: GwenNode): Boolean = paused.get.contains(parent.uuid)
  private [event] def pause(parent: GwenNode): Unit = { paused.set(Some(parent.uuid)) }
  private [event] def resume(): Unit = { paused.set(None) }
  private [event] def pushParent(parent: GwenNode): Unit = { parents.get += parent }
  private [event] def popParent(): GwenNode = parents.get.removeLast()
  
  def beforeUnit(event: NodeEvent[FeatureUnit]): Unit = { }
  def afterUnit(event: NodeEvent[FeatureUnit]): Unit = { }
  def beforeSpec(event: NodeEvent[Spec]): Unit = { }
  def afterSpec(event: NodeEvent[SpecResult]): Unit = { }
  def beforeBackground(event: NodeEvent[Background]): Unit = { }
  def afterBackground(event: NodeEvent[Background]): Unit = { }
  def beforeScenario(event: NodeEvent[Scenario]): Unit = { }
  def afterScenario(event: NodeEvent[Scenario]): Unit = { }
  def beforeExamples(event: NodeEvent[Examples]): Unit = { }
  def afterExamples(event: NodeEvent[Examples]): Unit = { }
  def beforeRule(event: NodeEvent[Rule]): Unit = { }
  def afterRule(event: NodeEvent[Rule]): Unit = { }
  def beforeStepDef(event: NodeEvent[Scenario]): Unit = { }
  def afterStepDef(event: NodeEvent[Scenario]): Unit = { }
  def beforeStep(event: NodeEvent[Step]): Unit = { }
  def afterStep(event: NodeEvent[Step]): Unit = { }
  def healthCheck(event: NodeEvent[Step]): Unit = { }

}
