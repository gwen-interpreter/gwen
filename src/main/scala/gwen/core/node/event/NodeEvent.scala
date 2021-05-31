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

import gwen.core.node.GwenNode
import gwen.core.node.gherkin.Step
import gwen.core.model.state.ScopedDataStack

import java.{util => ju}

case class NodeEvent[T <: GwenNode](phase: NodePhase.Value, parent: GwenNode, source: T, callTrail: List[Step], scopes: ScopedDataStack) {
  val time: ju.Date = ju.Calendar.getInstance.getTime
  override def toString: String = 
    s"${phase}${source.nodeType} $time ${this.getClass.getSimpleName}[${source.getClass.getSimpleName}]($source,${parent.uuid},${source.uuid})"
}
