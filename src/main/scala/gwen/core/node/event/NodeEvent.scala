/*
 * Copyright 2021-2024 Branko Juric, Brady Wood
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
import gwen.core.node.NodeChain
import gwen.core.state.Environment

import java.{util => ju}
import gwen.core.state.Environment

case class NodeEvent[T <: GwenNode](phase: NodePhase, callChain: NodeChain, source: T, env: Environment) {
  val time: ju.Date = ju.Calendar.getInstance.getTime
  override def toString: String = 
    s"${phase}${source.nodeType} $time ${this.getClass.getSimpleName}[${source.getClass.getSimpleName}]($source,${callChain.previous.uuid},${source.uuid})"
}
