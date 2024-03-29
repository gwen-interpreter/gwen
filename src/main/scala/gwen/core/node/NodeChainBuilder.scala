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
package gwen.core.node

import gwen.core.node.event.NodeEventListener

import scala.collection.mutable
import scala.util.chaining._

class NodeChainBuilder extends NodeEventListener("Call chain builder") {

  private val nodes = mutable.Queue[GwenNode](Root)

  def push(node: GwenNode): NodeChain = {
    nodes += node
    new NodeChain(nodes.toList)
  }

  def pop(): (GwenNode, NodeChain) = {
    (nodes.removeLast(false), new NodeChain(nodes.toList))
  }

  def nodeChain: NodeChain = new NodeChain(nodes.toList)

}

object NodeChainBuilder {

  def apply(chain: NodeChain): NodeChainBuilder = {
    new NodeChainBuilder() tap { builder =>
      chain.nodes.foreach(builder.push)
    }
  }

}
