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

import gwen.core.Assert
import gwen.core.node.RecurringNode
import gwen.core.node.gherkin.GherkinNode
import gwen.core.node.gherkin.Step

class NodeChain(val nodes: List[GwenNode]) {

  Assert(nodes.nonEmpty, "IllegalState: nodes cannot be empty")

  def previous: GwenNode = nodes(nodes.size - 2)
  def last: GwenNode = nodes.last
  def take(n: Int): NodeChain = new NodeChain(nodes.take(n))
  def add(node: GwenNode):NodeChain = new NodeChain(nodes ++ List(node))
  def steps: List[Step] = nodes.filter(_.nodeType == NodeType.Step).map(_.asInstanceOf[Step])

  def nodePath: String = {
    nodes.foldLeft("") { (path: String, node: GwenNode) =>
      s"${if (path != "/") path else ""}/${nodeName(node)}${occurrenceNo(node)}"
    }
  }

  private def nodeName(node: GwenNode): String = {
    node match {
      case step: Step => step.expression
      case _ => node.name
    }
  }

  private def occurrenceNo(node: GwenNode): String = {
    (node match {
      case rNode: RecurringNode => rNode.occurrenceNo
      case _ => None
    }).filter(n => n > 1 || node.isInstanceOf[Step]).map(n => s"[$n]").mkString 
  }

}

object NodeChain {

  def apply(): NodeChain = new NodeChain(List(Root))

}

