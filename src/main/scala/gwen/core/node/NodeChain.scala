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

import gwen.core.node.gherkin.GherkinNode
import gwen.core.node.gherkin.Step
import gwen.core.state.ReservedParam

class NodeChain(val nodes: List[GwenNode]) {

  assert(nodes.nonEmpty, "IllegalState: nodes cannot be empty")

  def previous: GwenNode = nodes(nodes.size - 2)
  def last: GwenNode = nodes.last
  def take(n: Int): NodeChain = new NodeChain(nodes.take(n))
  def add(node: GwenNode):NodeChain = new NodeChain(nodes ++ List(node))
  def steps: List[Step] = nodes.filter(_.nodeType == NodeType.Step).map(_.asInstanceOf[Step])

  def nodePath: String = {
    nodes match {
      case head :: Nil =>
        s"/${head.name}"
      case head :: tail =>
        (nodes zip tail).foldLeft("") { (path: String, pair: (GwenNode, GwenNode)) =>
          val (parent, node) = pair
          val name = node.name
          val occurrenceNo = occurrence(parent, node)
          s"$path/$name$occurrenceNo"
        }
      case _ => 
        ""
    }
  }

  private def occurrence(parent: GwenNode, node: GwenNode): String = {
    node match {
      case _: GherkinNode =>  
        node.params collectFirst { case (name, value) 
          if name == ReservedParam.`iteration.number`.toString => value
        } orElse {
          node.occurrenceIn(parent) orElse {
            parent match {
              case step: Step if step.stepDef.map(_ == node).getOrElse(false) => Some(1)
              case _ => None
            }
          }
        } map { occurrence => 
          s"[$occurrence]"
        } getOrElse ""
      case _ => ""
    }
  }

}

object NodeChain {

  def apply(): NodeChain = new NodeChain(List(Root))

}

