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

import scala.collection.mutable
import gwen.core.node.gherkin.GherkinNode
import gwen.core.node.gherkin.Step
import gwen.core.state.ReservedParam

class NodeChain {

  private val chain = mutable.Queue[GwenNode]()

  def push(node: GwenNode): List[GwenNode] = { 
    chain += node
    chain.toList
  }

  def pop(): Option[GwenNode] = { 
    chain.removeLastOption(false)
  }

  def nodes: List[GwenNode] = chain.toList

  def nodePath: String = {
    nodes match {
      case head :: Nil =>
        s"/${head.name}"
      case head :: tail =>
        (nodes zip tail).foldLeft("") { (path: String, pair: (GwenNode, GwenNode)) =>
          val (parent, node) = pair
          val name = node.name
          val occurrenceNo = occurrence(parent, node)
          val iterationNo = iteration(node)
          s"$path/$name$occurrenceNo$iterationNo"
        }
      case _ => 
        ""
    }
  }

  private def occurrence(parent: GwenNode, node: GwenNode): String = {
    node match {
      case _: GherkinNode =>  
        node.occurrenceIn(parent) orElse {
          parent match {
            case step: Step if step.stepDef.map(_ == node).getOrElse(false) => Some(1)
            case _ => None
          }
        } map { occurrence => 
          s"[$occurrence]"
        } getOrElse ""
      case _ => ""
    }
  }

  private def iteration(node: GwenNode): String = {
    node match {
      case step: Step =>
        step.params collectFirst { 
          case (name, value) if ReservedParam.iterations.contains(name) => s"[$value]"
        } getOrElse ""
      case _ => ""
    }
  }

}

