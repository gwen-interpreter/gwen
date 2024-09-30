/*
 * Copyright 2014-2021 Branko Juric, Brady Wood
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

import gwen.core.Occurrence
import gwen.core.UUIDGenerator
import gwen.core.node.NodeType
import gwen.core.status.EvalStatus
import gwen.core.status.Pending

trait GwenNode {

  val uuid: String = UUIDGenerator.nextId
  val sourceRef: Option[SourceRef]
  val name: String
  val nodeType: NodeType
  val evalStatus: EvalStatus = Pending
  val params: List[(String, String)] = Nil
  val callerParams: List[(String, String)] = Nil

  def siblingsIn(parent: GwenNode): List[GwenNode]

  final def indexIn(parent: GwenNode): Option[Int] = {
    indexIn(siblingsIn(parent))
  }

  final def isLast: Boolean = {
    siblingsIn(this).lastOption.map(_ == this).getOrElse(false)
  }

  private def indexIn(nodes: List[GwenNode]): Option[Int] = {
    nodes.zipWithIndex.collectFirst {
      case (that, idx) if (that == this) || (that.sourceRef.nonEmpty && that.sourceRef == this.sourceRef) => 
        Some(idx)
    } getOrElse None
  }
  
  override def toString: String = name
}
