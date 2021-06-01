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

package gwen.core.node.gherkin

import gwen.core.node.GwenNode
import gwen.core.node.SourceRef
import gwen.core.status._

/**
  * Base trait for all Gherkin nodes.  
  */
trait GherkinNode extends GwenNode {

  /** The location in the Gherkin file or None if the node is synthetic or instantiated directly. */
  val sourceRef: Option[SourceRef]

  /** The name of the node. */
  val name: String

  /** Returns the evaluation status of this node. */
  val evalStatus: EvalStatus = Pending

  private [gherkin] def occurrenceIn(nodes: List[GherkinNode]): Int = {
    1 + indexIn(
      nodes filter { that => 
        that.name.size > 0 && that.name == this.name
      }
    )
  }

  private [gherkin] def indexIn(nodes: List[GherkinNode]): Int = {
    nodes.zipWithIndex.collectFirst {
      case (that, idx) if that.sourceRef == this.sourceRef => idx
    } getOrElse -1
  }

  override def toString: String = name

}
