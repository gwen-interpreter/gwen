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

package gwen.model

import gwen._
import gwen.model._

import com.typesafe.scalalogging.LazyLogging

trait Identifiable {
  def nodeType: NodeType.Value
  val uuid: String = UUIDGenerator.nextId
}

object Root extends Identifiable {
  def nodeType: NodeType.Value = NodeType.Root
  override val uuid: String = UUIDGenerator.baseId
}

/**
  * Base trait for all Gherkin specification nodes.  
  */
trait SpecNode extends Identifiable with LazyLogging {

  /** The location in the Gherkin file or None if the node is synthetic or instantiated directly. */
  val sourceRef: Option[SourceRef]

  /** The name of the node. */
  val name: String

  /** Returns the evaluation status of this node. */
  val evalStatus: EvalStatus = Pending

  /** Gets the index of the node relative to parent. */
  def index = sourceRef.map(_.pos.index).getOrElse(0)
  
  def logStatus(): Unit = { 
    val msg = s"$evalStatus $nodeType: $name"
    evalStatus match {
      case Loaded => logger.debug(msg)
      case Passed(_) => logger.info(msg)
      case Failed(_, _) => logger.error(msg)
      case Sustained(_, _) => logger.warn(msg)
      case _ => logger.warn(msg)
    }
  }

  override def toString: String = name

}
