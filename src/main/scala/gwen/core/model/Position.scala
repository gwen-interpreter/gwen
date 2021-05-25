/*
 * Copyright 2020-2021 Branko Juric, Brady Wood
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

package gwen.core.model

/** 
 * Line and column number coordinates (base is 1).
 * 
 * @param line line number in souce (1st is 1)
 * @param column column number in souce (1st is 1)
 * @param indexes List of table and row/node indexes or a single node index
 * 
 */
case class Position(line: Int, column: Int, indexes: List[Int]) {
  val index: Int = if (indexes.nonEmpty) indexes(indexes.size - 1) else 0
  val tableNo: Option[Int] = if (indexes.size > 1) Some(indexes(0) + 1) else None
  val rowNo: Int = index + 1
  
  override def toString: String = Position.asString(Some(line), Some(column), tableNo.map(t => Some((t, rowNo))).getOrElse(None))
}
object Position {
  def apply(line: Int, column: Int, index: Int): Position = {
    Position(line, column, List(index))
  }
  def asString(line: Option[Int], column: Option[Int], tableRow: Option[(Int, Int)]): String = {
    (line, column, tableRow) match {
      case (Some(l), Some(c), Some((t, r))) => s":$l:$c[$t][$r]"
      case (Some(l), Some(c), None) => s":$l:$c"
      case (Some(l), None, Some((t, r))) => s":$l[$t][$r]"
      case (Some(l), None, None) => s":$l"
      case (None, Some(c), Some((t, r))) => s":_:$c[$t][$r]"
      case (None, Some(c), None) => s":_:$c"
      case (None, None, Some((t, r))) => s":_:_[$t][$r]"
      case _ => ""
    }
  }
}
