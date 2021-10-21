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

package gwen.core.node

import gwen.core._

import java.io.File
import io.cucumber.messages.{ types => cucumber }

/** 
 * Reperesents a location in source. 
 * 
 * @param file the source file
 * @param line the line number in the source (base 1)
 */
case class SourceRef(file: Option[File], line: Long) {
  
  def uri = file.map(_.uri).getOrElse("")
  def isFeature = uri.endsWith(".feature")
  def isMeta = uri.endsWith(".meta")

  def copy(withFile: Option[File] = file, withLine: Long = line): SourceRef = {
    SourceRef(withFile, withLine)
  }
  
  override def toString: String = s"$uri:$line"

}

object SourceRef {
  private val lineOffset = new ThreadLocal[Long]() {
    override protected def initialValue: Long = 0
  }
  def apply(file: Option[File], location: cucumber.Location): SourceRef = {
    SourceRef(file, location.getLine + lineOffset.get)
  }
  def setLineOffset(offset: Long): Unit = {
    lineOffset.set(offset)
  }
  def toString(file: Option[File], line: Option[Long], column: Option[Long]): String = {
    (file, line, column) match {
      case (Some(f), Some(l), Some(c)) => s"${f.uri}:$l:$c"
      case (Some(f), Some(l), None) => s"${f.uri}:$l"
      case (Some(f), None, Some(c)) => s"${f.uri}::$c"
      case (Some(f), None, None) => f.uri
      case (None, Some(l), Some(c)) => s"line $l column $c"
      case (None, Some(l), None) => s"line $l"
      case (None, None, Some(c)) => s"column $c"
      case _ => ""
    }
  }
  
}
