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

import io.cucumber.messages.{ Messages => Cucumber }

import java.io.File

/** 
 * Reperesents a location in source. 
 * 
 * @param file the source file
 * @param line the line number in the source (base 1)
 */
case class SourceRef(file: Option[File], line: Int) {
  
  def uri = file.map(_.getPath).getOrElse("")
  def isFeature = uri.endsWith(".feature")
  def isMeta = uri.endsWith(".meta")

  def copy(withFile: Option[File] = file, withLine: Int = line): SourceRef = {
    SourceRef(withFile, withLine)
  }
  
  override def toString: String = s"$uri:$line"

}

object SourceRef {
  private val lineOffset = new ThreadLocal[Int]() {
    override protected def initialValue: Int = 0
  }
  def apply(file: Option[File], location: Cucumber.Location): SourceRef = {
    SourceRef(file, location.getLine + lineOffset.get)
  }
  def setLineOffset(offset: Int): Unit = {
    lineOffset.set(offset)
  }
  def toString(file: Option[File], line: Option[Int], column: Option[Int]): String = {
    (file, line, column) match {
      case (Some(f), Some(l), Some(c)) => s"${f.getPath}:$l:$c"
      case (Some(f), Some(l), None) => s"${f.getPath}:$l"
      case (Some(f), None, Some(c)) => s"${f.getPath}::$c"
      case (Some(f), None, None) => f.getPath
      case (None, Some(l), Some(c)) => s"line $l column $c"
      case (None, Some(l), None) => s"line $l"
      case (None, None, Some(c)) => s"column $c"
      case _ => ""
    }
  }
  
}
