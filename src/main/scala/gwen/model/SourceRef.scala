/*
 * Copyright 2020 Branko Juric, Brady Wood
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

import io.cucumber.messages.{ Messages => Cucumber }

import java.io.File

/** Reperesents a location in source. */
case class SourceRef(uri: String, pos: Position) {
  def isFeature = uri.endsWith(".feature")
  def isMeta = uri.endsWith(".meta")
  override def toString: String = SourceRef.asString(Some(uri), Some(pos.line), Some(pos.column))
}
object SourceRef {
  private val lineOffset = new ThreadLocal[Int]() {
    override protected def initialValue: Int = 0
  }
  def setLineOffset(offset: Int): Unit = {
    lineOffset.set(offset)
  }
  def apply(uri: String, location: Cucumber.Location): SourceRef = {
    SourceRef(uri, Position(location.getLine + lineOffset.get, location.getColumn))
  }
  def asString(sourceRef: Option[SourceRef]): String = {
    SourceRef.asString(None, sourceRef)
  }
  def asString(file: Option[File], sourceRef: Option[SourceRef]): String = {
    SourceRef.asString(
      file.map(_.getPath).orElse(sourceRef.map(_.uri)), 
      sourceRef.map(_.pos.line), 
      sourceRef.map(_.pos.column))
  }
  def asString(uri: Option[String], line: Option[Int], column: Option[Int]): String = {
    s"${uri.filter(_.length > 0).map(u => s"$u:").getOrElse("")}${Position.asString(line, column)}"
  }
  
}