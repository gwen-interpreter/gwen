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

package gwen.eval

import gwen.dsl.DataTable

import java.io.File

trait Binding {
  val name: String
}

case class TextBinding(name: String, value: String) extends Binding {
  override def toString: String = name
}

case class JavaScriptBinding(name: String, javascript: String) extends Binding {
  override def toString: String = s"$name [javascript: $javascript]"
}

case class XPathBinding(name: String, xpath: String, target: String, source: String) extends Binding {
  override def toString: String = s"$name [xpath: $xpath, target: $target, source: $source]"
}

case class RegexBinding(name: String, regex: String, source: String) extends Binding {
  override def toString: String = s"$name [regex: $regex, source: $source]"
}

case class JsonPathBinding(name: String, jsonPath: String, source: String) extends Binding {
  override def toString: String = s"$name [json path: $jsonPath, source: $source]"
}

case class SysprocBinding(name: String, sysproc: String) extends Binding {
  override def toString: String = s"$name [sysproc: $sysproc]"
}

case class FileBinding(name: String, file: File) extends Binding {
  override def toString: String = s"$name [file: $file]"
}

case class SQLBinding(name: String, db: String, sql: String) extends Binding {
  override def toString: String = s"$name [db: $db, sql: $sql]"
}

case class RecordBinding(name: String, record: ScopedData) extends Binding {
  override def toString: String = name
}

case class TableBinding(name: String, table: DataTable) extends Binding {
  override def toString: String = name
}

case class SettingsBinding(name: String, value: String) extends Binding {
  override def toString: String = name
}

case class AttributeBinding(name: String, value: String) extends Binding {
  override def toString: String = name
}
