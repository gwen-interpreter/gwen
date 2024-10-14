/*
 * Copyright 2024 Branko Juric, Brady Wood
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

 package gwen.core

 import java.io.File

 case class Process(name: String, baseDir: File) {

  def isDefault: Boolean = name == ""

  def settingsFile: Option[File] = {
    if (name.trim != "") {
        val processDir = new File(new File(baseDir, "conf"), "process")
        val files = List("conf", "json", "properties").map(ext => new File(processDir, s"${name}.$ext"))
        val exists = files.filter(_.exists)
        if (exists.isEmpty) Errors.invocationError(s"At least one of ${files.take(files.length - 1).map(_.getName).mkString(", ")} or ${files.last.getName} files must exist in $processDir for $name process")
        else if (exists.length > 1) Errors.invocationError(s"Only one of ${exists.take(exists.length - 1).map(_.getName).mkString(", ")} or ${exists.last.getName} files must exist in $processDir for $name process")
        else Some(exists.head)
    } else {
        None
    }
  }

 }
 