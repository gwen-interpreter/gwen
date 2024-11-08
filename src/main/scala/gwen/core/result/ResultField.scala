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

package gwen.core.result

import gwen.core.Errors
import gwen.core.status.StatusKeyword

import java.io.File

import scala.util.Try

case class ResultField(name: String, ref: String, defaultValue: Option[String])

object ResultField {
  def validateSettingName(name: String): Unit = {
    Try(ResultFieldAtts.valueOf(name)) getOrElse {
      Errors.illegalSettingAttributeError(name, "gwen.report.results.fields", ResultFieldAtts.values.mkString(", "))
    }
  }
}

enum ResultFieldAtts:
  case field, ref, defaultValue, excludes