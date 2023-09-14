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

package gwen.core.eval.binding

import scala.util.Failure
import scala.util.Success
import scala.util.Try

enum BindingType:
  case text, javascript, function, xpath, regex, sysproc, unixsysproc, property, setting, file, sql, `json path`, loading, dryValue

object BindingType {

  def parse(bType: String): BindingType = {
    Try(valueOf(bType)) match {
      case Success(value) => value
      case Failure(error) => bType match {
        case "js" => javascript
        case "system process" => sysproc
        case "unix system process" => unixsysproc
        case _ => throw error
      }
    }
  }

}

