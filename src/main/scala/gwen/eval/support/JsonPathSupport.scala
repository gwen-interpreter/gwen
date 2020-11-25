/*
 * Copyright 2016 Branko Juric, Brady Wood
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

package gwen.eval.support

import gwen._
import gwen.eval.EnvContext

import scala.util.Try

import com.jayway.jsonpath.JsonPath

/** Can be mixed into evaluation contexts to provide Json path support. */
trait JsonPathSupport {
  this: EnvContext =>

  /**
    * Evaluates an json path expression against a JSON source string and returns 
    * the result.
    * 
    * @param jsonpath the json path expression
    * @param source the json source string
    * @return the result of evaluating the json path expression
    */
  def evaluateJsonPath(jsonpath: String, source: String): String = {
    evaluate("$[dryRun:json path]") {
      if (source.trim().length() == 0) {
        Errors.jsonPathError("Cannot evaluate Json path on empty source")
      }
      Try(JsonPath.parse(source).read(jsonpath).toString).getOrElse("")
    }
  }

}