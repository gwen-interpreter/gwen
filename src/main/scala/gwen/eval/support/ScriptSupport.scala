/*
 * Copyright 2017 Branko Juric, Brady Wood
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

import javax.script.ScriptEngineManager

/**
  * Can be mixed into evaluation engines to provide Script support. Currently only
  * JavaScript is supported but other scripting languages can also be added.
  */
trait ScriptSupport {

  /**
    * Evaluates a JavaScript expression.
    *
    * @param javascript the JavaScript expression to evaluate
    */
  def evaluateJavaScript[T](javascript: String): T = evaluateScript("JavaScript", javascript)

  /**
    * Evaluates a script expression.
    *
    * @param language the scripting language name (e.g: JavaScript)
    * @param script the script expression to evaluate
    */
  private def evaluateScript[T](language: String, script: String): T =
    new ScriptEngineManager(null).getEngineByName(language).eval(script).asInstanceOf[T]


}