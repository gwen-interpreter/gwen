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
import gwen.errors.scriptError

/**
  * Can be mixed into evaluation engines to provide Script support. Currently only
  * JavaScript is supported but other scripting languages can also be added.
  */
trait ScriptSupport {

  /**
    * Evaluates a JavaScript expression.
    *
    * @param javascript the JavaScript expression to evaluate
    * @param params optional parameters to the script
    */
  def evaluateJS(javascript: String, params: Any*): Any =
    evaluateScript("JavaScript", javascript, params.map(_.asInstanceOf[AnyRef]) : _*)

  /**
    * Evaluates a javascript predicate.
    *
    * @param javascript the script predicate expression to execute (must evaluate to true or false)
    * @param params optional parameters to the script
    */
  def evaluateJSPredicate(javascript: String, params: Any*): Boolean =
    evaluateJS(formatJSReturn(javascript), params.map(_.asInstanceOf[AnyRef]) : _*).asInstanceOf[Boolean]

  /**
    * Formats the given javascript expression in preparation for execute and return
    * (this implementation returns the javascript expression 'as is' since it uses the Java script engine
    * which does not require a 'return ' prefix, but subclasses can override it to include the prefix if necessary).
    *
    * @param javascript the javascript function
    */
  def formatJSReturn(javascript: String) = javascript

  /**
    * Evaluates a script expression.
    *
    * @param language the scripting language name (e.g: JavaScript)
    * @param script the script expression to evaluate
    * @param params optional parameters to the script
    */
  private def evaluateScript[T](language: String, script: String, params: Any*): T = {
    try {
      new ScriptEngineManager(null).getEngineByName(language).eval(s"(function() { return $script })()").asInstanceOf[T]
    } catch {
      case e: Throwable => scriptError(language, script, e)
    }
  }

}