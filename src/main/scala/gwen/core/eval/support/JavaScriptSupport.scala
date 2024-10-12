/*
 * Copyright 2017-2022 Branko Juric, Brady Wood
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

package gwen.core.eval.support

import gwen.core._
import gwen.core.eval.EvalContext
import gwen.core.state.SensitiveData

import scala.util.Try
import scala.util.Success

import javax.script.ScriptEngineManager

/**
  * Can be mixed into evaluation engines to provide JavaScript support.
  */
trait JavaScriptSupport[T <: EvalContext] extends ArrowFunctionSupport[T] {
  this: T =>

  /**
    * Evaluates a JavaScript expression.
    *
    * @param javascript the JavaScript expression to evaluate (with no params)
    */
  def evaluateJS(javascript: String): Any = evaluateJS(javascript, Nil)

  /**
    * Evaluates a JavaScript expression.
    *
    * @param javascript the JavaScript expression to evaluate
    * @param params optional parameters to the script
    */
  def evaluateJS(javascript: String, params: List[Any]): Any = {
    val jScript = params.map(_.toString).zipWithIndex.foldLeft(javascript) { (js, paramAndIndex) => 
      val (param, index) = paramAndIndex
      js.replaceAll(s"arguments\\[$index\\]", Formatting.surroundWithQuotes(param))
    }
    try {
      SensitiveData.withValue(jScript) { js =>
        new ScriptEngineManager().getEngineByName("js").eval(formatJSReturn(parseJS(js)))
      }
    } catch {
      case e: Errors.GwenException => throw e
      case e: Throwable => Errors.functionError(jScript, e)
    }
  }

  /**
    * Formats the given javascript expression in preparation for execute and return
    * (this implementation returns the javascript expression 'as is' since it uses the Java script engine
    * which does not require a 'return ' prefix, but subclasses can override it to include the prefix if necessary).
    *
    * @param javascript the javascript function
    */
  def formatJSReturn(javascript: String) = javascript

  def parseJS(javascript: String): String = {
    Try(parseArrowFunction(javascript)) match {
      case Success(Some(func)) => func.wrapper
      case _ => javascript
    }
  }

  def jsFunctionWrapper(args: List[(String, String)], body: String): String = {
    val names = args.map(_._1)
    val values = args.map(_._2)
    val jsBody = if (body.trim.startsWith("{") && body.trim.endsWith("}")) body.trim.drop(1).dropRight(1).trim else s"return $body"
    jsFunctionWrapper(names.mkString(","), values.mkString(","), jsBody)
  }

  def jsFunctionWrapper(name: String, value: String, body: String): String = {
    s"(function($name) { $body })(${value})"
  }

}
