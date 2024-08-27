/*
 * Copyright 2023 Branko Juric, Brady Wood
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

import scala.collection.SeqView
import scala.io.Source

/** Captures the arguments and body of a Javascript arrow function. */
class ArrowFunction[T <: EvalContext](val source: String, argBindings: List[(String, String)], val body: String, ctx: T) {
  private def argNames: List[String] = argBindings map { (name, _) => name }
  private def argRefOpt(name: String): Option[String] = argBindings.find((n, _) => n == name).map((_, ref) => ref)
  private def argValue(name: String): String = argValueOpt(name) getOrElse Errors.functionError(source, s"Undefined argument $name in")
  private def argValueOpt(name: String): Option[String] = argRefOpt(name).map(ctx.getBoundValue)
  lazy val wrapper: String = wrapper(argNames.map(argValue))
  def wrapper(argValues: List[String]): String = {
    val args = (argNames zip argValues) map { (n, v) => 
      (n, if(Source.fromString(v).getLines().toList.length > 1) s"`$v`" else Formatting.surroundWithQuotes(v))
    }
    ctx.jsFunctionWrapper(args, body)
  }
  def apply: String = {
    ctx.addAttachment("js-function", "txt", wrapper) 
    ctx.evaluateJS(wrapper).toString
  }
  override def toString: String = source
}

/**
  * Provides support for and JavaScript arrow functions.
  */
trait ArrowFunctionSupport[T <: EvalContext] {
  this: T =>

  def parseArrowFunction(function: String): Option[ArrowFunction[T]] = {
    if (function.matches("""(?s)\s*\(\s*function\s*\(.*""")) None else {
      val arrowIndex = function.indexOf("=>")
      if (arrowIndex == -1) None else {
        val params = function.substring(0, arrowIndex)
        val body = function.drop(params.length + 2)
        val argList = Option(params).map(_.trim) map { ps =>
          if (ps.startsWith("(") && ps.endsWith(")")) ps.drop(1).dropRight(1) else ps
        } getOrElse ""
        val args = argList.split(",").toList.map(_.trim).filter(_.nonEmpty).zipWithIndex map { (arg, argIndex) =>
          val assigmentIndex = arg.indexOf("=")
          if (assigmentIndex != -1) {
            val n = arg.substring(0, assigmentIndex)
            val ref = arg.drop(n.length + 1).trim
            val name = n.trim
            if (name.isEmpty()) Errors.functionError(function, s"Illegal blank name for argument ${argIndex + 1} in function")
            if (ref.isEmpty()) Errors.functionError(function, s"Illegal blank reference for argument ${argIndex + 1} in function")
            (name, ref)
          } else {
            (arg, arg)
          }
        }
        args filter { (n, _) => 
          n.matches("""[a-z]\w*""")
        } match {
          case Nil if args.nonEmpty => None
          case _ => Some(ArrowFunction(function, args, body.trim, this))
        }
      }
    }
  }

}
