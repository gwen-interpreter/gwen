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

package gwen.eval.binding

import gwen.eval.EvalContext
import gwen.eval.EvalEnvironment

import scala.util.Try

object JavaScriptBinding {
  
  def key(name: String) = s"$name/${BindingType.javascript}"

  def bind(name: String, javascript: String, env: EvalEnvironment): Unit = {
    env.scopes.set(key(name), javascript)
  }

}

class JavaScriptBinding[T <: EvalContext](name: String, ctx: T) extends Binding[T, String](name, ctx) {

  private val key = JavaScriptBinding.key(name)

  override def resolve(): String = {
    resolveValue(key) { javascript =>
      ctx.evaluate(s"$$[dryRun:${BindingType.javascript}]") {
        Option(ctx.evaluateJS(ctx.formatJSReturn(javascript))).map(_.toString).getOrElse("")
      }
    }
  }

  override def toString: String = Try {
    resolveValue(key) { javascript =>
      s"$name [${BindingType.javascript}: $javascript]"
    }
  } getOrElse name

}
