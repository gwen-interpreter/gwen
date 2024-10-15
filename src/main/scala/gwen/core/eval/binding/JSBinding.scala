/*
 * Copyright 2021-2022 Branko Juric, Brady Wood
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

import gwen.core.eval.EvalContext
import gwen.core.eval.support.JavaScriptSupport
import gwen.core.state.Environment
import gwen.core.state.SensitiveData

import scala.util.Try

object JSBinding {
  
  def key(name: String) = s"$name/${BindingType.javascript}"
  def maskedKey(name: String) = s"${key(name)}/masked"

  def bind(name: String, javascript: String, masked: Boolean, env: Environment): Unit = {
    env.topScope.clear(name)
    env.topScope.set(key(name), javascript)
    if (masked) {
      env.topScope.set(maskedKey(name), true.toString)
    }
  }

  def find[T <: EvalContext](name: String, ctx: T): Try[JSBinding[T]] = Try {
    ctx.topScope.get(key(name))
    new JSBinding(name, Nil, ctx)
  }

}

class JSBinding[T <: EvalContext](name: String, params: List[String], ctx: T) extends Binding[T, String](name, ctx) {

  val key = JSBinding.key(name)
  val maskedKey = JSBinding.maskedKey(name)

  override def resolve(): String = {
    resolveValue(key) { javascript =>
      val value = evaluateFunction(javascript, params)
      val masked = ctx.topScope.getOpt(maskedKey).map(_.toBoolean).getOrElse(false)
      if (masked) SensitiveData.mask(name, value) else value
    }
  }

  def evaluateFunction(javascript: String, params: List[String]): String = {
    bindIfLazy {
      ctx.evaluate(resolveDryValue(BindingType.javascript.toString)) {
        Option(ctx.evaluateJS(javascript, params)).map(_.toString).getOrElse("")
      }
    }
  }

  override def toString: String = Try {
    resolveValue(key) { javascript =>
      s"$name [${BindingType.javascript}: $javascript]"
    }
  } getOrElse name

}
