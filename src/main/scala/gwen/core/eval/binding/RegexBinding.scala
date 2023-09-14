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

import gwen.core.eval.EvalContext
import gwen.core.state.Environment

import scala.util.Try

object RegexBinding {

  def baseKey(name: String) = s"$name/${BindingType.regex}"
  private def regexKey(name: String) = s"${baseKey(name)}/expression"
  private def sourceKey(name: String) = s"${baseKey(name)}/source"

  def bind(name: String, regex: String, source: String, env: Environment): Unit = {
    env.scopes.clear(name)
    env.scopes.set(regexKey(name), regex)
    env.scopes.set(sourceKey(name), source)
  }

}

class RegexBinding[T <: EvalContext](name: String, ctx: T) extends Binding[T, String](name, ctx) {

  val regexKey = RegexBinding.regexKey(name)
  val sourceKey = RegexBinding.sourceKey(name)

  override def resolve(): String = {
    bindIfLazy(
      resolveValue(regexKey) { regex =>
        resolveRef(sourceKey) { source =>
          ctx.evaluate(resolveDryValue(BindingType.regex.toString)) {
            ctx.extractByRegex(regex, source)
          }
        }
      }
    )
  }

  override def toString: String = Try {
    resolveValue(regexKey) { regex =>
      resolveValue(sourceKey) { source =>
        s"$name [${BindingType.regex}: $regex, source: $source]"
      }
    }
  } getOrElse name

}
