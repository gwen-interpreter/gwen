/*
 * Copyright 2021-2024 Branko Juric, Brady Wood
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
import gwen.core.state.SensitiveData

import scala.util.Try

object JsonPathBinding {
  
  def baseKey(name: String) = s"$name/${BindingType.`json path`}"
  private def pathKey(name: String) = s"${baseKey(name)}/expression"
  private def sourceKey(name: String) = s"${baseKey(name)}/source"
  private def maskedKey(name: String) = s"${baseKey(name)}/masked"

  def bind(name: String, jsonPath: String, source: String, masked: Boolean, env: Environment): Unit = {
    env.topScope.clear(name)
    env.topScope.set(pathKey(name), jsonPath)
    env.topScope.set(sourceKey(name), source)
    if (masked) {
      env.topScope.set(maskedKey(name), true.toString)
    }
  }

}

class JsonPathBinding[T <: EvalContext](name: String, ctx: T) extends Binding[T, String](name, ctx) {

  val pathKey = JsonPathBinding.pathKey(name)
  val sourceKey = JsonPathBinding.sourceKey(name)
  val maskedKey = JsonPathBinding.maskedKey(name)

  override def resolve(): String = {
    bindIfLazy(
      resolveValue(pathKey) { jsonPath => 
        resolveRef(sourceKey) { source =>
          ctx.evaluate(resolveDryValue(BindingType.`json path`.toString)) {
            val value = ctx.evaluateJsonPath(jsonPath, source)
            val masked = ctx.topScope.getOpt(maskedKey).map(_.toBoolean).getOrElse(false)
            if (masked) SensitiveData.mask(name, value) else value
          }
        }
      }
    )
  }

  override def toString: String = Try {
    resolveValue(pathKey) { jsonPath => 
      resolveValue(sourceKey) { source =>
        s"$name [${BindingType.`json path`}: $jsonPath, source: $source]"
      }
    }
  } getOrElse name

}
