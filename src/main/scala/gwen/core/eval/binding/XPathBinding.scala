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
import gwen.core.eval.support.XMLNodeType
import gwen.core.state.Environment
import gwen.core.state.SensitiveData

import scala.util.Try

object XPathBinding {

  def baseKey(name: String) = s"$name/${BindingType.xpath}"
  private def xpathKey(name: String) = s"${baseKey(name)}/expression"
  private def targetKey(name: String) = s"${baseKey(name)}/targetType"
  private def sourceKey(name: String) = s"${baseKey(name)}/source"
  private def maskedKey(name: String) = s"${baseKey(name)}/masked"

  def bind(name: String, xpath: String, target: String, source: String, masked: Boolean, env: Environment): Unit = {
    env.topScope.clear(name)
    env.topScope.set(xpathKey(name), xpath)
    env.topScope.set(targetKey(name), target)
    env.topScope.set(sourceKey(name), source)
    if (masked) {
      env.topScope.set(maskedKey(name), true.toString)
    }
  }

}

class XPathBinding[T <: EvalContext](name: String, ctx: T) extends Binding[T, String](name, ctx) {

  val xpathKey = XPathBinding.xpathKey(name)
  val targetKey = XPathBinding.targetKey(name)
  val sourceKey = XPathBinding.sourceKey(name)
  val maskedKey = XPathBinding.maskedKey(name)

  override def resolve(): String = {
    bindIfLazy(
      resolveValue(xpathKey) { xpath =>
        resolveValue(targetKey) { target =>
          resolveRef(sourceKey) { source =>
            ctx.evaluate(resolveDryValue(BindingType.xpath.toString)) {
              val value = ctx.evaluateXPath(xpath, source, XMLNodeType.valueOf(target))
              val masked = ctx.topScope.getOpt(maskedKey).map(_.toBoolean).getOrElse(false)
              if (masked) SensitiveData.mask(name, value) else value
            }
          }
        }
      }
    )
  }

  override def toString: String = Try {
    resolveValue(xpathKey) { xpath =>
      resolveValue(targetKey) { target =>
        resolveValue(sourceKey) { source =>
          s"$name [${BindingType.xpath}: $xpath, target: $target, source: $source]"
        }
      }
    }
  } getOrElse name

}
