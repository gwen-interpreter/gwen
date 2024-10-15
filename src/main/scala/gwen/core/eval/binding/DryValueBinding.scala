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

package gwen.core.eval.binding

import gwen.core.eval.EvalContext
import gwen.core.state.Environment

import scala.io.Source
import scala.util.Try

import java.io.File
import java.io.FileNotFoundException

object DryValueBinding {
  
  def key(name: String) = s"$name/${BindingType.dryValue}"

  def bind(name: String, value: String, env: Environment): Unit = {
    env.topScope.set(key(name), value)
  }

  def unresolved(bindingType: BindingType): String = unresolved(bindingType.toString)
  def unresolved(value: String): String = s"$$[${BindingType.dryValue}:$value]"

}

class DryValueBinding[T <: EvalContext](name: String, defaultValue: String, ctx: T) extends Binding[T, String](name, ctx) {

  private val key = DryValueBinding.key(name)

  override def resolve(): String = {
    bindIfLazy(
      resolveOpt getOrElse {
        Try(SimpleBinding(name, ctx).resolve()) getOrElse {
          DryValueBinding.unresolved(defaultValue)
        }
      }
    )
  }

  def resolveOpt: Option[String] = ctx.topScope.getOpt(key)

  override def toString: String = Try {
    resolveValue(key) { value =>
      s"$name [${BindingType.dryValue}: $value]"
    }
  } getOrElse name

}
