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

import scala.sys.process._
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import gwen.core.Errors

object SysprocBinding {

  private object Lock
  
  def key(name: String) = s"$name/${BindingType.sysproc}"
  def delimiterKey(name: String) = s"$name/delimiter"
  def unixKey(name: String) = s"$name/unix"
  def maskedKey(name: String) = s"$name/masked"

  def bind(name: String, sysproc: String, delimiter: Option[String], unix: Boolean, masked: Boolean, env: Environment): Unit = {
    env.topScope.clear(name)
    env.topScope.set(key(name), sysproc)
    delimiter foreach { delim => 
      env.topScope.set(delimiterKey(name), delim)
    }
    env.topScope.set(unixKey(name), unix.toString)
    if (masked) {
      env.topScope.set(maskedKey(name), true.toString)
    }
  }

}

class SysprocBinding[T <: EvalContext](name: String, ctx: T) extends Binding[T, String](name, ctx) {

  private val key = SysprocBinding.key(name)
  private val delimiterKey = SysprocBinding.delimiterKey(name)
  private val unixKey = SysprocBinding.unixKey(name)
  private val maskedKey = SysprocBinding.maskedKey(name)

  override def resolve(): String = {
    val delimiter = ctx.topScope.getOpt(delimiterKey).map(ctx.interpolate)
    val unix = ctx.topScope.getOpt(unixKey).map(ctx.interpolate).map(_.toBoolean).getOrElse(false)
    bindIfLazy(
      lookupValue(key) { sysproc => 
        ctx.evaluate(resolveDryValue(s"${if (unix) BindingType.unixsysproc else BindingType.sysproc}${delimiter.map(d => s", delimiter: $d").getOrElse("")}")) {
          val value = SysprocBinding.Lock.synchronized { ctx.callSysProc(sysproc, delimiter, unix) }
          val masked = ctx.topScope.getOpt(maskedKey).map(_.toBoolean).getOrElse(false)
          if (masked) SensitiveData.mask(name, value) else value
        }
      }
    )
  }

  override def toString: String = Try {
    val delimiter = ctx.topScope.getOpt(delimiterKey).map(ctx.interpolate)
    val unix = ctx.topScope.getOpt(unixKey).map(ctx.interpolate).map(_.toBoolean).getOrElse(false)
    lookupValue(key) { sysproc => 
      s"$name [${if (unix) BindingType.unixsysproc else BindingType.sysproc}: $sysproc${delimiter.map(d => s", delimiter: $d").getOrElse("")}]"
    }
  } getOrElse name

}
