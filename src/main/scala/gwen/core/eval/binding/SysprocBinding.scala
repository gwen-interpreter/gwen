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

import scala.sys.process._
import scala.util.Try

object SysprocBinding {
  
  def key(name: String) = s"$name/${BindingType.sysproc}"
  def delimiterKey(name: String) = s"$name/delimiter"

  def bind(name: String, sysproc: String, delimiter: Option[String], env: Environment): Unit = {
    env.scopes.clear(name)
    env.scopes.set(key(name), sysproc)
    delimiter foreach { delim => 
      env.scopes.set(delimiterKey(name), delim)
    }
  }

}

class SysprocBinding[T <: EvalContext](name: String, ctx: T) extends Binding[T, String](name, ctx) {

  private val key = SysprocBinding.key(name)
  private val delimiterKey = SysprocBinding.delimiterKey(name)

  override def resolve(): String = {
    val delimiter = ctx.scopes.getOpt(delimiterKey).map(ctx.interpolate)
    bindIfLazy(
      lookupValue(key) { sysproc => 
        ctx.evaluate(s"$$[dryRun:${BindingType.sysproc}${delimiter.map(d => s", delimiter: $d").getOrElse("")}]") {
          delimiter match {
            case Some(delim) => sysproc.split(delim).toSeq.!!.trim
            case None => sysproc.!!.trim
          }
        }
      }
    )
  }

  override def toString: String = Try {
    val delimiter = ctx.scopes.getOpt(delimiterKey).map(ctx.interpolate)
    lookupValue(key) { sysproc => 
      s"$name [${BindingType.sysproc}: $sysproc${delimiter.map(d => s", delimiter: $d").getOrElse("")}]"
    }
  } getOrElse name

}
