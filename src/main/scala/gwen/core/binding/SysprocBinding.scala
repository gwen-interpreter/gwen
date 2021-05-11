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

package gwen.core.engine.binding

import gwen.core.engine.EvalContext
import gwen.core.engine.EvalEnvironment

import scala.sys.process._
import scala.util.Try

object SysprocBinding {
  
  def key(name: String) = s"$name/${BindingType.sysproc}"

  def bind(name: String, sysproc: String, env: EvalEnvironment): Unit = {
    env.scopes.set(key(name), sysproc)
  }

}

class SysprocBinding[T <: EvalContext](name: String, ctx: T) extends Binding[T, String](name, ctx) {

  private val key = SysprocBinding.key(name)

  override def resolve(): String = {
    lookupValue(key) { sysproc => 
      ctx.evaluate(s"$$[dryRun:${BindingType.sysproc}]") {
        sysproc.!!.trim
      }
    }
  }

  override def toString: String = Try {
    lookupValue(key) { sysproc => 
      s"$name [${BindingType.sysproc}: $sysproc]"
    }
  } getOrElse name

}
