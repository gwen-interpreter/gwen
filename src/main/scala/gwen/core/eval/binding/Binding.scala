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

import gwen.core.GwenSettings
import gwen.core.Errors
import gwen.core.eval.EvalContext
import gwen.core.state.Environment

import util.chaining.scalaUtilChainingOps

/** 
 * Defines a named binding and provides access to its evaluated value. 
 */
abstract class Binding[T <: EvalContext, U](name: String, ctx: T) {

  /**
    * Resolves the bound value
    * @return the bound value
    */
  def resolve(): U

  /**
    * Looks up a bound value
    *
    * @param key the bound value key
    * @return the looked up value
    */
  private [binding] def lookupValue(key: String)(resolver: String => U): U = { 
    resolve(key) { resolver }
  }

  /**
    * Looks up, interpolates and a bound value
    *
    * @param key the bound value key
    * @return the resolved and interpolated value
    */
  private [binding] def resolveValue(key: String)(resolver: String => U): U = { 
    resolve(key) { value =>
      resolver(ctx.interpolate(value))
    }
  }

  /**
    * Looks up an optional dry run value
    *
    * @param key the bound value key
    * @return the resolved and interpolated value
    */
  def resolveDryValue(defaultValue: String): String = { 
    val binding = new DryValueBinding[T](name, defaultValue, ctx)
    binding.resolve()
  }

  /**
    * Looks up an optional dry run value
    *
    * @param key the bound value key
    * @return the resolved and interpolated value
    */
  def resolveDryValueOpt(): Option[String] = { 
    val binding = new DryValueBinding[T](name, "", ctx)
    binding.resolveOpt
  }

  /**
    * Looks up, interpolates and a bound reference
    *
    * @param key the bound value key
    * @return the resolved and interpolated value
    */
  private [binding] def resolveRef(key: String)(resolver: String => U): U = { 
    resolve(key) { value =>
      resolver(ctx.interpolate(ctx.getBoundValue(value)))
    }
  }

  /**
    * Looks up, interpolates and resolves a bound value
    *
    * @param key the bound value key
    * @return the resolved and interpolated value
    */
  private def resolve(key: String)(resolver: String => U): U = { 
    ctx.topScope.getOpt(key) map { value => 
      resolver(value)
    } getOrElse {
      Errors.unboundAttributeError(name)
    }
  }

  private [binding] def bindIfLazy(value: String): String = {
    value tap { _ =>
      LoadStrategyBinding.bindIfLazy(name, value, ctx)
    }
  }

  def displayName: String = {
    if (GwenSettings.`gwen.error.messages.inline.locators`) this.toString
    else name
  }
  
}
