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

import gwen.core.Errors
import gwen.core.eval.EvalContext

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
    * Looks up, interpolates and a bound reference
    *
    * @param key the bound value key
    * @return the resolved and interpolated value
    */
  private [binding] def resolveRef(key: String)(resolver: String => U): U = { 
    resolve(key) { value =>
      resolver(ctx.interpolate(ctx.getBoundReferenceValue(value)))
    }
  }

  /**
    * Looks up, interpolates and resolves a bound value
    *
    * @param key the bound value key
    * @return the resolved and interpolated value
    */
  private def resolve(key: String)(resolver: String => U): U = ctx.withEnv { env => 
    env.scopes.getOpt(key) map { value => 
      resolver(value)
    } getOrElse {
      Errors.unboundAttributeError(name)
    }
  }
  
}