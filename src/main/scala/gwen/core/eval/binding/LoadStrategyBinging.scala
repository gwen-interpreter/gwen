/*
 * Copyright 2022 Branko Juric, Brady Wood
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

import gwen.core.LoadStrategy
import gwen.core.eval.EvalContext
import gwen.core.state.Environment

object LoadStrategyBinding {

  def key(name: String) = s"$name/${BindingType.loading}"

  def bind[T <: EvalContext](name: String, value: Option[String], strategy: LoadStrategy, ctx: T): Unit = {
    value match {
      case Some(v) =>
        if (strategy == LoadStrategy.Eager) {
          AttributeBinding.bind(name, v, ctx)
        }
        else if (strategy == LoadStrategy.Lazy) {
          AttributeBinding.bind(name, v, ctx)
          val nKey = key(name)
          if (ctx.topScope.getOpt(nKey).nonEmpty) {
            ctx.topScope.set(key(name), null)
          }
        }
      case None =>
        if (strategy == LoadStrategy.Lazy) {
          ctx.topScope.set(key(name), strategy.toString)
        }
    }
  }

  def bindIfLazy[T <: EvalContext](name: String, value: String, ctx: T): Unit = {
    ctx.topScope.getOpt(key(name)).map(LoadStrategy.valueOf).filter(_ == LoadStrategy.Lazy) foreach { strategy => 
      LoadStrategyBinding.bind(name, Option(value), strategy, ctx)
    }
  }

}
