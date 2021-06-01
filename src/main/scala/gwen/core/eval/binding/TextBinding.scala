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

object TextBinding {

  def key(name: String) = s"$name/${BindingType.text}"

  def bind(name: String, text: String, env: Environment): Unit = {
    env.scopes.set(key(name), text)
  }

}

class TextBinding[T <: EvalContext](name: String, ctx: T) extends Binding[T, String](name, ctx) {
  
  val key = TextBinding.key(name)

  override def resolve(): String = {
    lookupValue(key) { identity }
  }

  override def toString: String = name

}
