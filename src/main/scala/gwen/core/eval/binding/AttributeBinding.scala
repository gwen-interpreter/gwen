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

import gwen.core.state.Environment

object AttributeBinding {

  def bind(name: String, value: String, env: Environment): Unit = {
    value match {
      case "true" | "false" => 
          JSBinding.bind(name, value, env)
      case _ =>
        env.topScope.set(name, value)
    }
  }

}
