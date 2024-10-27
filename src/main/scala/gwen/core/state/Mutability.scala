/*
 * Copyright 2024 Branko Juric, Brady Wood
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

package gwen.core.state

import gwen.core.Errors
import gwen.core.GwenSettings
import gwen.core.state.Environment
import gwen.core.node.gherkin.Annotations

trait Mutability {

    private def key(name: String): String = {
      s"${name.takeWhile(_ != '/')}/readOnly"
    }

    def checkMutability(name: String, scopedData: ScopedData): Unit = {
      scopedData.getOpt(key(name)).map(Annotations.valueOf) foreach { annotation =>
        if (GwenSettings.`gwen.input.data.readOnly`) {
          Errors.immutableModificationError(name, annotation)
        }
      }
    }

    def setReadOnly(name: String, annotation: Annotations, scopedData: ScopedData): Unit = {
      if (GwenSettings.`gwen.input.data.readOnly`) {
        scopedData.set(key(name), annotation.toString, force = true)
      }
    }
}