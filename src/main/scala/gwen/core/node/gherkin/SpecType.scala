/*
 * Copyright 2017-2021 Branko Juric, Brady Wood
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

package gwen.core.node.gherkin

import gwen.core._

import java.io.File

object SpecType extends Enumeration {

  val Feature, Meta = Value

  def isFeature(specType: SpecType.Value):Boolean = specType == Feature
  def isMeta(specType: SpecType.Value):Boolean = specType == Meta

  def ofFile(specFile: File): SpecType.Value = {
    if (FileIO.isMetaFile(specFile)) SpecType.Meta
    else SpecType.Feature
  }

}
