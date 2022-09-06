/*
 * Copyright 2014-2021 Branko Juric, Brady Wood
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

package gwen.core.node

import gwen.core._
import gwen.core.node.gherkin.TagFilter
import gwen.core.result.SpecResult
import gwen.core.state.DataRecord

import java.io.File

/**
  * Captures a feature file and its associated meta as a unit.
  * 
  * @param featureFile the feature file
  * @param metaFiles the associated meta files (if any)
  * @param dataRecord optional data record
  * @param tagFilter tag filter
  * @param result optional result
  */
case class FeatureUnit(
    parent: GwenNode,
    featureFile: File,
    metaFiles: List[File], 
    dataRecord: Option[DataRecord], 
    tagFilter: TagFilter,
    result: Option[SpecResult] = None) extends GwenNode {

  override val sourceRef: Option[SourceRef] = None
  override val name: String = featureFile.uri
  val displayName: String = s"$name${dataRecord.map(rec => if (FileIO.isMetaFile(featureFile)) "" else s" [${rec.descriptor}]").getOrElse("")}"
  override val nodeType: NodeType = NodeType.Unit
  override def siblingsIn(parent: GwenNode): List[GwenNode] = Nil

  def ancestor: GwenNode = {
    parent match {
      case parentUnit: FeatureUnit => 
        if (parentUnit.parent == Root) parent
        else parentUnit.ancestor
      case _ => this
    }
  }

}

object FeatureUnit {
  def apply(parent: GwenNode, unit: FeatureUnit, result: SpecResult): FeatureUnit = {
    FeatureUnit(parent, unit.featureFile, unit.metaFiles, unit.dataRecord, unit.tagFilter, Some(result))
  }
}
