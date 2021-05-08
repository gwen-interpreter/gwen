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

package gwen.core.model

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
    parent: Identifiable,
    featureFile: File,
    metaFiles: List[File], 
    dataRecord: Option[DataRecord], 
    tagFilter: TagFilter,
    result: Option[SpecResult] = None) extends Identifiable {

  val nodeType: NodeType.Value = NodeType.Unit
  def ancestor: Identifiable = parent match {
    case parentUnit: FeatureUnit => 
      if (parentUnit.parent == Root) parent
      else parentUnit.ancestor
    case _ => this
  }
  val uri: String = s"${featureFile.getPath}${dataRecord.map(rec => s"[${rec.recordNo}]").getOrElse("")}"
}

object FeatureUnit {
  def apply(parent: Identifiable, unit: FeatureUnit, result: SpecResult): FeatureUnit = {
    FeatureUnit(parent, unit.featureFile, unit.metaFiles, unit.dataRecord, unit.tagFilter, Some(result))
  }
}

/**
  * Captures a data record used to initialise feature level data bindings
  * 
  *  @param dataFilePath the path to the source data file
  *  @param recordNo the current data record number
  *  @param data the current data  
  */
class DataRecord(val dataFilePath: String, val recordNo: Int, val data: List[(String, String)]) {
  override def toString = s"DataRecord($dataFilePath[$recordNo])"
}

