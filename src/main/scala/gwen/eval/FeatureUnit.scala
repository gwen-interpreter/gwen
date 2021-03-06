/*
 * Copyright 2014-2015 Branko Juric, Brady Wood
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

package gwen.eval

import gwen._
import gwen.dsl.Identifiable
import gwen.dsl.NodeType

import java.io.File
import gwen.dsl.Root

/**
  * Captures a feature file and its associated meta as a unit.
  * 
  * @param featureFile the feature file
  * @param metaFiles the associated meta files (if any)
  * @param dataRecord optional data record
  * @param result option result
  */
case class FeatureUnit(
    parent: Identifiable,
    featureFile: File,
    metaFiles: List[File], 
    dataRecord: Option[DataRecord], 
    result: Option[FeatureResult] = None) extends Identifiable {

  override def nodeType: NodeType.Value = NodeType.Unit

  val uri: String = featureFile.uri
  val name: String = s"$uri${dataRecord.map(rec => s"[${rec.recordNo}]").getOrElse("")}"
  
  def ancestor: Identifiable = parent match {
    case parentUnit @ FeatureUnit(grandparent, _, _, _, _) => 
      if (grandparent == Root) parent
      else parentUnit.ancestor
    case _ => this
  }
}

object FeatureUnit {
  def apply(parent: Identifiable, unit: FeatureUnit, result: FeatureResult): FeatureUnit = {
    FeatureUnit(parent, unit.featureFile, unit.metaFiles, unit.dataRecord, Some(result))
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

