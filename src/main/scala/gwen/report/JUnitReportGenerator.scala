/*
 * Copyright 2015 Branko Juric, Brady Wood
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

package gwen.report

import java.io.File
import gwen.Predefs.FileIO
import gwen.GwenInfo
import gwen.eval.GwenOptions
import gwen.dsl.FeatureSpec
import gwen.eval.DataRecord
import gwen.eval.FeatureResult

/**
  * Generates JUnit xml report files (for integration will build servers 
  * that support the standand JUnit report syntax).
  * 
  * @author Branko Juric
  */
class JUnitReportGenerator(val options: GwenOptions) extends ReportGenerator(ReportFormat.junit, options) with JUnitReportFormatter {

  override def reportAttachments(spec: FeatureSpec, featureReportFile: File): Unit = {
    // noop
  }
  
  override def reportMetaDetail(info: GwenInfo, metaSpecs: List[FeatureSpec], featureReportFile: File, dataRecord: Option[DataRecord]): List[FeatureResult] = {
    metaSpecs.map(FeatureResult(_, None, Nil))
  }
  
  override def createReportDir(spec: FeatureSpec, dataRecord: Option[DataRecord]): File = reportDir
  
  override def createReportFileName(spec: FeatureSpec, dataRecord: Option[DataRecord]): String = { 
    val parentDirPath = spec.featureFile.map(_.getParentFile).map(_.getPath).getOrElse("")
    s"TEST-${FileIO.encodeDir(parentDirPath)}-${encodeDataRecordNo(dataRecord)}${super.createReportFileName(spec, dataRecord)}"
  }
  
}