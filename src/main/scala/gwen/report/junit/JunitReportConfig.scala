/*
 * Copyright 2020-2021 Branko Juric, Brady Wood
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

package gwen.report.junit

import gwen.FileIO
import gwen.GwenOptions
import gwen.model.DataRecord
import gwen.model.gherkin.Specification
import gwen.report.ReportConfig
import gwen.report.ReportFormat
import gwen.report.ReportGenerator

import java.io.File

object JUnitReportConfig extends ReportConfig(
  ReportFormat.junit,
  "JUnit-XML", 
  Some("xml"), 
  None, 
  options => new JUnitReportGenerator(options), 
  options => options.reportDir.map(dir => new File(dir, "junit")),
  (spec: Specification, dataRecord: Option[DataRecord]) => {
    val parentDirPath = spec.featureFile.flatMap(f => Option(f.getParentFile)).map(_.getPath).getOrElse("")
    val dataRecNo = ReportGenerator.encodeDataRecordNo(dataRecord)
    Some(s"TEST-${FileIO.encodeDir(parentDirPath)}-$dataRecNo${spec.featureFile.map(_.getName).getOrElse(spec.feature.name)}")
  }) {
    override def createReportDir(options: GwenOptions, spec: Specification, dataRecord: Option[DataRecord]): Option[File] = getReportDir(options)
}
