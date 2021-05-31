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

package gwen.core.report

import gwen.core.FileIO
import gwen.core.GwenOptions
import gwen.core.model.DataRecord
import gwen.core.node.gherkin.Spec

import java.io.File

class ReportConfig(
  val format: ReportFormat.Value,
  val name: String,
  val fileExtension: Option[String],
  val summaryFilename: Option[String],
  val getGenerator: GwenOptions => ReportGenerator, 
  val getReportDir: GwenOptions => Option[File],
  val getReportDetailFilename: (Spec, Option[DataRecord]) => Option[String]) {

  def reportGenerator(options: GwenOptions): ReportGenerator = getGenerator(options)
  def reportDir(options: GwenOptions): Option[File] = getReportDir(options)
  def getReportFilename(spec: Spec, dataRecord: Option[DataRecord]): Option[String] = getReportDetailFilename(spec, dataRecord)
  def createReportDir(options: GwenOptions, spec: Spec, dataRecord: Option[DataRecord]): Option[File] = {
    val reportDir = getReportDir(options)
    val dataRecordDir = ReportGenerator.encodeDataRecordNo(dataRecord)
    val reportPath = spec.specFile match {
      case Some(file) =>
        reportDir map { dir =>
          file.toPath(dir, Some(dataRecordDir + FileIO.encodeDir(file.getName.substring(0, file.getName.lastIndexOf(".")))))
        }
      case None => 
        reportDir map { dir =>
          dir.getPath + File.separator + dataRecordDir + FileIO.encodeDir(spec.feature.name)
        }
    }
    reportPath map { path => new File(path) }
  }
  def createReportFile(toDir: File, prefix: String, spec: Spec, dataRecord: Option[DataRecord]): Option[File] = {
    fileExtension flatMap { ext => 
      getReportFilename(spec, dataRecord) map { filename =>
        new File(toDir, s"$prefix$filename.$ext")
      }
    }
  }

}

