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

package gwen.report

import gwen.dsl.FeatureSpec
import gwen.eval.DataRecord
import gwen.eval.GwenOptions
import gwen.FileIO

import java.io.File

class ReportConfig(
  val format: ReportFormat.Value,
  val name: String,
  val fileExtension: Option[String],
  val summaryFilename: Option[String],
  val getGenerator: GwenOptions => ReportGenerator, 
  val getReportDir: GwenOptions => Option[File],
  val getReportDetailFilename: (FeatureSpec, Option[DataRecord]) => Option[String]) {

  def reportGenerator(options: GwenOptions): ReportGenerator = getGenerator(options)
  def reportDir(options: GwenOptions): Option[File] = getReportDir(options)
  def getReportFilename(spec: FeatureSpec, dataRecord: Option[DataRecord]): Option[String] = getReportDetailFilename(spec, dataRecord)
  def createReportDir(options: GwenOptions, spec: FeatureSpec, dataRecord: Option[DataRecord]): Option[File] = {
    val reportDir = getReportDir(options)
    val dataRecordDir = ReportGenerator.encodeDataRecordNo(dataRecord)
    val reportPath = spec.featureFile match {
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
  def createReportFile(toDir: File, prefix: String, spec: FeatureSpec, dataRecord: Option[DataRecord]): Option[File] = {
    fileExtension flatMap { ext => 
      getReportFilename(spec, dataRecord) map { filename =>
        new File(toDir, s"$prefix$filename.$ext")
      }
    }
  }

}

object HtmlReportConfig extends ReportConfig(
  ReportFormat.html,
  "HTML", 
  Some("html"), 
  Some("feature-summary"), 
  options => new HtmlReportGenerator(options), 
  options => options.reportDir.map(dir => new File(dir, "html")),
  (spec: FeatureSpec, _) =>
    Some(spec.featureFile.map(_.getName).getOrElse(spec.feature.name)))

object HtmlSlideshowReportConfig extends ReportConfig(
  ReportFormat.slideshow,
  "Slideshow", 
  Some("html"), 
  None, 
  options => new HtmlSlideshowGenerator(options), 
  options => options.reportDir.map(dir => new File(dir, "html")),
  (spec: FeatureSpec, _) =>
    Some(s"${spec.featureFile.map(_.getName).getOrElse(spec.feature.name)}.slideshow"))

object JUnitReportConfig extends ReportConfig(
  ReportFormat.junit,
  "JUnit-XML", 
  Some("xml"), 
  None, 
  options => new JUnitReportGenerator(options), 
  options => options.reportDir.map(dir => new File(dir, "junit")),
  (spec: FeatureSpec, dataRecord: Option[DataRecord]) => {
    val parentDirPath = spec.featureFile.flatMap(f => Option(f.getParentFile)).map(_.getPath).getOrElse("")
    val dataRecNo = ReportGenerator.encodeDataRecordNo(dataRecord)
    Some(s"TEST-${FileIO.encodeDir(parentDirPath)}-$dataRecNo${spec.featureFile.map(_.getName).getOrElse(spec.feature.name)}")
  }) {
    override def createReportDir(options: GwenOptions, spec: FeatureSpec, dataRecord: Option[DataRecord]): Option[File] = getReportDir(options)
}

object JsonReportConfig extends ReportConfig(
  ReportFormat.json,
  "JSON",
  Some("json"),
  None,
  options => new JsonReportGenerator(options),
  options => options.reportDir.map(dir => new File(dir, "json")),
  (spec: FeatureSpec, dataRecord: Option[DataRecord]) => {
    val parentDirPath = spec.featureFile.flatMap(f => Option(f.getParentFile)).map(_.getPath).getOrElse("")
    val dataRecNo = ReportGenerator.encodeDataRecordNo(dataRecord)
    Some(s"${FileIO.encodeDir(parentDirPath)}-$dataRecNo${spec.featureFile.map(_.getName).getOrElse(spec.feature.name)}")
  }) {
  override def createReportDir(options: GwenOptions, spec: FeatureSpec, dataRecord: Option[DataRecord]): Option[File] = getReportDir(options)
}
