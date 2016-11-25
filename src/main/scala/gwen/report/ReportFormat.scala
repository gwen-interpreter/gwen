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

import scala.language.implicitConversions
import scala.language.postfixOps
import gwen.eval.GwenOptions
import java.io.File
import gwen.dsl.FeatureSpec
import gwen.eval.DataRecord
import gwen.Predefs.FileIO

/**
  * Enumeration of supported report formats.
  * 
  * @author Branko Juric
  */
object ReportFormat extends Enumeration {

  val html, slideshow, junit = Value
  
  class FormatValue(
    val name: String, 
    val fileExtension: String, 
    val summaryFilename: Option[String], 
    val getGenerator: GwenOptions => ReportGenerator, 
    val getReportDir: GwenOptions => File,
    val getReportDetailFilename: (FeatureSpec, Option[DataRecord]) => String) {
      def reportGenerator(options: GwenOptions): ReportGenerator = getGenerator(options)
      def reportDir(options: GwenOptions): File = getReportDir(options)
      def getReportFilename(spec: FeatureSpec, dataRecord: Option[DataRecord]) = getReportDetailFilename(spec, dataRecord)
      def createReportDir(options: GwenOptions, spec: FeatureSpec, dataRecord: Option[DataRecord]): File = {
        val reportDir = getReportDir(options)
        val dataRecordDir = ReportGenerator.encodeDataRecordNo(dataRecord)
        val reportPath = spec.featureFile match {
          case Some(file) =>
            file.toPath(reportDir, Some(dataRecordDir + FileIO.encodeDir(file.getName().substring(0, file.getName().lastIndexOf(".")))))
          case None => 
            reportDir.getPath() + File.separator + dataRecordDir + FileIO.encodeDir(spec.feature.name)
        }
        new File(reportPath)
      }
      def createReportFile(toDir: File, prefix: String, spec: FeatureSpec, dataRecord: Option[DataRecord]): File =
        new File(toDir, s"${prefix}${getReportFilename(spec, dataRecord)}.${fileExtension}")
    }
  
  private val Values = Map(
    html -> new FormatValue(
        "HTML", 
        "html", 
        Some("feature-summary"), 
        options => new HtmlReportGenerator(options), 
        options => options.reportDir.map(dir => new File(dir, "html")).get,
        (spec: FeatureSpec, dataRecord: Option[DataRecord]) =>
          spec.featureFile.map(_.getName).getOrElse(spec.feature.name)),
    slideshow -> new FormatValue(
        "Slideshow", 
        "html", 
        None, 
        options => new HtmlSlideshowGenerator(options), 
        options => options.reportDir.map(dir => new File(dir, "html")).get,
        (spec: FeatureSpec, dataRecord: Option[DataRecord]) =>
          s"${spec.featureFile.map(_.getName).getOrElse(spec.feature.name)}.slideshow"),
    junit -> new FormatValue(
        "JUnit-XML", 
        "xml", 
        None, 
        options => new JUnitReportGenerator(options), 
        options => options.reportDir.map(dir => new File(dir, "junit")).get,
        (spec: FeatureSpec, dataRecord: Option[DataRecord]) => {
          val parentDirPath = spec.featureFile.flatMap(f => Option(f.getParentFile)).map(_.getPath).getOrElse("")
          val dataRecNo = ReportGenerator.encodeDataRecordNo(dataRecord)
          s"TEST-${FileIO.encodeDir(parentDirPath)}-${dataRecNo}${spec.featureFile.map(_.getName).getOrElse(spec.feature.name)}"
        }) {
          override def createReportDir(options: GwenOptions, spec: FeatureSpec, dataRecord: Option[DataRecord]): File = getReportDir(options)
        }
  )
  
  implicit def value2ReportFormat(value: Value) = Values(value)
  
}

