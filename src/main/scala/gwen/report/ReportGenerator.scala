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

package gwen.report

import java.io.BufferedInputStream
import java.io.File
import scala.io.Source
import com.typesafe.scalalogging.slf4j.LazyLogging
import gwen.GwenInfo
import gwen.Predefs.FileIO
import gwen.Predefs.Kestrel
import gwen.dsl.FeatureSpec
import gwen.eval.FeatureResult
import gwen.eval.FeatureSummary
import gwen.eval.GwenOptions
import gwen.eval.DataRecord
import gwen.Predefs.FileIO
import gwen.eval.FeatureUnit
import gwen.Predefs.Formatting
import gwen.GwenSettings
import java.text.SimpleDateFormat
import java.util.Date

/**
  * Base class for report generators.
  * 
  * @author Branko Juric
  */
class ReportGenerator (
    val reportFormat: ReportFormat.Value,
    private val options: GwenOptions) extends LazyLogging {
  formatter: ReportFormatter => 

  private[report] def reportDir = reportFormat.reportDir(options) tap { dir =>
    if (!dir.exists) {
      dir.mkdirs()
    }
  }
  
  private val summaryReportFile = reportFormat.summaryFilename.map(name => new File(reportDir, s"$name.${reportFormat.fileExtension}"))
    
  /**
    * Generate and return a detail feature report.
    * 
    * @param info the gwen implementation info
    * @param result the evaluated feature result
    * @param dataRecord optional data record
    * @return the list of report files (head = feature report, tail = meta reports)
    */
  final def reportDetail(info: GwenInfo, unit: FeatureUnit, result: FeatureResult): List[File] = {
    val featureSpec = result.spec
    val dataRecord = unit.dataRecord
    val featureReportFile = reportFormat.createReportFile(reportFormat.createReportDir(options, featureSpec, dataRecord), "", featureSpec, dataRecord)
    val metaReportFiles = result.metaResults.zipWithIndex map { case (metaResult, idx) =>
      val metaspec = metaResult.spec
      val prefix = s"${Formatting.padWithZeroes(idx + 1)}-"
      reportFormat.createReportFile(new File(featureReportFile.getParentFile() + File.separator + "meta"), prefix, metaspec, unit.dataRecord)
    }
    val reportFiles = featureReportFile :: metaReportFiles
    reportFeatureDetail(info, unit, result, reportFiles).map(file => file :: reportMetaDetail(info, unit, result.metaResults, reportFiles)).getOrElse(Nil)
  }
  
  private[report] def reportMetaDetail(info: GwenInfo, unit: FeatureUnit, metaResults: List[FeatureResult], reportFiles: List[File]): List[File] = {
    if (GwenSettings.`gwen.report.suppress.meta`) {
      Nil
    } else {
      metaResults.zipWithIndex flatMap { case (metaResult, idx) =>
        val featureCrumb = ("Feature", reportFiles.head)
        val breadcrumbs = summaryReportFile.map(f => List(("Summary", f), featureCrumb)).getOrElse(List(featureCrumb))
        val reportFile = reportFiles.tail(idx)
        formatDetail(options, info, unit, metaResult, breadcrumbs, reportFile :: Nil) map { content => 
          reportFile tap { file =>
            file.writeText(content) 
            logger.info(s"${reportFormat.name} meta detail report generated: ${file.getAbsolutePath()}")
          }
        }
      }
    }
  }
  
  private final def reportFeatureDetail(info: GwenInfo, unit: FeatureUnit, result: FeatureResult, reportFiles: List[File]): Option[File] = {
    val featureSpec = result.spec
    val dataRecord = unit.dataRecord
    val reportFile = reportFiles.head
    formatDetail(options, info, unit, result, summaryReportFile.map(f => List(("Summary", f))).getOrElse(Nil), reportFiles) map { content =>
      reportFile tap { file =>
        file.writeText(content)
        reportAttachments(result.spec, file)
        logger.info(s"${reportFormat.name} feature detail report generated: ${file.getAbsolutePath()}")
      }
    }
  }
  
  def reportAttachments(spec: FeatureSpec, featureReportFile: File): Unit = {
    val attachmentsDir = new File(featureReportFile.getParentFile(), "attachments")
    spec.scenarios.flatMap(_.steps).flatMap(_.attachments ) foreach { case (_, file) =>
      new File(attachmentsDir, file.getName()).writeFile(file)
    }
  }
  
  /**
    * Must be implemented to generate and return a summary report file.
    * 
    * @param info the gwen info
    * @param summary the feature summary to report
    */
  final def reportSummary(info: GwenInfo, summary: FeatureSummary): Option[File] =
    if (summary.results.nonEmpty) {
      summaryReportFile tap { reportFile =>
        reportFile foreach { file =>
          formatSummary(options, info, summary) foreach { content =>
            file.writeText(content)
            logger.info(s"${reportFormat.name} feature summary report generated: ${file.getAbsolutePath()}")
          }
        }
      }
    } else {
      None
    }
   
  private[report] def copyClasspathTextResourceToFile(resource: String, targetDir: File) = 
    new File(targetDir, new File(resource).getName) tap { file =>
      file.writeText(Source.fromInputStream(getClass().getResourceAsStream(resource)).mkString)
    }
  
  private[report] def copyClasspathBinaryResourceToFile(resource: String, targetDir: File) = 
    new File(targetDir, new File(resource).getName) tap { file =>
      file.writeBinary(new BufferedInputStream(getClass().getResourceAsStream(resource)))
    }
  
}

object ReportGenerator {
  
  def generatorsFor(options: GwenOptions): List[ReportGenerator] = {
    options.reportDir foreach { dir =>
      if (dir.exists) {
        if (GwenSettings.`gwen.report.overwrite`) {
          dir.deleteDir()
        } else {
          dir.renameTo(new File(s"${dir.getAbsolutePath()}-${new SimpleDateFormat("yyyyMMdd-HHmmss").format(new Date())}"))
        }
      }
      dir.mkdirs()
    }
    val formats = 
      if (options.reportFormats.contains(ReportFormat.html)) 
        ReportFormat.slideshow :: options.reportFormats 
      else options.reportFormats
    options.reportDir.map(_ => formats.map(_.reportGenerator(options))).getOrElse(Nil)
  }
  
  def encodeDataRecordNo(dataRecord: Option[DataRecord]) = dataRecord.map(record => s"${Formatting.padWithZeroes(record.recordNo)}-").getOrElse("")
  
}
