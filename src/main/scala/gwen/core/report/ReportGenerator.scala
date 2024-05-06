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

package gwen.core.report

import gwen.core._
import gwen.core.Errors.WaitTimeoutException
import gwen.core.data.DataRecord
import gwen.core.node.FeatureUnit
import gwen.core.node.gherkin.Spec
import gwen.core.node.gherkin.SpecType
import gwen.core.node.event.NodeEventDispatcher
import gwen.core.status._

import gwen.core.report.html.HtmlReportConfig
import gwen.core.report.html.HtmlSlideshowConfig
import gwen.core.report.json.JsonReportConfig
import gwen.core.report.junit.JUnitReportConfig
import gwen.core.report.rp.RPReportConfig
import gwen.core.result.ResultsSummary
import gwen.core.result.SpecResult

import scala.io.Source
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import scala.util.chaining._

import com.typesafe.scalalogging.LazyLogging

import java.io.BufferedInputStream
import java.io.File
import java.util.Date

/**
  * Base class for report generators.
  *
  * @author Branko Juric
  */
class ReportGenerator (
    config: ReportConfig,
    options: GwenOptions,
    info: GwenInfo) extends LazyLogging {
  formatter: ReportFormatter =>

  private [report] def reportDir: Option[File] = {
    config.reportDir(options) tap { dir =>
      dir.filter(!_.exists).foreach(_.mkdirs())
    }
  }

  private val summaryReportFile: Option[File] = {
    config.summaryFilename.flatMap { name =>
      reportDir flatMap { dir =>
        config.fileExtension map { ext =>
          new File(dir, s"$name.$ext")
        }
      }
    }
  }

  val format: ReportFormat = config.format

  def init(lifecycle: NodeEventDispatcher): Unit = { }

  def close(lifecycle: NodeEventDispatcher, evalStatus: EvalStatus): Option[String] = {
    summaryReportFile orElse {
      reportDir flatMap { dir => 
        if (format.isCliOption) {
          Some(dir)
        } else None
      }
    } filter { report => 
      if (report.isFile) report.exists
      else { 
        val contents = report.list
        contents != null && contents.length > 0
      }
     } map (_.getAbsolutePath)
  }

  /**
    * Generate and return a detail feature report.
    *
    * @param unit the feature unit
    * @param result the evaluated feature result
    * @return the list of report files (head = feature report, tail = meta reports)
    */
  final def reportDetail(unit: FeatureUnit, result: SpecResult): List[File] = {
    val featureSpec = result.spec
    val dataRecord = unit.dataRecord
    val featureReportFile = config.createReportDir(options, featureSpec, dataRecord) flatMap { dir =>
      config.createReportFile(dir, "", featureSpec, dataRecord)
    }
    val metaReportFiles = result.metaResults.zipWithIndex flatMap { case (metaResult, idx) =>
      val metaspec = metaResult.spec
      val prefix = s"${Formatting.padWithZeroes(idx + 1)}-"
      featureReportFile flatMap { reportFile =>
        config.createReportFile(new File(reportFile.getParentFile, "meta"), prefix, metaspec, unit.dataRecord)
      }
    }
    val reportFiles = featureReportFile.map(_ :: metaReportFiles).getOrElse(Nil)
    reportFeatureDetail(unit, result, reportFiles).map(file => file :: reportMetaDetail(unit, result.metaResults, reportFiles)).getOrElse(Nil)
  }

  private [report] def reportMetaDetail(unit: FeatureUnit, metaResults: List[SpecResult], reportFiles: List[File]): List[File] = {
    if (GwenSettings.`gwen.report.suppress.meta`) {
      Nil
    } else {
      metaResults.zipWithIndex flatMap { case (metaResult, idx) =>
        val featureCrumb = (SpecType.Feature.toString, reportFiles.head)
        val breadcrumbs = summaryReportFile.map(f => List(("Summary", f), featureCrumb)).getOrElse(List(featureCrumb))
        val reportFile = reportFiles.tail(idx)
        formatDetail(options, info, unit, metaResult, breadcrumbs, reportFile :: Nil) map { content =>
          reportFile tap { file =>
            file.writeText(content)
            logger.info(s"${config.name} meta detail${if (options.dryRun) " dry-run" else " evaluation"} report generated: ${file.getAbsolutePath}")
          }
        }
      }
    }
  }

  private final def reportFeatureDetail(unit: FeatureUnit, result: SpecResult, reportFiles: List[File]): Option[File] = {
    reportFiles.headOption flatMap { reportFile =>
      formatDetail(options, info, unit, result, summaryReportFile.map(f => List(("Summary", f))).getOrElse(Nil), reportFiles) map { content =>
        reportFile tap { file =>
          file.writeText(content)
          copyAttachments(result.spec, file)
          copyVideos(result, file)
          logger.info(s"${config.name} feature detail${if (options.dryRun) " dry-run" else " evaluation"} report generated: ${file.getAbsolutePath}")
        }
      }
    }
  }

  def copyAttachments(spec: Spec, featureReportFile: File): Unit = {
    val attachmentsDir = new File(featureReportFile.getParentFile, "attachments")
    spec.attachments foreach { case (_, file) =>
      file.copyToDir(attachmentsDir)
    }
  }

  def copyVideos(result: SpecResult, featureReportFile: File): Unit = {
    result.videos foreach { videoFile =>
      Try(Wait.waitUntil(GwenSettings.`gwen.video.timeoutSecs`, s"waiting for video file: $videoFile") { videoFile.exists }) match {
        case Success(_) => 
          val attachmentsDir = new File(featureReportFile.getParentFile, "attachments")
          val videoDir = new File(attachmentsDir, "videos")
          videoFile.copyToDir(videoDir)
          videoFile.deleteOnExit
        case Failure(e) => 
          if (e.isInstanceOf[WaitTimeoutException]) {
            logger.error(e.getMessage)
          } else throw e
      }
    }
  }

  /**
    * Must be implemented to generate and return a summary report file.
    *
    * @param summary the feature summary to report
    */
  final def reportSummary(summary: ResultsSummary): Option[File] =
    summaryReportFile tap { reportFile =>
      reportFile foreach { file =>
        formatSummary(options, info, summary) foreach { content =>
          file.writeText(content)
          logger.info(s"${config.name} feature summary${if (options.dryRun) " dry-run" else " evaluation"} report generated: ${file.getAbsolutePath}")
        }
      }
    }

}

object ReportGenerator {

  def generatorsFor(options: GwenOptions, info: GwenInfo): List[ReportGenerator] = {
    options.reportFormats.distinct match {
      case Nil  => Nil
      case head :: Nil if (head == ReportFormat.none) => Nil
      case reportFormats => 
        if (reportFormats.exists(_.isFileSystemReport)) {
          options.reportDir foreach { dir =>
            if (dir.exists) {
              if (GwenSettings.`gwen.report.overwrite`) {
                dir.deleteDir()
              } else {
                dir.renameTo(new File(s"${dir.getAbsolutePath}-${Formatting.formatDate(new Date(), "yyyyMMdd-HHmmss")}"))
              }
            }
            dir.mkdirs()
          }
        }
        var formats = 
          if (reportFormats.contains(ReportFormat.html))
            ReportFormat.slideshow :: reportFormats
          else reportFormats

        formats =
          if (options.dryRun && !formats.contains(ReportFormat.html))
            ReportFormat.html :: formats
          else formats

        formats.flatMap { format =>
          format match {
            case ReportFormat.html => Some(HtmlReportConfig)
            case ReportFormat.slideshow => Some(HtmlSlideshowConfig)
            case ReportFormat.junit => Some(JUnitReportConfig)
            case ReportFormat.json => Some(JsonReportConfig)
            case ReportFormat.rp => Some(RPReportConfig)
            case ReportFormat.none => None
          }
        } map { config =>
          config.reportGenerator(options, info)
        }
    }
  }

  def encodeDataRecordNo(dataRecord: Option[DataRecord]): String = dataRecord.map(record => s"${Formatting.padWithZeroes(record.recordNo)}-").getOrElse("")

}
