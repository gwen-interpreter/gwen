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
import gwen.core.node.FeatureUnit
import gwen.core.node.gherkin.Spec
import gwen.core.node.gherkin.SpecType
import gwen.core.node.event.NodeEventDispatcher
import gwen.core.state.DataRecord
import gwen.core.status._

import gwen.core.report.html.HtmlReportConfig
import gwen.core.report.html.HtmlSlideshowConfig
import gwen.core.report.json.JsonReportConfig
import gwen.core.report.junit.JUnitReportConfig
import gwen.core.result.ResultsSummary
import gwen.core.result.SpecResult

import scala.io.Source
import scala.util.chaining._

import com.typesafe.scalalogging.LazyLogging

import java.io.BufferedInputStream
import java.io.File
import java.text.SimpleDateFormat
import java.util.Date

/**
  * Base class for report generators.
  *
  * @author Branko Juric
  */
class ReportGenerator (
    config: ReportConfig,
    options: GwenOptions) extends LazyLogging {
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
  def close(lifecycle: NodeEventDispatcher, evalStatus: EvalStatus): Unit = { }

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
        formatDetail(options, unit, metaResult, breadcrumbs, reportFile :: Nil) map { content =>
          reportFile tap { file =>
            file.writeText(content)
            logger.info(s"${config.name} meta detail report generated: ${file.getAbsolutePath}")
          }
        }
      }
    }
  }

  private final def reportFeatureDetail(unit: FeatureUnit, result: SpecResult, reportFiles: List[File]): Option[File] = {
    reportFiles.headOption flatMap { reportFile =>
      formatDetail(options, unit, result, summaryReportFile.map(f => List(("Summary", f))).getOrElse(Nil), reportFiles) map { content =>
        reportFile tap { file =>
          file.writeText(content)
          reportAttachments(result.spec, file)
          logger.info(s"${config.name} feature detail report generated: ${file.getAbsolutePath}")
        }
      }
    }
  }

  def reportAttachments(spec: Spec, featureReportFile: File): Unit = {
    val attachmentsDir = new File(featureReportFile.getParentFile, "attachments")
    spec.attachments foreach { case (_, file) =>
      new File(attachmentsDir, file.getName).writeFile(file)
    }
  }

  /**
    * Must be implemented to generate and return a summary report file.
    *
    * @param summary the feature summary to report
    */
  final def reportSummary(summary: ResultsSummary): Option[File] =
    if (summary.results.nonEmpty) {
      summaryReportFile tap { reportFile =>
        reportFile foreach { file =>
          formatSummary(options, summary) foreach { content =>
            file.writeText(content)
            logger.info(s"${config.name} feature summary report generated: ${file.getAbsolutePath}")
          }
        }
      }
    } else {
      None
    }

}

object ReportGenerator {

  def generatorsFor(options: GwenOptions): List[ReportGenerator] = {
    options.reportDir foreach { dir =>
      if (dir.exists) {
        if (GwenSettings.`gwen.report.overwrite`) {
          dir.deleteDir()
        } else {
          dir.renameTo(new File(s"${dir.getAbsolutePath}-${new SimpleDateFormat("yyyyMMdd-HHmmss").format(new Date())}"))
        }
      }
      dir.mkdirs()
    }
    val formats =
      if (options.reportFormats.contains(ReportFormat.html))
        ReportFormat.slideshow :: options.reportFormats
      else options.reportFormats

    formats.flatMap { format =>
      format match {
        case ReportFormat.html => Some(HtmlReportConfig)
        case ReportFormat.slideshow => Some(HtmlSlideshowConfig)
        case ReportFormat.junit => Some(JUnitReportConfig)
        case ReportFormat.json => Some(JsonReportConfig)
     }
    } map { config =>
      config.reportGenerator(options)
    }

  }

  def encodeDataRecordNo(dataRecord: Option[DataRecord]): String = dataRecord.map(record => s"${Formatting.padWithZeroes(record.recordNo)}-").getOrElse("")

}
