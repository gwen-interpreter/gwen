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
import scala.reflect.io.Path
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

/**
  * Base class for report generators.
  * 
  * @author Branko Juric
  */
class ReportGenerator (
    private val options: GwenOptions, 
    private val summaryFilename: String, 
    private val fileExtension: String,
    private val reportSubDir: Option[String]) extends LazyLogging {
  formatter: ReportFormatter => 
    
  private val summaryReportFile = new File(reportDir, s"${summaryFilename}")
  
  /** Lazily creates and returns the target report directory. */
  private[report] lazy val reportDir = {
    val reportDir = options.reportDir.get 
    val targetDir = reportSubDir.map(new File(reportDir, _)).getOrElse(reportDir)
    if (targetDir.exists) {
      targetDir.renameTo(new File(s"${targetDir.getAbsolutePath()}-${System.currentTimeMillis()}"))
    }
    new File(Path(targetDir).createDirectory().path)
  }
  
  /**
    * Generate and return a detail feature report.
    * 
    * @param info the gwen implementation info
    * @param specs the list of evaluated specs (head = feature spec, tail = meta specs)
    * @param dataRecord optional data record
    * @return the report file
    */
  final def reportDetail(info: GwenInfo, specs: List[FeatureSpec], dataRecord: Option[DataRecord]): Option[File] = {
    val (featureSpec::metaSpecs) = specs
    val reportFile = createReportFile(createReportDir(reportDir, featureSpec, dataRecord), "", featureSpec) tap { file =>
      reportFeatureDetail(info, featureSpec, file, reportMetaDetail(info, metaSpecs, file))
    }
    Some(reportFile)
  }
  
  def reportMetaDetail(info: GwenInfo, metaSpecs: List[FeatureSpec], featureReportFile: File): List[FeatureResult] = {
    metaSpecs.zipWithIndex map { case (metaspec, idx) =>
      val prefix = s"${"%04d".format(idx + 1)}-"
      val file = createReportFile(new File(Path(featureReportFile.getParentFile() + File.separator + "meta").createDirectory().path), prefix, metaspec) 
      logger.info(s"Generating meta detail report [${metaspec.feature.name}]..")
      FeatureResult(metaspec, Some(Map(fileExtension -> file)), Nil) tap { metaResult =>
        file.writeText(
          formatDetail(
            options,
            info, 
            metaResult, 
            List(("Summary", summaryReportFile), ("Feature", featureReportFile))))
        logger.info(s"Meta detail report generated: ${file.getAbsolutePath()}")
      }
    }
  }
  
  private final def reportFeatureDetail(info: GwenInfo, spec: FeatureSpec, featureReportFile: File, metaResults: List[FeatureResult]) { 
    logger.info(s"Generating feature detail report [${spec.feature.name}]..")
    FeatureResult(spec, Some(Map(fileExtension -> featureReportFile)), metaResults) tap { featureResult =>
      featureReportFile.writeText(
        formatDetail(
          options,
          info, 
          featureResult, 
          List(("Summary", summaryReportFile))))
      reportAttachments(spec, featureReportFile)
      logger.info(s"Feature detail report generated: ${featureReportFile.getAbsolutePath()}")
    }
  }
  
  def reportAttachments(spec: FeatureSpec, featureReportFile: File): Unit = {
    val attachmentsDir = new File(Path(new File(featureReportFile.getParentFile(), "attachments")).createDirectory().path)
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
    if (summary.summaryLines.nonEmpty) {
      formatSummary(options, info, summary) map { content =>
        logger.info(s"Generating feature summary report..")
        summaryReportFile.writeText(content)
        logger.info(s"Feature summary report generated: ${summaryReportFile.getAbsolutePath()}")
        summaryReportFile
      }
    } else {
      None
    }
   
  private def createReportDir(baseDir: File, spec: FeatureSpec, dataRecord: Option[DataRecord]): File = {
    val dataRecordDir = dataRecord.map(record => s"${"%04d".format(record.recordNo)}-").getOrElse("")
    spec.featureFile match {
      case Some(file) =>
        file.toDir(baseDir, Some(dataRecordDir + FileIO.encodeDir(file.getName().substring(0, file.getName().lastIndexOf(".")))))
      case None => 
        new File(Path(baseDir.getPath() + File.separator + dataRecordDir + FileIO.encodeDir(spec.feature.name)).createDirectory().path)
    }
  }
  
  private def createReportFile(toDir: File, prefix: String, spec: FeatureSpec): File =
    new File(toDir, s"${prefix}${createReportFileName(spec)}.${fileExtension}")
  
  private def createReportFileName(spec: FeatureSpec): String = spec.featureFile match {
    case Some(file) =>
      s"${file.getName()}"
    case None => 
      s"${spec.feature.name}.${fileExtension}"
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
