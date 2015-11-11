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
    private val summaryFilename: Option[String],
    val reportDir: File) extends LazyLogging {
  formatter: ReportFormatter => 
    
  private val summaryReportFile = summaryFilename.map(name => new File(reportDir, s"$name.$formatExtension"))
  
  if (reportDir.exists) {
    reportDir.renameTo(new File(s"${reportDir.getAbsolutePath()}-${System.currentTimeMillis()}"))
  }
  Path(reportDir).createDirectory()
    
  /**
    * Generate and return a detail feature report.
    * 
    * @param info the gwen implementation info
    * @param specs the list of evaluated specs (head = feature spec, tail = meta specs)
    * @param dataRecord optional data record
    * @return the report file
    */
  final def reportDetail(info: GwenInfo, specs: List[FeatureSpec], dataRecord: Option[DataRecord]): Option[(String, File)] = {
    val (featureSpec::metaSpecs) = specs
    val reportFile = createReportFile(createReportDir(featureSpec, dataRecord), "", featureSpec, dataRecord) tap { file =>
      reportFeatureDetail(info, featureSpec, file, reportMetaDetail(info, metaSpecs, file, dataRecord))
    }
    Some((formatName, reportFile))
  }
  
  private[report] def reportMetaDetail(info: GwenInfo, metaSpecs: List[FeatureSpec], featureReportFile: File, dataRecord: Option[DataRecord]): List[FeatureResult] = {
    metaSpecs.zipWithIndex map { case (metaspec, idx) =>
      val prefix = s"${encodeNo(idx + 1)}-"
      val file = createReportFile(new File(Path(featureReportFile.getParentFile() + File.separator + "meta").createDirectory().path), prefix, metaspec, dataRecord) 
      logger.info(s"Generating $formatName meta detail report [${metaspec.feature.name}]..")
      FeatureResult(metaspec, Some(Map(formatName -> file)), Nil) tap { metaResult =>
        val featureCrumb = ("Feature", featureReportFile)
        val breadcrumbs = summaryReportFile.map(f => List(("Summary", f), featureCrumb)).getOrElse(List(featureCrumb))
        file.writeText(
          formatDetail(options, info, metaResult, breadcrumbs))
        logger.info(s"$formatName meta detail report generated: ${file.getAbsolutePath()}")
      }
    }
  }
  
  private final def reportFeatureDetail(info: GwenInfo, spec: FeatureSpec, featureReportFile: File, metaResults: List[FeatureResult]) { 
    logger.info(s"Generating $formatName feature detail report [${spec.feature.name}]..")
    FeatureResult(spec, Some(Map(formatName -> featureReportFile)), metaResults) tap { featureResult =>
      featureReportFile.writeText(
        formatDetail(
          options,
          info, 
          featureResult, 
          summaryReportFile.map(f => List(("Summary", f))).getOrElse(Nil)))
      reportAttachments(spec, featureReportFile)
      logger.info(s"$formatName feature detail report generated: ${featureReportFile.getAbsolutePath()}")
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
      summaryReportFile tap { reportFile =>
        reportFile foreach { file =>
          formatSummary(options, info, summary) foreach { content =>
            logger.info(s"Generating $formatName feature summary report..")
            file.writeText(content)
            logger.info(s"$formatName feature summary report generated: ${file.getAbsolutePath()}")
          }
        }
      }
    } else {
      None
    }
   
  private[report] def createReportDir(spec: FeatureSpec, dataRecord: Option[DataRecord]): File = 
    new File(Path(createReportPath(spec, dataRecord)).createDirectory().path)
  
  private[report] def createReportPath(spec: FeatureSpec, dataRecord: Option[DataRecord]): String = {
    val dataRecordDir = encodeDataRecordNo(dataRecord)
    spec.featureFile match {
      case Some(file) =>
        file.toPath(reportDir, Some(dataRecordDir + FileIO.encodeDir(file.getName().substring(0, file.getName().lastIndexOf(".")))))
      case None => 
        reportDir.getPath() + File.separator + dataRecordDir + FileIO.encodeDir(spec.feature.name)
    }
  }
  
  private def createReportFile(toDir: File, prefix: String, spec: FeatureSpec, dataRecord: Option[DataRecord]): File =
    new File(toDir, s"${prefix}${createReportFileName(spec, dataRecord)}.${formatExtension}")
  
  private[report] def createReportFileName(spec: FeatureSpec, dataRecord: Option[DataRecord]): String = spec.featureFile match {
    case Some(file) =>
      s"${file.getName()}"
    case None => 
      s"${spec.feature.name}"
  }
  
  private[report] def encodeDataRecordNo(dataRecord: Option[DataRecord]) = 
    dataRecord.map(record => s"${encodeNo(record.recordNo)}-").getOrElse("")
  
  private[report] def encodeNo(num: Int) = "%04d".format(num)
  
  private[report] def copyClasspathTextResourceToFile(resource: String, targetDir: File) = 
    new File(targetDir, new File(resource).getName) tap { file =>
      file.writeText(Source.fromInputStream(getClass().getResourceAsStream(resource)).mkString)
    }
  
  private[report] def copyClasspathBinaryResourceToFile(resource: String, targetDir: File) = 
    new File(targetDir, new File(resource).getName) tap { file =>
      file.writeBinary(new BufferedInputStream(getClass().getResourceAsStream(resource)))
    }
  
}
