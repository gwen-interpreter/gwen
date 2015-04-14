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
import gwen.Predefs.FileIO
import gwen.Predefs.Kestrel
import gwen.dsl.FeatureSpec
import gwen.eval.FeatureResult
import gwen.eval.FeatureSummary
import gwen.GwenInfo

/**
  * Base class for report generators.
  * 
  * @author Branko Juric
  */
class ReportGenerator (
    private val targetDir: File, 
    private val summaryFilePrefix: String, 
    private val fileExtension: String) extends LazyLogging {
  formatter: ReportFormatter => 
    
  private val summaryFileName = s"${summaryFilePrefix}.${fileExtension}"
  
  /** Lazily creates and returns the target report directory. */
  private[report] lazy val reportDir = {
    if (targetDir.exists) {
      targetDir.renameTo(new File(s"${targetDir.getAbsolutePath()}-${System.currentTimeMillis()}"))
    }
    new File(Path(targetDir).createDirectory().path)
  }
  
  /**
    * Must be implemented to generate and return a detail feature report.
    * 
    * @param spec the feature spec to report
    */
  final def reportDetail(info: GwenInfo, result: FeatureResult): File = {
    val featureReportFile = createReportFile(createReportDir(reportDir, result.spec), "", result.spec)
    val metaReportFiles = result.metaResults.zipWithIndex map { case (res, idx) => 
      (reportMetaDetail(info, res, featureReportFile, s"${createReportFileName(result.spec)}.${idx + 1}.")) 
    }
    reportFeatureDetail(info, result, featureReportFile, metaReportFiles)
  }
  
  private final def reportMetaDetail(info: GwenInfo, result: FeatureResult, featureReportFile: File, prefix: String): File =
    createReportFile(featureReportFile.getParentFile(), prefix, result.spec) tap { file => 
      logger.info(s"Generating meta detail report [${result.spec.feature.name}]..")
      file.writeText(
        formatDetail(
          info, 
          result, 
          Map(), 
          List(("Summary", s"../${summaryFileName}"), ("Feature", featureReportFile.getName()))))
      logger.info(s"Meta detail report generated: ${file.getAbsolutePath()}")
    }
  
  private final def reportFeatureDetail(info: GwenInfo, result: FeatureResult, featureReportFile: File, metaReportFiles: List[File]): File = 
    featureReportFile tap { file =>
      val spec = result.spec
      logger.info(s"Generating feature detail report [${spec.feature.name}]..")
      file.writeText(
        formatDetail(
          info, 
          result, 
          (result.metaResults zip metaReportFiles).toMap, 
          List(("Summary", s"../${summaryFileName}"))))
      val attachmentsDir = new File(Path(new File(featureReportFile.getParentFile(), "attachments")).createDirectory().path)
      spec.scenarios.flatMap(_.steps).flatMap(_.attachments ) foreach { case (_, file) =>
        new File(attachmentsDir, file.getName()).writeFile(file)
      }
      logger.info(s"Feature detail report generated: ${file.getAbsolutePath()}")
    }
  
  /**
    * Must be implemented to generate and return a summary report file.
    * 
    * @param summary the feature summary to report
    * @param featureReportFiles list of feature reports
    */
  final def reportSummary(info: GwenInfo, summary: FeatureSummary, featureReportFiles: List[File]): Option[File] =
    if (!summary.featureResults.isEmpty) {
      Some(new File(reportDir, summaryFileName) tap { file =>
        logger.info(s"Generating feature summary report..")
        file.writeText(
          formatSummary(
            info,
            summary,
            (summary.featureResults zip featureReportFiles).toMap))
        logger.info(s"Feature summary report generated: ${file.getAbsolutePath()}")
      })
    } else {
      None
    }
   
  private def createReportDir(baseDir: File, spec: FeatureSpec): File = spec.featureFile match {
    case Some(file) =>
      new File(Path(baseDir.getPath() + File.separator + encodeDir(file.getParent())).createDirectory().path)
    case None => 
      new File(Path(baseDir.getPath() + File.separator + encodeDir(spec.feature.name)).createDirectory().path)
  }
  
  private def createReportFile(toDir: File, prefix: String, spec: FeatureSpec): File = 
    new File(toDir, s"${prefix}${createReportFileName(spec)}.${fileExtension}")
  
  private def createReportFileName(spec: FeatureSpec): String = spec.featureFile match {
    case Some(file) =>
      s"${file.getName()}"
    case None => 
      s"${spec.feature.name}.${fileExtension}"
  }
  
  private def encodeDir(dirpath: String): String = 
    if (dirpath != null) dirpath.replaceAll("[/\\:\\\\]", "-") else "";
  
  private[report] def copyClasspathTextResourceToFile(resource: String, targetDir: File) = 
    new File(targetDir, new File(resource).getName) tap { file =>
      file.writeText(Source.fromInputStream(getClass().getResourceAsStream(resource)).mkString)
    }
  
  private[report] def copyClasspathBinaryResourceToFile(resource: String, targetDir: File) = 
    new File(targetDir, new File(resource).getName) tap { file =>
      file.writeBinary(new BufferedInputStream(getClass().getResourceAsStream(resource)))
    }
  
}
