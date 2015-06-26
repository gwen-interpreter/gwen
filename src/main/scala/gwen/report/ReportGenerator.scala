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
import gwen.Predefs.FileIO
import gwen.Predefs.Kestrel
import gwen.dsl.FeatureSpec
import gwen.eval.FeatureResult
import gwen.eval.FeatureSummary
import gwen.GwenInfo
import gwen.eval.GwenOptions
import com.typesafe.scalalogging.slf4j.LazyLogging

/**
  * Base class for report generators.
  * 
  * @author Branko Juric
  */
class ReportGenerator (
    private val options: GwenOptions, 
    private val summaryFilePrefix: String, 
    private val fileExtension: String) extends LazyLogging {
  formatter: ReportFormatter => 
    
  private val summaryReportFile = new File(reportDir, s"${summaryFilePrefix}.${fileExtension}")
  
  /** Lazily creates and returns the target report directory. */
  private[report] lazy val reportDir = {
    val targetDir = options.reportDir.getOrElse(new File("target/reports"))
    if (targetDir.exists) {
      targetDir.renameTo(new File(s"${targetDir.getAbsolutePath()}-${System.currentTimeMillis()}"))
    }
    new File(Path(targetDir).createDirectory().path)
  }
  
  /**
    * Must be implemented to generate and return a detail feature report.
    * 
    * @param info the gwen implementation info
    * @param specs the list of evaluated specs (head = feature spec, tail = meta specs)
    * @return the reported feature result
    */
  final def reportDetail(info: GwenInfo, specs: List[FeatureSpec]): FeatureResult = {
    val (featureSpec::metaSpecs) = specs
    val featureReportFile = createReportFile(createReportDir(reportDir, featureSpec), "", featureSpec)
    val metaResults = metaSpecs.zipWithIndex map { case (res, idx) => 
      (reportMetaDetail(info, res, featureReportFile, s"${"%04d".format(idx + 1)}-")) 
    }
    reportFeatureDetail(info, featureSpec, featureReportFile, metaResults)
  }
  
  private final def reportMetaDetail(info: GwenInfo, metaspec: FeatureSpec, featureReportFile: File, prefix: String): FeatureResult = {
    val file = createReportFile(new File(Path(featureReportFile.getParentFile() + File.separator + "meta").createDirectory().path), prefix, metaspec) 
    logger.info(s"Generating meta detail report [${metaspec.feature.name}]..")
    new FeatureResult(metaspec, Nil, Some(file)) tap { metaResult =>
      file.writeText(
        formatDetail(
          options,
          info, 
          metaResult, 
          List(("Summary", summaryReportFile), ("Feature", featureReportFile))))
      logger.info(s"Meta detail report generated: ${file.getAbsolutePath()}")
    }
  }
  
  private final def reportFeatureDetail(info: GwenInfo, spec: FeatureSpec, featureReportFile: File, metaResults: List[FeatureResult]): FeatureResult = { 
    logger.info(s"Generating feature detail report [${spec.feature.name}]..")
    new FeatureResult(spec, metaResults, Some(featureReportFile)) tap { featureResult =>
      featureReportFile.writeText(
        formatDetail(
          options,
          info, 
          featureResult, 
          List(("Summary", summaryReportFile))))
      val attachmentsDir = new File(Path(new File(featureReportFile.getParentFile(), "attachments")).createDirectory().path)
      spec.scenarios.flatMap(_.steps).flatMap(_.attachments ) foreach { case (_, file) =>
        new File(attachmentsDir, file.getName()).writeFile(file)
      }
      logger.info(s"Feature detail report generated: ${featureReportFile.getAbsolutePath()}")
    }
  }
  
  /**
    * Must be implemented to generate and return a summary report file.
    * 
    * @param info the gwen info
    * @param summary the feature summary to report
    */
  final def reportSummary(info: GwenInfo, summary: FeatureSummary): Option[File] =
    if (!summary.featureResults.map(_.report).isEmpty) {
        logger.info(s"Generating feature summary report..")
        summaryReportFile.writeText(formatSummary(options, info, summary))
        logger.info(s"Feature summary report generated: ${summaryReportFile.getAbsolutePath()}")
        Some(summaryReportFile)
    } else {
      None
    }
   
  private def createReportDir(baseDir: File, spec: FeatureSpec): File = spec.featureFile match {
    case Some(file) =>
      new File(Path(baseDir.getPath() + File.separator + encodeDir(file.getParent()) + File.separator + encodeDir(file.getName().substring(0, file.getName().lastIndexOf(".")))).createDirectory().path)
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
