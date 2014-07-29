/*
 * Copyright 2014 Branko Juric, Brady Wood
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

/**
 * Base class for report generators.
 * 
 * @author Branko Juric
 */
class ReportGenerator (
    private val targetDir: File, 
    private val interpreterName: String, 
    private val summaryFilePrefix: String, 
    private val fileExtension: String) extends LazyLogging {
  formatter: ReportFormatter => 
    
  private val summaryFileName = s"${summaryFilePrefix}.${fileExtension}"
  
  /**
   * Lazily creates and returns the target report directory.
   */
  private[report] lazy val reportDir = new File(Path(targetDir).createDirectory().path)
  
  /**
   * Lazily creates and returns the target report attachments directory.
   */
  private[report] lazy val attachmentsDir = new File(Path(new File(targetDir, "attachments")).createDirectory().path)

  /**
   * Must be implemented to generate and return a detail feature report.
   * 
   * @param spec
   * 			the feature spec to report
   */
  final def reportDetail(spec: FeatureSpec): File =
    new File(reportDir, s"${getNamePrefix(spec)}.${fileExtension}") tap { featureReportFile =>
      spec.metaSpecs.zipWithIndex map { case (meta, idx) =>
        new File(reportDir, s"${getNamePrefix(spec)}.${idx + 1}.meta.${fileExtension}") tap { metaReportFile =>
          reportMetaDetail(featureReportFile, meta, metaReportFile)
        }
      } tap { metaReportFiles =>
        reportFeatureDetail(spec, featureReportFile, metaReportFiles)
      }
    }
  
  private final def reportFeatureDetail(spec: FeatureSpec, featureReportFile: File, metaReportFiles: List[File]) { 
    logger.info(s"Generating feature detail report [${spec.feature.name}]..")
    featureReportFile.writeText(formatDetail(spec, interpreterName, List(("Summary", new File(summaryFileName))), metaReportFiles))
    spec.scenarios.flatMap(_.steps).flatMap(_.attachments ) foreach { case (_, file) =>
      new File(attachmentsDir, file.getName()).writeFile(file)
    }
    logger.info(s"Feature detail report generated: ${featureReportFile.getAbsolutePath()}")
  }
  
  private final def reportMetaDetail(featureReportFile: File, metaFeature: FeatureSpec, metaReportFile: File) { 
    logger.info(s"Generating meta detail report [${metaFeature.feature.name}]..")
    metaReportFile.writeText(formatDetail(metaFeature, interpreterName, List(("Summary", new File(summaryFileName)), ("Feature", featureReportFile)), Nil))
    logger.info(s"Meta detail report generated: ${metaReportFile.getAbsolutePath()}")
  }
  
  /**
   * Must be implemented to generate and return a summary report file.
   * 
   * @param featureSummary
   * 			the feature summary to report
   */
  final def reportSummary(featureSummary: FeatureSummary): Option[File] =
    if (!featureSummary.featureResults.isEmpty) {
      Some(new File(reportDir, summaryFileName) tap { file =>
        logger.info(s"Generating feature summary report..")
	    file.writeText(formatSummary(featureSummary, interpreterName))
	    logger.info(s"Feature summary report generated: ${file.getAbsolutePath()}")
      })
    } else {
      None
    }
    
  private def getNamePrefix(spec: FeatureSpec): String = spec.featureFile match {
    case Some(file) => encodePath(file.getParent()) + file.getName()
    case None => spec.feature.name
  }
  
  private def encodePath(path: String): String = if (path != null) path.replaceAll("[/\\:\\\\]", "-") + "-" else "";
  
  private[report] def copyClasspathTextResourceToFile(resource: String, targetDir: File) = 
    new File(targetDir, new File(resource).getName) tap { file =>
      file.writeText(Source.fromInputStream(getClass().getResourceAsStream(resource)).mkString)
    }
  
  private[report] def copyClasspathBinaryResourceToFile(resource: String, targetDir: File) = 
    new File(targetDir, new File(resource).getName) tap { file =>
      file.writeBinary(new BufferedInputStream(getClass().getResourceAsStream(resource)))
    }
  
}
