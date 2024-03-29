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

import gwen.core.GwenInfo
import gwen.core.GwenOptions
import gwen.core.node.FeatureUnit
import gwen.core.result.ResultsSummary
import gwen.core.result.SpecResult

import java.io.File

/** Trait for formatting the feature summary and detail reports. */
trait ReportFormatter {

  /**
    * Formats the feature detail report.
    * 
    * @param options gwen command line options
    * @param unit the feature input
    * @param info gwen info
    * @param result the feature result
    * @param breadcrumbs names and references for linking back to parent reports
    * @param reportFiles the target report files (head = detail, tail = metas)
    */
  def formatDetail(options: GwenOptions, info: GwenInfo, unit: FeatureUnit, result: SpecResult, breadcrumbs: List[(String, File)], reportFiles: List[File]): Option[String] = None
  
  /**
    * Formats the feature summary report.
    * 
    * @param options gwen command line options
    * @param info gwen info
    * @param summary the accumulated feature results summary
    */
  def formatSummary(options: GwenOptions, info: GwenInfo, summary: ResultsSummary): Option[String] = None
  
  private [report] def relativePath(reportFile: File, reportDir: File) = 
    reportFile.getPath.substring(reportDir.getPath.length + 1)
  
}