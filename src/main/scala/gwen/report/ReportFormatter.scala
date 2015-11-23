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

import java.io.File
import gwen.eval.FeatureResult
import gwen.eval.FeatureSummary
import gwen.GwenInfo
import gwen.eval.GwenOptions
import gwen.eval.FeatureUnit

/** Trait for formatting the feature summary and detail reports. */
trait ReportFormatter {

  /**
    * Formats the feature detail report.
    * 
    * @param options gwen command line options
    * @param info the gwen implementation info
    * @param unit the feature input
    * @param result the feature result
    * @param breadcrumbs names and references for linking back to parent reports
    */
  def formatDetail(options: GwenOptions, info: GwenInfo, unit: FeatureUnit, result: FeatureResult, breadcrumbs: List[(String, File)]): Option[String]
  
  /**
    * Formats the feature summary report.
    * 
    * @param options gwen command line options
    * @param info the gwen implementation info
    * @param summary the accumulated feature results summary
    */
  def formatSummary(options: GwenOptions, info: GwenInfo, summary: FeatureSummary): Option[String]
  
  private[report] def relativePath(reportFile: File, reportDir: File) = reportFile.getPath.substring(reportDir.getPath().length() + 1)
  
}