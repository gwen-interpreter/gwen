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

import gwen.dsl.FeatureSpec
import java.io.File

/**
 * Trait for formatting the feature summary and detail reports.
 */
trait ReportFormatter {

  /**
   * Formats the feature detail report.
   * 
   * @param spec
   * 			the feature spec to report
   * @param interpreterName
   * 			the gwen interpreter name
   * @param backlinks
   *   			names and references for linking back to parent reports
   * @param metaReportFiles
   *   			list of meta report files (if any)
   */
  def formatDetail(spec: FeatureSpec, interpreterName: String, backlinks: List[(String, File)], metaReportFiles: List[File]): String
  
  /**
   * Formats the feature summary report.
   * 
   * @param summary
   * 			the accumulated feature results summary
   * @param interpreterName
   * 			the gwen interpreter name
   */
  def formatSummary(summary: FeatureSummary, interpreterName: String): String
  
}