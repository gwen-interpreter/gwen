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

/** Trait for formatting the feature summary and detail reports. */
trait ReportFormatter {

  /**
    * Formats the feature detail report.
    * 
    * @param info the gwen implementation info
    * @param result the feature result
    * @param breadcrumbs names and references for linking back to parent reports
    */
  def formatDetail(info: GwenInfo, result: FeatureResult, breadcrumbs: List[(String, String)]): String
  
  /**
    * Formats the feature summary report.
    * 
    * @param info the gwen implementation info
    * @param summary the accumulated feature results summary
    */
  def formatSummary(info: GwenInfo, summary: FeatureSummary): String
  
}