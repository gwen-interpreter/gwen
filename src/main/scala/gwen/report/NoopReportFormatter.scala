/*
 * Copyright 2017 Branko Juric, Brady Wood
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

import gwen.GwenInfo
import gwen.GwenOptions
import gwen.model.SpecResult
import gwen.model.ResultsSummary
import gwen.model.FeatureUnit

import java.io.File

/**
  * Noop report formatter.
  */
trait NoopReportFormatter extends ReportFormatter {
 
  override def formatDetail(options: GwenOptions, info: GwenInfo, unit: FeatureUnit, result: SpecResult, breadcrumbs: List[(String, File)], reportFiles: List[File]): Option[String] = None
  override def formatSummary(options: GwenOptions, info: GwenInfo, summary: ResultsSummary): Option[String] = None

}