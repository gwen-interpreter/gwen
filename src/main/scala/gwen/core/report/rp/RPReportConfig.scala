/*
 * Copyright 2020-2021 Branko Juric, Brady Wood
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

package gwen.core.report.rp

import gwen.core.GwenOptions
import gwen.core.node.gherkin.Spec
import gwen.core.report.ReportConfig
import gwen.core.report.ReportFormat
import gwen.core.state.DataRecord

import java.io.File

object RPReportConfig extends ReportConfig(
  ReportFormat.rp,
  "Report Portal",
  None,
  None,
  (options, info) => new RPReportGenerator(options, info),
  options => None,
  (spec: Spec, dataRecord: Option[DataRecord]) => None) {
  override def createReportDir(options: GwenOptions, spec: Spec, dataRecord: Option[DataRecord]): Option[File] = None
}