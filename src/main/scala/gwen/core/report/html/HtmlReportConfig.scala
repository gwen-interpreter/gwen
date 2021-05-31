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

package gwen.core.report.html

import gwen.core.node.gherkin.Spec
import gwen.core.report.ReportConfig
import gwen.core.report.ReportFormat

import java.io.File

object HtmlReportConfig extends ReportConfig(
  ReportFormat.html,
  "HTML", 
  Some("html"), 
  Some("feature-summary"), 
  options => new HtmlReportGenerator(options), 
  options => options.reportDir.map(dir => new File(dir, "html")),
  (spec: Spec, _) =>
    Some(spec.specFile.map(_.getName).getOrElse(spec.feature.name)))
