/*
 * Copyright 2017-2021 Branko Juric, Brady Wood
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

package gwen.core.report.json

import gwen.core.GwenInfo
import gwen.core.GwenOptions
import gwen.core.report.NoopReportGenerator

/**
  * Generates JSON report files (for integration with other gherkin report builders).
  * 
  * @author Branko Juric
  */
class JsonReportGenerator(options: GwenOptions, info: GwenInfo) extends NoopReportGenerator(JsonReportConfig, options, info) with JsonReportFormatter