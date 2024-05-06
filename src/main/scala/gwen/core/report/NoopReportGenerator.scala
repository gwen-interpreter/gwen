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

package gwen.core.report

import gwen.core.GwenInfo
import gwen.core.GwenOptions
import gwen.core.node.FeatureUnit
import gwen.core.node.gherkin.Spec
import gwen.core.result.SpecResult

import java.io.File

/**
  * No-op report generator.
  */
abstract class NoopReportGenerator(config: ReportConfig, options: GwenOptions, info: GwenInfo) extends ReportGenerator(config, options, info) with ReportFormatter {
  
  override def copyAttachments(spec: Spec, featureReportFile: File): Unit = {}
  override def copyVideos(result: SpecResult, featureReportFile: File): Unit = {}
  override def reportMetaDetail(unit: FeatureUnit, metaResults: List[SpecResult], reportFiles: List[File]): List[File] = Nil

}