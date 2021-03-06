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
import gwen.dsl.FeatureSpec
import gwen.eval.{FeatureResult, FeatureUnit, GwenOptions}

import java.io.File

/**
  * Noop report generator.
  */
abstract class NoopReportGenerator(config: ReportConfig, options: GwenOptions) extends ReportGenerator(config, options) with ReportFormatter {
  
  override def reportAttachments(spec: FeatureSpec, featureReportFile: File): Unit = {}
  override def reportMetaDetail(info: GwenInfo, unit: FeatureUnit, metaResults: List[FeatureResult], reportFiles: List[File]): List[File] = Nil

}