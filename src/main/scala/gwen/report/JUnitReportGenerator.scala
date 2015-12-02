/*
 * Copyright 2015 Branko Juric, Brady Wood
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

import gwen.GwenInfo
import gwen.dsl.FeatureSpec
import gwen.eval.FeatureResult
import gwen.eval.FeatureUnit
import gwen.eval.GwenOptions

/**
  * Generates JUnit xml report files (for integration will build servers 
  * that support the standand JUnit report syntax).
  * 
  * @author Branko Juric
  */
class JUnitReportGenerator(val options: GwenOptions) extends ReportGenerator(ReportFormat.junit, options) with JUnitReportFormatter {

  override def reportAttachments(spec: FeatureSpec, featureReportFile: File): Unit = {
    // noop
  }
  
  override def reportMetaDetail(info: GwenInfo, unit: FeatureUnit, metaResults: List[FeatureResult], reportFiles: List[File]): List[File] = Nil
  
}