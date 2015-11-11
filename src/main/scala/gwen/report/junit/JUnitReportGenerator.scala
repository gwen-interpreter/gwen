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

package gwen.report.junit

import java.io.File
import scala.reflect.io.Path
import gwen.Predefs.FileIO
import gwen.Predefs.Kestrel
import gwen.report.ReportGenerator
import gwen.GwenInfo
import gwen.eval.GwenOptions
import gwen.report.html.JUnitReportFormatter
import gwen.dsl.FeatureSpec
import gwen.eval.DataRecord
import gwen.eval.FeatureResult

/**
  * Generates JUnit xml report files (for integration will build servers 
  * that support the standand JUnit report syntax).
  * 
  * @author Branko Juric
  */
class JUnitReportGenerator(val options: GwenOptions) 
  extends ReportGenerator(options, "feature-summary.xml", "xml", Some("junit-xml")) 
  with JUnitReportFormatter {

  override def reportAttachments(spec: FeatureSpec, featureReportFile: File): Unit = {
    // noop
  }
  
  override def reportMetaDetail(info: GwenInfo, metaSpecs: List[FeatureSpec], featureReportFile: File): List[FeatureResult] = {
    metaSpecs.map(FeatureResult(_, None, Nil))
  }
  
}