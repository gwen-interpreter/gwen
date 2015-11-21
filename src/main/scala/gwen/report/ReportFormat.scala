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

import scala.language.implicitConversions
import scala.language.postfixOps
import gwen.eval.GwenOptions
import java.io.File

/**
  * Enumeration of supported report formats.
  * 
  * @author Branko Juric
  */
object ReportFormat extends Enumeration {

  val html, junit = Value
  
  class FormatValue(
      val name: String, 
      val fileExtension: String, 
      val summaryFilename: Option[String], 
      getGenerator: GwenOptions => ReportGenerator, 
      getReportDir: GwenOptions => File) {
    def reportGenerator(options: GwenOptions): ReportGenerator = getGenerator(options)
    def reportDir(options: GwenOptions): File = getReportDir(options)
  }
  
  private val Values = Map(
    html -> new FormatValue(
        "HTML", 
        "html", 
        Some("feature-summary"), 
        options => new HtmlReportGenerator(options), 
        options => options.reportDir.map(new File(_, "html")).get),
    junit -> new FormatValue(
        "JUnit-XML", 
        "xml", 
        None, 
        options => new JUnitReportGenerator(options), 
        options => options.reportDir.map(new File(_, "junit")).get)
  )
  
  implicit def value2ReportFormat(value: Value) = Values(value)
  
}

