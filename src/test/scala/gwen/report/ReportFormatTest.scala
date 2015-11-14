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

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import gwen.eval.GwenOptions
import java.io.File
import gwen.report.html.HtmlReportGenerator
import gwen.report.junit.JUnitReportGenerator

class ReportFormatTest extends FlatSpec with Matchers  {

  "valueOf of all formats" should "map correctly" in {
    ReportFormat.withName("html") should be (ReportFormat.html)
    ReportFormat.withName("junit") should be (ReportFormat.junit)
  }
  
  "File extensions for all report formats" should "map correctly" in {
    ReportFormat.html.fileExtension should be ("html")
    ReportFormat.junit.fileExtension should be ("xml")
  }
  
  "Names of all report formats" should "map correctly" in {
    ReportFormat.html.name should be ("HTML")
    ReportFormat.junit.name should be ("JUnit-XML")
  }
  
  "Output directory of all report formats" should "map correctly" in {
    val options = GwenOptions(reportDir = Some(new File("target/report")))
    ReportFormat.html.reportDir(options).getPath should be (s"target${File.separatorChar}report${File.separatorChar}html")
    ReportFormat.junit.reportDir(options).getPath should be (s"target${File.separatorChar}report${File.separatorChar}junit")
  }
  
  "Report generator for all report formats" should "map correctly" in {
    val options = GwenOptions(reportDir = Some(new File("target/report")))
    ReportFormat.html.reportGenerator(options).isInstanceOf[HtmlReportGenerator] should be (true)
    ReportFormat.junit.reportGenerator(options).isInstanceOf[JUnitReportGenerator] should be (true)
  }
  
}