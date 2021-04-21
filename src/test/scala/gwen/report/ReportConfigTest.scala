/*
 * Copyright 2020 Branko Juric, Brady Wood
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

import gwen.eval.GwenOptions

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import java.io.File

class ReportConfigTest extends FlatSpec with Matchers  {
  
  "Format for all report formats" should "map correctly" in {
    HtmlReportConfig.format should be (ReportFormat.html)
    HtmlSlideshowReportConfig.format should be (ReportFormat.slideshow)
    JUnitReportConfig.format should be (ReportFormat.junit)
    JsonReportConfig.format should be (ReportFormat.json)
  }

  "File extensions for all report formats" should "map correctly" in {
    HtmlReportConfig.fileExtension should be (Some("html"))
    HtmlSlideshowReportConfig.fileExtension should be (Some("html"))
    JUnitReportConfig.fileExtension should be (Some("xml"))
    JsonReportConfig.fileExtension should be (Some("json"))
  }
  
  "Names of all report formats" should "map correctly" in {
    HtmlReportConfig.name should be ("HTML")
    HtmlSlideshowReportConfig.name should be ("Slideshow")
    JUnitReportConfig.name should be ("JUnit-XML")
    JsonReportConfig.name should be ("JSON")
  }
  
  "Output directory of all report formats" should "map correctly" in {
    val options = GwenOptions(reportDir = Some(new File("target/report")))
    HtmlReportConfig.reportDir(options).get.getPath should be (s"target${File.separatorChar}report${File.separatorChar}html")
    HtmlSlideshowReportConfig.reportDir(options).get.getPath should be (s"target${File.separatorChar}report${File.separatorChar}html")
    JUnitReportConfig.reportDir(options).get.getPath should be (s"target${File.separatorChar}report${File.separatorChar}junit")
    JsonReportConfig.reportDir(options).get.getPath should be (s"target${File.separatorChar}report${File.separatorChar}json")
  }
  
  "Report generator for all report formats" should "map correctly" in {
    val options = GwenOptions(reportDir = Some(new File("target/report")))
    HtmlReportConfig.reportGenerator(options).isInstanceOf[HtmlReportGenerator] should be (true)
    HtmlSlideshowReportConfig.reportGenerator(options).isInstanceOf[HtmlSlideshowGenerator] should be (true)
    JUnitReportConfig.reportGenerator(options).isInstanceOf[JUnitReportGenerator] should be (true)
    JsonReportConfig.reportGenerator(options).isInstanceOf[JsonReportGenerator] should be (true)
  }
  
}