/*
 * Copyright 2020-2024 Branko Juric, Brady Wood
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

import gwen.core.BaseTest
import gwen.core.GwenInfo
import gwen.core.GwenOptions
import gwen.core.report.html.HtmlReportConfig
import gwen.core.report.html.HtmlReportGenerator
import gwen.core.report.html.HtmlSlideshowConfig
import gwen.core.report.html.HtmlSlideshowGenerator
import gwen.core.report.json.JsonReportConfig
import gwen.core.report.json.JsonReportGenerator
import gwen.core.report.junit.JUnitReportConfig
import gwen.core.report.junit.JUnitReportGenerator
import gwen.core.report.results.ResultReportsConfig
import gwen.core.report.results.ResultReportsGenerator

import org.scalatest.matchers.should.Matchers

import java.io.File

class ReportConfigTest extends BaseTest with Matchers  {
  
  "Format for all report formats" should "map correctly" in {
    HtmlReportConfig.format should be (ReportFormat.html)
    HtmlSlideshowConfig.format should be (ReportFormat.slideshow)
    JUnitReportConfig.format should be (ReportFormat.junit)
    JsonReportConfig.format should be (ReportFormat.json)
    ResultReportsConfig.format should be (ReportFormat.results)
  }

  "File extensions for all report formats" should "map correctly" in {
    HtmlReportConfig.fileExtension should be (Some("html"))
    HtmlSlideshowConfig.fileExtension should be (Some("html"))
    JUnitReportConfig.fileExtension should be (Some("xml"))
    JsonReportConfig.fileExtension should be (Some("json"))
    ResultReportsConfig.fileExtension should be (Some("csv"))
  }
  
  "Names of all report formats" should "map correctly" in {
    HtmlReportConfig.name should be ("HTML")
    HtmlSlideshowConfig.name should be ("Slideshow")
    JUnitReportConfig.name should be ("JUnit-XML")
    JsonReportConfig.name should be ("JSON")
    ResultReportsConfig.name should be ("Results")
  }
  
  "Output directory of all report formats" should "map correctly" in {
    val options = GwenOptions(reportDir = Some(new File("target/report")))
    HtmlReportConfig.reportDir(options).get.getPath should be (s"target${File.separatorChar}report${File.separatorChar}html")
    HtmlSlideshowConfig.reportDir(options).get.getPath should be (s"target${File.separatorChar}report${File.separatorChar}html")
    JUnitReportConfig.reportDir(options).get.getPath should be (s"target${File.separatorChar}report${File.separatorChar}junit")
    JsonReportConfig.reportDir(options).get.getPath should be (s"target${File.separatorChar}report${File.separatorChar}json")
    ResultReportsConfig.reportDir(options).get.getPath should be (s"target${File.separatorChar}report${File.separatorChar}results")
  }
  
  "Report generator for all report formats" should "map correctly" in {
    val options = GwenOptions(reportDir = Some(new File("target/report")))
    val info = new GwenInfo() { }
    HtmlReportConfig.reportGenerator(options, info).isInstanceOf[HtmlReportGenerator] should be (true)
    HtmlSlideshowConfig.reportGenerator(options, info).isInstanceOf[HtmlSlideshowGenerator] should be (true)
    JUnitReportConfig.reportGenerator(options, info).isInstanceOf[JUnitReportGenerator] should be (true)
    JsonReportConfig.reportGenerator(options, info).isInstanceOf[JsonReportGenerator] should be (true)
    ResultReportsConfig.reportGenerator(options, info).isInstanceOf[ResultReportsGenerator] should be (true)
  }
  
}