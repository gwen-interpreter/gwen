/*
 * Copyright 2015-2020 Branko Juric, Brady Wood
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

class ReportFormatTest extends FlatSpec with Matchers  {

  "valueOf of all formats" should "map correctly" in {
    ReportFormat.withName("html") should be (ReportFormat.html)
    ReportFormat.withName("slideshow") should be (ReportFormat.slideshow)
    ReportFormat.withName("junit") should be (ReportFormat.junit)
    ReportFormat.withName("json") should be (ReportFormat.json)
    ReportFormat.withName("rp") should be (ReportFormat.rp)
  }

  "configOf of all formats" should "map correctly" in {
    ReportFormat.configOf(ReportFormat.html) should be (Some(HtmlReportConfig))
    ReportFormat.configOf(ReportFormat.slideshow) should be (Some(HtmlSlideshowReportConfig))
    ReportFormat.configOf(ReportFormat.junit) should be (Some(JUnitReportConfig))
    ReportFormat.configOf(ReportFormat.json) should be (Some(JsonReportConfig))
    ReportFormat.configOf(ReportFormat.rp) should be (Some(RPReportConfig))
  }
  
}