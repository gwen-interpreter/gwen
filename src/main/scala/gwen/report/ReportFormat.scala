/*
 * Copyright 2015-2017 Branko Juric, Brady Wood
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

/**
  * Enumeration of supported report formats.
  * 
  * @author Branko Juric
  */
object ReportFormat extends Enumeration {

  type ReportFormat = Value
  val html, slideshow, junit, json = Value
  
  def configOf(format: ReportFormat.Value): Option[ReportConfig] = {
    format match {
      case ReportFormat.html => Some(HtmlReportConfig)
      case ReportFormat.slideshow => Some(HtmlSlideshowReportConfig)
      case ReportFormat.junit => Some(JUnitReportConfig)
      case ReportFormat.json => Some(JsonReportConfig)
      case _ => None
    }
  }
  
}

