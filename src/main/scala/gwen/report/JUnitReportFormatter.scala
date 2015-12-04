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
import java.net.InetAddress
import org.joda.time.DateTime
import org.joda.time.DateTimeZone
import gwen.GwenInfo
import gwen.Predefs.Exceptions
import gwen.dsl.Failed
import gwen.dsl.Pending
import gwen.dsl.Skipped
import gwen.dsl.StatusKeyword
import gwen.eval.FeatureResult
import gwen.eval.FeatureSummary
import gwen.eval.GwenOptions
import gwen.eval.FeatureUnit
import gwen.Predefs.Formatting._

/** Formats the feature summary and detail reports in JUnit xml. */
trait JUnitReportFormatter extends ReportFormatter {
  
  /**
    * Formats the feature detail report as HTML.
    * 
    * @param options gwen command line options
    * @param info the gwen implementation info
    * @param unit the feature input
    * @param result the feature result to report
    * @param breadcrumbs names and references for linking back to parent reports
    * @param reportFiles the target report files (head = detail, tail = metas)
    */
  override def formatDetail(options: GwenOptions, info: GwenInfo, unit: FeatureUnit, result: FeatureResult, breadcrumbs: List[(String, File)], reportFiles: List[File]): Option[String] = {
    
    val scenarios = result.spec.scenarios.filter(!_.isStepDef)
    val hostname = s""" hostname="${escapeXml(InetAddress.getLocalHost.getHostName)}""""
    val packageName = result.spec.featureFile.map(f => escapeXml(f.getPath())).getOrElse("")
    val name = s""" name="${packageName}.Feature: ${escapeXml(result.spec.feature.name)}""""
    val pkg = result.spec.featureFile.map(f => s""" package="${packageName}"""").getOrElse("")
    val scenarioCount = scenarios.length
    val tests = s""" tests="${scenarioCount}""""
    val counts = result.summary.scenarioCounts
    val errorCount = counts.get(StatusKeyword.Failed).getOrElse(0)
    val errors = s""" errors="${errorCount}""""
    val skipped = s""" skipped="${counts.get(StatusKeyword.Skipped).getOrElse(0) + counts.get(StatusKeyword.Pending).getOrElse(0)}""""
    val time = s""" time="${result.duration.toNanos.toDouble / 1000000000d}""""
    val timestamp = s""" timestamp="${new DateTime(result.finished).withZone(DateTimeZone.UTC)}""""
    
    Some(s"""<?xml version="1.0" encoding="UTF-8" ?>
<testsuite${hostname}${name}${pkg}${tests}${errors}${skipped}${time}${timestamp}>
    <properties>${(sys.props.map { case (name, value) => s"""
        <property name="${escapeXml(name)}" value="${escapeXml(value)}"/>"""}).mkString}
    </properties>${(scenarios.zipWithIndex.map{case (scenario, idx) => s"""
    <testcase name="Scenario ${padWithZeroes(idx + 1)}: ${escapeXml(scenario.name)}" time="${scenario.evalStatus.nanos.toDouble / 1000000000d}" status="${escapeXml(scenario.evalStatus.status.toString)}"${scenario.evalStatus match {
    case Failed(_, error) => 
      s""">
        <error type="${escapeXml(error.getClass().getName())}" message="${escapeXml(error.writeStackTrace)}"/>
    </testcase>"""
    case Skipped | Pending => 
      s""">
        <skipped/>
    </testcase>"""
    case _ => "/>"
  }}"""}).mkString}
</testsuite>
""")
  }
  
  /**
    * Formats the feature summary report as HTML.
    * 
    * @param options gwen command line options
    * @param info the gwen implementation info
    * @param summary the accumulated feature results summary
    */
  override def formatSummary(options: GwenOptions, info: GwenInfo, summary: FeatureSummary): Option[String] = None
  
}