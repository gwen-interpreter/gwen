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

import java.io.File
import java.net.InetAddress

import org.joda.time.DateTime
import org.joda.time.DateTimeZone
import gwen.{GwenInfo, Settings}
import gwen.Predefs.Exceptions
import gwen.dsl._
import gwen.eval.FeatureResult
import gwen.eval.FeatureSummary
import gwen.eval.GwenOptions
import gwen.eval.FeatureUnit
import gwen.eval.SpecNormaliser
import gwen.Predefs.Formatting._
import scala.sys.process._

import scala.util.Try

/** Formats the feature summary and detail reports in JUnit xml. */
trait JUnitReportFormatter extends ReportFormatter with SpecNormaliser {
  
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
    
    val scenarios = result.spec.evalScenarios.filter(!_.isStepDef).flatMap { scenario =>
      if (scenario.isOutline) {
        val s = if (EvalStatus.isEvaluated(scenario.evalStatus.status)) {
          scenario
        } else {
          expandScenarioOutline(scenario, scenario.background)
        }
        s.examples.flatMap(_.scenarios).map((_, true))
      }
      else List((scenario, false))
    }

    val hostname = s""" hostname="${escapeXml(Try(InetAddress.getLocalHost.getHostName).getOrElse("hostname".!!.trim))}""""
    val packageName = result.spec.featureFile.map(f => escapeXml(f.getPath)).getOrElse("")
    val name = s""" name="$packageName.Feature: ${escapeXml(result.spec.feature.name)}""""
    val pkg = result.spec.featureFile.map(_ => s""" package="$packageName"""").getOrElse("")
    val scenarioCount = scenarios.length
    val tests = s""" tests="$scenarioCount""""
    val counts = result.summary.evalScenarioCounts
    val errorCount = counts.getOrElse(StatusKeyword.Failed, 0)
    val errors = s""" errors="$errorCount""""
    val skipped = s""" skipped="${counts.getOrElse(StatusKeyword.Skipped, 0) + counts.getOrElse(StatusKeyword.Pending, 0)}""""
    val time = s""" time="${result.elapsedTime.toNanos.toDouble / 1000000000d}""""
    val timestamp = s""" timestamp="${new DateTime(result.finished).withZone(DateTimeZone.UTC)}""""
    
    Some(s"""<?xml version="1.0" encoding="UTF-8" ?>
<testsuite$hostname$name$pkg$tests$errors$skipped$time$timestamp>
    <properties>${Settings.entries.map { case (n, v) => s"""
        <property name="${escapeXml(n)}" value="${escapeXml(v)}"/>"""}.mkString}
    </properties>${scenarios.zipWithIndex.map{case ((scenario, isExpanded), idx) => s"""
    <testcase name="Scenario ${padWithZeroes(idx + 1)}${if (isExpanded) " Outline" else ""}: ${escapeXml(scenario.name)}" time="${scenario.evalStatus.nanos.toDouble / 1000000000d}" status="${escapeXml(scenario.evalStatus.status.toString)}"${scenario.evalStatus match {
    case Failed(_, error) => 
      s""">
        <error type="${escapeXml(error.getClass.getName)}" message="${escapeXml(error.writeStackTrace)}"/>
    </testcase>"""
    case Skipped | Pending => 
      s""">
        <skipped/>
    </testcase>"""
    case _ => "/>"
  }}"""}.mkString}
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