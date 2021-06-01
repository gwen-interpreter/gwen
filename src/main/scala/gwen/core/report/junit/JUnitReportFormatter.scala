/*
 * Copyright 2015-2021 Branko Juric, Brady Wood
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
package gwen.core.report.junit

import gwen.core._
import gwen.core.Formatting.padWithZeroes
import gwen.core.node.FeatureUnit
import gwen.core.node.gherkin.Scenario
import gwen.core.node.gherkin.SpecNormaliser
import gwen.core.report.ReportFormatter
import gwen.core.status._
import gwen.core.result.SpecResult

import scala.sys.process._
import scala.util.Try

import org.joda.time.DateTime
import org.joda.time.DateTimeZone
import scalatags.Text.all._

import java.io.File
import java.net.InetAddress


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
  override def formatDetail(options: GwenOptions, info: GwenInfo, unit: FeatureUnit, result: SpecResult, breadcrumbs: List[(String, File)], reportFiles: List[File]): Option[String] = {
    
    val hostname = Try(InetAddress.getLocalHost.getHostName).getOrElse("hostname".!!.trim)
    val packageName = result.spec.specFile.map(f => f.getPath).getOrElse("")
    val name = s"$packageName.Feature: ${result.spec.feature.name}"
    val pkg = result.spec.specFile.map(_ => packageName)
    val scenarios = findScenarios(result)
    val scenarioCount = scenarios.length
    val evalStatuses = scenarios.map(_.evalStatus)
    val failureCount = evalStatuses.filter(_.isAssertionError).size
    val errorCount = evalStatuses.filter(status => status.isError && !status.isAssertionError).size
    val skippedCount = evalStatuses.filter(status => status.isSkipped || status.isPending).size
    val time = result.elapsedTime.toNanos.toDouble / 1000000000d
    val timestamp = new DateTime(result.finished).withZone(DateTimeZone.UTC).toString
    
    val testsuiteTag = {
      tag("testsuite")(
        attr("hostname") := hostname,
        attr("name") := name,
        for {
          pckgName <- pkg
        } yield {
          attr("package") := pckgName
        },
        attr("tests") := scenarioCount,
        attr("errors") := errorCount,
        attr("failures") := failureCount,
        attr("skipped") := skippedCount,
        attr("time") := time,
        attr("timestamp") := timestamp,
        tag("properties")(
          for {
            (n, v) <- Settings.entries.toList
          } yield {
            tag("property")(
              attr("name") := n,
              attr("value") := v
            )
          }
        ),
        for {
          (scenario, idx) <- scenarios.zipWithIndex
          name = s"Scenario ${padWithZeroes(idx + 1)}: ${scenario.name}"
          time = scenario.evalStatus.nanos.toDouble / 1000000000d
          status = scenario.evalStatus.keyword.toString
        } yield {
          tag("testcase")(
            attr("name") := name,
            attr("time") := time,
            attr("status") := status,
            scenario.evalStatus match {
              case status @ Failed(_, error) => 
                tag(if (status.isAssertionError) "failure" else "error")(
                  attr("type") := error.getClass.getName,
                  attr("message") := error.getMessage
                )
              case Skipped | Pending => 
                tag("skipped")
              case _ =>
            },
            scenario.evalStatus match {
              case status @ Failed(_, error) if !status.isAssertionError => 
                tag("system-err")(error.writeStackTrace())
              case _ =>
            }
          )
        }
      )
    }
    
    val junitXML = s"""<?xml version="1.0" encoding="UTF-8" ?>""" + testsuiteTag.render
    Some(Formatting.prettyPrintXML(junitXML, Some("system-err")))
    
  }

  private def findScenarios(result: SpecResult): List[Scenario] = {
    result.spec.evalScenarios.filter(!_.isStepDef).flatMap { scenario =>
      if (scenario.isOutline) {
        val s = if (scenario.evalStatus.isEvaluated) {
          scenario
        } else {
          normaliseScenarioOutline(scenario, scenario.background)
        }
        s.examples.flatMap(_.scenarios)
      }
      else List(scenario)
    }
  }
  
}
