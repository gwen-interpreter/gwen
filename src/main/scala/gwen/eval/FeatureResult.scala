/*
 * Copyright 2014-2015 Branko Juric, Brady Wood
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

package gwen.eval

import gwen.dsl.{EvalStatus, FeatureSpec, Pending, StatusKeyword}
import gwen.report.ReportFormat
import java.io.File

import scala.concurrent.duration._
import java.util.Date

import gwen.Predefs.Formatting._
import gwen.Predefs.DurationOps
import gwen.dsl.Scenario
import gwen.dsl.FeatureKeyword

/**
  * Captures the results of an evaluated feature.
  * 
  * @param spec the evaluated feature
  * @param metaResults the evaluated meta results
  * @param reports optional map of report files (keyed by report type)
  * @param started the started time
  * @param finished the finished time
  */
class FeatureResult(
  val spec: FeatureSpec, 
  val reports: Option[Map[ReportFormat.Value, List[File]]], 
  val metaResults: List[FeatureResult],
  val started: Date,
  val finished: Date) {
  
  lazy val elapsedTime = Duration(finished.getTime - started.getTime, MILLISECONDS)
  lazy val screenshots: List[File] = spec.steps.flatMap(_.attachments).filter(_._1 == "Screenshot").map(_._2)
  lazy val isMeta: Boolean = spec.featureFile.exists(_.getName.endsWith(".meta"))
  lazy val summary = FeatureSummary(this)
  lazy val evalStatus: EvalStatus = spec.evalStatus
  lazy val duration: Duration = evalStatus.duration
  lazy val overhead: Duration = elapsedTime - duration - DurationOps.sum(metaResults.map(_.overhead))
  lazy val sustainedCount: Int = spec.sustainedCount
  
  private[eval] lazy val scenarioCounts = 
    StatusKeyword.countsByStatus(spec.evalScenarios.flatMap { s =>
      if (s.isOutline) {
        val scenarios = s.examples.flatMap(_.scenarios)
        if (scenarios.nonEmpty) {
          scenarios.map(_.evalStatus)
        } else {
          s.examples.flatMap(_.table.tail).map(_ => Pending).map(_.asInstanceOf[EvalStatus])
        }
      }
      else List(s.evalStatus)
    })

  private[eval] lazy val ruleCounts =
    StatusKeyword.countsByStatus(spec.rules.map(_.evalStatus))

  private[eval] lazy val stepCounts = StatusKeyword.countsByStatus(spec.evalScenarios.flatMap(_.allSteps.map(_.evalStatus)))
  override def toString: String = s"""[${formatDuration(duration)}] ${evalStatus.status}${if (sustainedCount > 0) s" with ${sustainedCount} sustained error${if (sustainedCount > 1) "s" else ""}" else ""} ${evalStatus.emoticon}, [${formatDuration(overhead)}] Overhead, [${formatDuration(elapsedTime)}] Elapsed, Started: $started, Finished: $finished""".stripMargin
}

