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

import gwen.dsl.{EvalStatus, FeatureSpec, StatusKeyword}
import gwen.report.ReportFormat
import java.io.File

import scala.concurrent.duration._
import java.util.Date

import gwen.Predefs.Formatting._
import gwen.Predefs.DurationOps

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
  
  private[eval] lazy val scenarioCounts = StatusKeyword.countsByStatus(spec.scenarios.map(_.evalStatus))
  private[eval] lazy val stepCounts = StatusKeyword.countsByStatus(spec.scenarios.flatMap(_.allSteps.map(_.evalStatus)))
  override def toString: String = s"""[${formatDuration(duration)}] ${evalStatus.status} ${evalStatus.emoticon}, [${formatDuration(overhead)}] Overhead, [${formatDuration(elapsedTime)}] Elapsed, Started: $started, Finished: $finished""".stripMargin
}

