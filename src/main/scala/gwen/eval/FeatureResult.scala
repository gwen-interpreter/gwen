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

import gwen.dsl.FeatureSpec
import gwen.report.ReportFormat
import java.io.File
import scala.concurrent.duration.Duration
import java.util.Date
import gwen.dsl.StatusKeyword
import gwen.Predefs.Formatting._

/**
  * Captures the results of an evaluated feature.
  * 
  * @param spec the evaluated feature
  * @param metaResults the evaluated meta results
  * @param reports optional map of report files (keyed by report type)
  * @param elapsedTime the time it took to process the feature
  */
class FeatureResult(
  val spec: FeatureSpec, 
  val reports: Option[Map[ReportFormat.Value, List[File]]], 
  val metaResults: List[FeatureResult],
  val elapsedTime: Duration) {
  
  val finished = new Date()
  val started = new Date(finished.getTime() - elapsedTime.toMillis)
  lazy val screenshots = spec.steps.flatMap(_.attachments).filter(_._1 == "Screenshot").map(_._2)
  lazy val isMeta = spec.featureFile.map(_.getName().endsWith(".meta")).getOrElse(false)
  def summary = FeatureSummary(this)
  private[eval] def scenarioCounts = StatusKeyword.countsByStatus(spec.scenarios.map(_.evalStatus))
  private[eval] def stepCounts = StatusKeyword.countsByStatus(spec.scenarios.flatMap(_.allSteps.map(_.evalStatus)))
  override def toString = s"[${formatDuration(spec.evalStatus.duration)}] ${spec.evalStatus.status} [${formatDuration(elapsedTime)}] Elapsed [${formatDuration(elapsedTime - spec.evalStatus.duration)}] Overhead ${finished} ${spec.evalStatus.emoticon}"
  
}

/** Feature result factory. */
object FeatureResult {
  def apply(spec: FeatureSpec, reports: Option[Map[ReportFormat.Value, List[File]]], metaResults: List[FeatureResult], elapsedTime: Duration): FeatureResult = 
    new FeatureResult(spec, reports, metaResults, elapsedTime)
}

