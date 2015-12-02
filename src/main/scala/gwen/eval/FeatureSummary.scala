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

import java.util.Date
import gwen.dsl.FeatureSpec
import gwen.dsl.Scenario
import gwen.dsl.StatusKeyword
import gwen.dsl.Tag
import java.io.File
import gwen.dsl.EvalStatus
import gwen.dsl.Passed
import gwen.report.ReportFormat
import scala.concurrent.duration.Duration
import gwen.dsl.DurationFormatter

/**
  * Captures the feature summary results of an evaluated feature.
  * 
  * @param summaryLines feature summary lines
  * @param featureCounts number of features by status
  * @param scenarioCounts number of scenarios by status
  * @param stepCounts number of steps by status
  * 
  * @author Branko Juric
  */
case class FeatureSummary(
  results: List[FeatureResult],
  scenarioCounts: Map[StatusKeyword.Value, Int], 
  stepCounts: Map[StatusKeyword.Value, Int]) {
  
  val finished = new Date()
  val statuses = results.map(_.spec.evalStatus)
  def evalStatus = EvalStatus(statuses)
  def featureCounts = StatusKeyword.countsByStatus(statuses)
  
  /** 
    * Adds the given feature result to the current summary (accumulates). 
    * 
    * @param featureResult the feature result to add
    */
  def +(featureResult: FeatureResult): FeatureSummary =
    new FeatureSummary(
      this.results ++ List(featureResult), 
      addCounts(this.scenarioCounts, StatusKeyword.countsByStatus(featureResult.spec.scenarios.map(_.evalStatus))),
      addCounts(this.stepCounts, StatusKeyword.countsByStatus(featureResult.spec.scenarios.flatMap(_.allSteps.map(_.evalStatus)))))
  
  private def addCounts(countsA: Map[StatusKeyword.Value, Int], countsB: Map[StatusKeyword.Value, Int]): Map[StatusKeyword.Value, Int] =
    (StatusKeyword.reportables flatMap { status => 
      val a = countsA.get(status).getOrElse(0)
      val b = countsB.get(status).getOrElse(0)
      val sum = a + b
      if (sum > 0) Some((status, sum)) else None
    }).toMap
    
  def duration: Duration = results.map(_.duration).reduce(_+_)
  
  override def toString = { 
    val featureCount = featureCounts.map(_._2).sum
    val scenarioCount = scenarioCounts.map(_._2).sum
    val stepCount = stepCounts.map(_._2).sum
    s"""|${featureCount} feature${if (featureCount == 1) "" else "s"}: ${formatCounts(featureCounts)}
        |${scenarioCount} scenario${if (scenarioCount == 1) "" else "s"}: ${formatCounts(scenarioCounts)}
        |${stepCount} step${if (stepCount == 1) "" else "s"}: ${formatCounts(stepCounts)}
        |
        |[${DurationFormatter.format(duration)}] ${evalStatus.status} ${finished} ${evalStatus.emoticon}""".stripMargin
  }
  
  private def formatCounts(counts: Map[StatusKeyword.Value, Int]) = 
    StatusKeyword.reportables map { status =>
      val count = counts.get(status).getOrElse(0)
      s"${status} ${count}"
    } mkString(", ")
  
}

/** Feature summary factory. */
object FeatureSummary {
  def apply(): FeatureSummary = new FeatureSummary(Nil, Map(), Map())
  def apply(result: FeatureResult): FeatureSummary = FeatureSummary() + result
}

/**
  * Captures the results of an evaluated feature.
  * 
  * @param spec the evaluated feature
  * @param metaResults the evaluated meta results
  * @param reports optional map of report files (keyed by report type)
  * @param duration the time it took to process the feature
  */
class FeatureResult(
  val spec: FeatureSpec, 
  val reports: Option[Map[ReportFormat.Value, List[File]]], 
  val metaResults: List[FeatureResult],
  val duration: Duration) {
  
  val finished = new Date()
  lazy val screenshots = spec.steps.flatMap(_.attachments).filter(_._1 == "Screenshot").map(_._2)
  lazy val isMeta = spec.featureFile.map(_.getName().endsWith(".meta")).getOrElse(false)
  def summary = FeatureSummary(this)
  
}

/** Feature result factory. */
object FeatureResult {
  def apply(spec: FeatureSpec, reports: Option[Map[ReportFormat.Value, List[File]]], metaResults: List[FeatureResult], duration: Duration): FeatureResult = 
    new FeatureResult(spec, reports, metaResults, duration)
}

