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

import gwen._
import gwen.dsl.StatusKeyword
import gwen.dsl.EvalStatus
import gwen.dsl.Passed

import scala.concurrent.duration._

import java.util.Date

/**
  * Captures the feature summary results of an evaluated feature.
  * 
  * @param results the feature results
  * @param ruleCounts number of rules by status
  * @param scenarioCounts number of scenarios (include those in rules) by status
  * @param stepCounts number of steps by status
  * 
  * @author Branko Juric
  */
case class FeatureSummary(
  results: List[FeatureResult],
  ruleCounts: Map[StatusKeyword.Value, Int], 
  scenarioCounts: Map[StatusKeyword.Value, Int], 
  stepCounts: Map[StatusKeyword.Value, Int]) {
  
  lazy val started: Date = results.sortBy(_.started).headOption.map(_.started).getOrElse(new Date)
  lazy val finished: Date = results.sortBy(_.finished).lastOption.map(_.finished).getOrElse(started)
  lazy val elapsedTime: Duration = Duration(finished.getTime - started.getTime, MILLISECONDS)
  
  private lazy val statuses = results.map(_.spec.evalStatus)
  lazy val evalStatus: EvalStatus = if (results.nonEmpty) EvalStatus(statuses) else Passed(0)
  lazy val resultsElapsedTime: Duration = DurationOps.sum(results.map(_.elapsedTime))
  lazy val overhead: Duration = DurationOps.sum(results.map(_.overhead))
  lazy val featureCounts: Map[_root_.gwen.dsl.StatusKeyword.Value, Int] = StatusKeyword.countsByStatus(statuses)
  lazy val sustainedCount: Int = results.map(_.sustainedCount).sum
  
  /** 
    * Adds the given feature result to the current summary (accumulates). 
    * 
    * @param featureResult the feature result to add
    */
  def +(featureResult: FeatureResult): FeatureSummary = {
    new FeatureSummary(
      this.results ++ List(featureResult), 
      addCounts(this.ruleCounts, featureResult.ruleCounts),
      addCounts(this.scenarioCounts, featureResult.scenarioCounts),
      addCounts(this.stepCounts, featureResult.stepCounts))
  }

  private def addCounts(countsA: Map[StatusKeyword.Value, Int], countsB: Map[StatusKeyword.Value, Int]): Map[StatusKeyword.Value, Int] =
    (StatusKeyword.reportables flatMap { status => 
      val a = countsA.getOrElse(status, 0)
      val b = countsB.getOrElse(status, 0)
      val sum = a + b
      if (sum > 0) Some((status, sum)) else None
    }).toMap
    
  override def toString: String = {
    val featureCount = featureCounts.values.sum
    val rulesCount = ruleCounts.values.sum
    val scenarioCount = scenarioCounts.values.sum
    val stepCount = stepCounts.values.sum
    s"""|$featureCount feature${if (featureCount == 1) "" else "s"}: ${formatCounts(featureCounts)}
        |$rulesCount rule${if (rulesCount == 1) "" else "s"}: ${formatCounts(ruleCounts)}
        |$scenarioCount scenario${if (scenarioCount == 1) "" else "s"}: ${formatCounts(scenarioCounts)}
        |$stepCount step${if (stepCount == 1) "" else "s"}: ${formatCounts(stepCounts)}
        |
        |[${Formatting.formatDuration(resultsElapsedTime)}] ${evalStatus.status}${if (sustainedCount > 0) s" with ${sustainedCount} sustained error${if (sustainedCount > 1) "s" else ""}" else ""} ${evalStatus.emoticon}
        |[${Formatting.formatDuration(overhead)}] Overhead
        |[${Formatting.formatDuration(elapsedTime)}] Elapsed, Started: $started, Finished: $finished""".stripMargin
  }
  
  private def formatCounts(counts: Map[StatusKeyword.Value, Int]) = 
    StatusKeyword.reportables map { status =>
      val count = counts.getOrElse(status, 0)
      s"$status $count"
    } mkString ", "
  
}

/** Feature summary factory. */
object FeatureSummary {
  def apply(): FeatureSummary = new FeatureSummary(Nil, Map(), Map(), Map())
  def apply(elapsedTime: Duration): FeatureSummary = new FeatureSummary(Nil, Map(), Map(), Map())
  def apply(result: FeatureResult): FeatureSummary = FeatureSummary() + result
}


