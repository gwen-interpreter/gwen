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

import gwen.dsl.StatusKeyword
import scala.concurrent.duration._
import java.util.Date
import gwen.dsl.EvalStatus
import gwen.dsl.Passed
import gwen.Predefs.Formatting._
import gwen.Predefs.DurationOps

/**
  * Captures the feature summary results of an evaluated feature.
  * 
  * @param results the feature results
  * @param scenarioCounts number of scenarios by status
  * @param stepCounts number of steps by status
  * 
  * @author Branko Juric
  */
case class FeatureSummary(
  results: List[FeatureResult],
  scenarioCounts: Map[StatusKeyword.Value, Int], 
  stepCounts: Map[StatusKeyword.Value, Int]) {
  
  lazy val started = results.sortBy(_.started).headOption.map(_.started).getOrElse(new Date)
  lazy val finished = results.sortBy(_.finished).lastOption.map(_.finished).getOrElse(started)
  lazy val elapsedTime = Duration(finished.getTime - started.getTime, MILLISECONDS)
  
  private lazy val statuses = results.map(_.spec.evalStatus)
  lazy val evalStatus = if (results.nonEmpty) EvalStatus(statuses) else Passed(0)
  lazy val resultsElapsedTime = DurationOps.sum(results.map(_.elapsedTime))
  lazy val overhead = elapsedTime - resultsElapsedTime
  lazy val featureCounts = StatusKeyword.countsByStatus(statuses)
  
  /** 
    * Adds the given feature result to the current summary (accumulates). 
    * 
    * @param featureResult the feature result to add
    */
  def +(featureResult: FeatureResult): FeatureSummary = 
    new FeatureSummary(
      this.results ++ List(featureResult), 
      addCounts(this.scenarioCounts, featureResult.scenarioCounts),
      addCounts(this.stepCounts, featureResult.stepCounts))
  
  private def addCounts(countsA: Map[StatusKeyword.Value, Int], countsB: Map[StatusKeyword.Value, Int]): Map[StatusKeyword.Value, Int] =
    (StatusKeyword.reportables flatMap { status => 
      val a = countsA.get(status).getOrElse(0)
      val b = countsB.get(status).getOrElse(0)
      val sum = a + b
      if (sum > 0) Some((status, sum)) else None
    }).toMap
    
  override def toString = { 
    val featureCount = featureCounts.map(_._2).sum
    val scenarioCount = scenarioCounts.map(_._2).sum
    val stepCount = stepCounts.map(_._2).sum
    s"""|${featureCount} feature${if (featureCount == 1) "" else "s"}: ${formatCounts(featureCounts)}
        |${scenarioCount} scenario${if (scenarioCount == 1) "" else "s"}: ${formatCounts(scenarioCounts)}
        |${stepCount} step${if (stepCount == 1) "" else "s"}: ${formatCounts(stepCounts)}
        |
        |[${formatDuration(resultsElapsedTime)}] ${evalStatus.status} ${evalStatus.emoticon}
        |[${formatDuration(overhead)}] Overhead
        |[${formatDuration(elapsedTime)}] Elapsed, Started: ${started}, Finished: ${finished}""".stripMargin
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
  def apply(elapsedTime: Duration): FeatureSummary = new FeatureSummary(Nil, Map(), Map())
  def apply(result: FeatureResult): FeatureSummary = FeatureSummary() + result
}


