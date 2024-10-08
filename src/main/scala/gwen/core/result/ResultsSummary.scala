/*
 * Copyright 2014-2021 Branko Juric, Brady Wood
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

package gwen.core.result

import gwen.core._
import gwen.core.node.NodeType
import gwen.core.report.ReportFormat
import gwen.core.report.ReportResult
import gwen.core.status._

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
case class ResultsSummary(
  results: List[SpecResult],
  ruleCounts: Map[StatusKeyword, Int], 
  scenarioCounts: Map[StatusKeyword, Int], 
  stepCounts: Map[StatusKeyword, Int],
  reportResults: List[ReportResult]) {
  
  lazy val started: Date = results.sortBy(_.started).headOption.map(_.started).getOrElse(new Date)
  lazy val finished: Date = results.sortBy(_.finished).lastOption.map(_.finished).getOrElse(started)
  lazy val elapsedTime: Duration = Duration(finished.getTime - started.getTime, MILLISECONDS)
  
  private lazy val statuses = results.map(_.spec.evalStatus)
  lazy val evalStatus: EvalStatus = if (results.nonEmpty) EvalStatus(statuses) else Passed(0)
  lazy val resultsElapsedTime: Duration = DurationOps.sum(results.map(_.elapsedTime))
  lazy val featureCounts: Map[StatusKeyword, Int] = EvalStatus.countsByType(statuses)
  lazy val sustainedCount: Int = results.map(_.sustainedCount).sum
  
  /** 
    * Adds the given feature result to the current summary (accumulates). 
    * 
    * @param result the feature result to add
    */
  def +(result: SpecResult): ResultsSummary = {
    new ResultsSummary(
      this.results ++ List(result), 
      addCounts(this.ruleCounts, result.ruleCounts),
      addCounts(this.scenarioCounts, result.scenarioCounts),
      addCounts(this.stepCounts, result.stepCounts),
      Nil)
  }

  private def addCounts(countsA: Map[StatusKeyword, Int], countsB: Map[StatusKeyword, Int]): Map[StatusKeyword, Int] =
    (StatusKeyword.reportables flatMap { status => 
      val a = countsA.getOrElse(status, 0)
      val b = countsB.getOrElse(status, 0)
      val sum = a + b
      if (sum > 0) Some((status, sum)) else None
    }).toMap

  def statsString: String = {
    statusCounts(withEmpty = true) map { (node, counts) =>
      val sum = counts.values.sum
      s"$sum ${node.toString.toLowerCase}${if (sum == 1) "" else "s"}: ${formatCounts(counts)}"
    } mkString ("\n")
  }

  def statusString: String = {
    s"""|[${Formatting.formatDuration(resultsElapsedTime)}] ${evalStatus.keyword}${if (sustainedCount > 0) s" with ${sustainedCount} sustained error${if (sustainedCount > 1) "s" else ""}" else ""} ${evalStatus.emoticon}
        |[${Formatting.formatDuration(elapsedTime)}] Elapsed, Started: $started, Finished: $finished""".stripMargin
  }

  def statusCounts(withEmpty: Boolean): List[(NodeType, Map[StatusKeyword, Int])] = {
    List(
      (NodeType.Feature, featureCounts),
      (NodeType.Rule, ruleCounts),
      (NodeType.Scenario, scenarioCounts),
      (NodeType.Step, stepCounts)
    ) filter { (nodeType, counts) => 
      withEmpty || counts.nonEmpty
    }
  }
  
  private def formatCounts(counts: Map[StatusKeyword, Int]): String = {
    StatusKeyword.reportables map { status =>
      val count = counts.getOrElse(status, 0)
      s"$status $count"
    } mkString ", "
  }

  def withReports(reports: List[ReportResult]): ResultsSummary = {
    ResultsSummary(results, ruleCounts, scenarioCounts, stepCounts, reports)
  }
  
}

/** Feature summary factory. */
object ResultsSummary {
  def apply(): ResultsSummary = new ResultsSummary(Nil, Map(), Map(), Map(), Nil)
  def apply(elapsedTime: Duration): ResultsSummary = new ResultsSummary(Nil, Map(), Map(), Map(), Nil)
  def apply(result: SpecResult): ResultsSummary = ResultsSummary() + result
}


