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
import gwen.core.node.GwenNode
import gwen.core.node.NodeType
import gwen.core.node.gherkin.Spec
import gwen.core.report.ReportFormat
import gwen.core.status._

import scala.concurrent.duration._

import java.io.File
import java.util.Date

/**
  * Captures the results of an evaluated specification.
  * 
  * @param spec the evaluated feature
  * @param metaResults the evaluated meta results
  * @param reports optional map of report files (keyed by report type)
  * @param started the started time
  * @param finished the finished time
  */
class SpecResult(
  val spec: Spec, 
  val reports: Option[Map[ReportFormat.Value, List[File]]], 
  val metaResults: List[SpecResult],
  val started: Date,
  val finished: Date) extends GwenNode {
  
  val nodeType: NodeType.Value = NodeType.Result

  lazy val elapsedTime = Duration(finished.getTime - started.getTime, MILLISECONDS)
  lazy val screenshots: List[File] = spec.attachments.filter(_._1 == "Screenshot").map(_._2)
  lazy val isMeta: Boolean = spec.specFile.exists(_.getName.endsWith(".meta"))
  lazy val summary = ResultsSummary(this)
  lazy val evalStatus: EvalStatus = spec.evalStatus
  lazy val duration: Duration = evalStatus.duration
  lazy val overhead: Duration = elapsedTime - duration - DurationOps.sum(metaResults.map(_.overhead))
  lazy val sustainedCount: Int = spec.sustainedCount
  
  private [result] lazy val scenarioCounts = 
    EvalStatus.countsByType(spec.evalScenarios.flatMap { s =>
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

  private [result] lazy val ruleCounts =
    EvalStatus.countsByType(spec.rules.map(_.evalStatus))

  private [result] lazy val stepCounts = EvalStatus.countsByType(spec.evalScenarios.flatMap(_.allSteps.map(_.evalStatus)))
  override def toString: String = s"""[${Formatting.formatDuration(duration)}] ${evalStatus.keyword}${if (sustainedCount > 0) s" with ${sustainedCount} sustained error${if (sustainedCount > 1) "s" else ""}" else ""} ${evalStatus.emoticon}, [${Formatting.formatDuration(overhead)}] Overhead, [${Formatting.formatDuration(elapsedTime)}] Elapsed, Started: $started, Finished: $finished""".stripMargin
}

