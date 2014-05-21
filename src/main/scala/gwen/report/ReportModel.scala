/*
 * Copyright 2014 Branko Juric, Brady Wood
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
import gwen.dsl.EvalStatus
import gwen.dsl.StatusKeyword
import gwen.Predefs.Kestrel
import gwen.dsl.FeatureSpec

/**
 * Captures the feature summary results of an evaluated feature.
 * 
 * @param featureResults
 * 			list of feature results
 * @param scenarioCounts
 * 			number of scenarios by status
 * @param stepCounts
 * 			number of steps by status
 * 
 * @author Branko Juric
 */
case class FeatureSummary(featureResults: List[FeatureResult], scenarioCounts: Map[StatusKeyword.Value, Int], stepCounts: Map[StatusKeyword.Value, Int]) {
  
  def accumulate(featureResult: FeatureResult, scenarioStatuses: List[EvalStatus], stepStatuses: List[EvalStatus]) =
    new FeatureSummary(
      this.featureResults ++ List(featureResult),
      addCounts(this.scenarioCounts, StatusKeyword.countsByStatus(scenarioStatuses)),
      addCounts(this.stepCounts, StatusKeyword.countsByStatus(stepStatuses)))
  
  def + (featureSummary: FeatureSummary): FeatureSummary = 
    new FeatureSummary(
      featureResults ::: featureSummary.featureResults,
      addCounts(this.scenarioCounts, featureSummary.scenarioCounts),
      addCounts(this.stepCounts, featureSummary.stepCounts))
  
  private def addCounts(countsA: Map[StatusKeyword.Value, Int], countsB: Map[StatusKeyword.Value, Int]): Map[StatusKeyword.Value, Int] =
    (StatusKeyword.values map { status => 
      val a = countsA.get(status).getOrElse(0)
      val b = countsB.get(status).getOrElse(0)
      (status, a + b)
    }).toMap
  
  override def toString = { 
    val featureCount = featureResults.size
    val scenarioCount = (scenarioCounts map { case (_, count) => count }).sum
    val stepCount = (stepCounts map { case (_, count) => count }).sum
    s"""|${featureCount} feature${if (featureCount == 1) "" else "s"}: ${formatCounts(StatusKeyword.countsByStatus(featureResults.map(_.evalStatus)))} 
        |${scenarioCount} scenario${if (scenarioCount == 1) "" else "s"}: ${formatCounts(scenarioCounts)}
        |${stepCount} step${if (stepCount == 1) "" else "s"}: ${formatCounts(stepCounts)}""".stripMargin
  }
  
  private def formatCounts(counts: Map[StatusKeyword.Value, Int]) = 
    StatusKeyword.valuesFixedOrder map { status =>
      val count = counts.get(status).getOrElse(0)
      s"${status} ${count}"
    } mkString(", ")
  
}

/**
 * Feature summary factory.
 */
object FeatureSummary {
  def apply(): FeatureSummary = new FeatureSummary(Nil, Map(), Map())
  def apply(spec: FeatureSpec, reportFile: Option[File]): FeatureSummary =
    FeatureSummary().accumulate(
       FeatureResult(spec.feature.name, spec.evalStatus, spec.featureFile, reportFile), 
       spec.scenarios.map(_.evalStatus), 
       spec.scenarios.flatMap(_.allSteps.map(_.evalStatus)))
}

/**
 * Captures the results of an evaluated feature.
 * 
 * @param name
 * 			the feature name
 * @param evalStatus
 * 			the evaluated status
 * @param featureFile
 * 			the optional feature file
 * @param reportFile
 * 			feature report file
 */
case class FeatureResult(featureName: String, evalStatus: EvalStatus, featureFile: Option[File], reportFile: Option[File])

/**
 * Feature result factory.
 */
object FeatureResult {
  def apply(spec: FeatureSpec, reportFile: Option[File]) = 
    new FeatureResult(spec.feature.name, spec.evalStatus, spec.featureFile, reportFile)
}