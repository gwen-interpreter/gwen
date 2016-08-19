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

package gwen.eval

import java.io.File
import org.scalatest.Matchers
import gwen.dsl.EvalStatus
import gwen.dsl.Failed
import gwen.dsl.Passed
import gwen.dsl.Pending
import gwen.dsl.Skipped
import gwen.dsl.StatusKeyword
import org.scalatest.FlatSpec
import gwen.dsl.FeatureSpec
import gwen.dsl.Scenario
import gwen.dsl.Step
import gwen.dsl.StepKeyword
import gwen.dsl.Feature
import gwen.dsl.Tag
import scala.concurrent.duration.Duration
import java.util.concurrent.TimeUnit
import gwen.dsl.Loaded

class FeatureSummaryTest extends FlatSpec with Matchers {
  
  val Passed1 = Passed(1000000)
  val Passed2 = Passed(2000000)
  val Passed3 = Passed(3000000)
  val Passed4 = Passed(4000000)
  
  val Failed3 = Failed(3000000, new Exception())
  val Failed4 = Failed(4000000, new Exception())

  "No results in summary" should "yield empty metrics" in {
    val summary = FeatureSummary(Duration.Zero)
    summary.results.size should be (0)
    summary.featureCounts.size should be (0)
    summary.scenarioCounts.size should be (0)
    summary.stepCounts.size should be (0)
    val summaryLines = summary.toString.split("\\r?\\n");
    summaryLines.size should be (5)
    summaryLines(0) should be ("0 features: Passed 0, Failed 0, Skipped 0, Pending 0")
    summaryLines(1) should be ("0 scenarios: Passed 0, Failed 0, Skipped 0, Pending 0")
    summaryLines(2) should be ("0 steps: Passed 0, Failed 0, Skipped 0, Pending 0")
    summaryLines(3) should be ("")
    summaryLines(4).startsWith("[~0ms] Passed") should be (true)
  }
  
  "Accumulated feature results in summary" should "sum correctly" in {
    
    var summary = FeatureSummary(Duration.Zero)
    var summaryLines = Array[String]()
    
    // add 1 meta
    val meta1 = FeatureSpec(
      Feature("meta1", Nil), None, List(
        Scenario(Set[Tag](), "metaScenario1", Nil, None, List(
          Step(StepKeyword.Given, "meta step 1", Passed2),
          Step(StepKeyword.Given, "meta step 2", Passed1),
          Step(StepKeyword.Given, "meta step 3", Passed2))
        ),
        Scenario(Set(Tag("StepDef")), "metaStepDef1", Nil, None, List(
          Step(StepKeyword.Given, "step 1", Loaded),
          Step(StepKeyword.Given, "step 2", Loaded),
          Step(StepKeyword.Given, "step 3", Loaded))
        )))
        
    // add 1 passed scenario
    val feature1 = FeatureSpec(
      Feature("feature1", Nil), None, List(
        Scenario(Set[Tag](), "scenario1", Nil, None, List(
          Step(StepKeyword.Given, "step 1", Passed2),
          Step(StepKeyword.Given, "step 2", Passed1),
          Step(StepKeyword.Given, "step 3", Passed2))
        ),
        Scenario(Set(Tag("StepDef")), "StepDef1", Nil, None, List(
          Step(StepKeyword.Given, "step 1", Loaded),
          Step(StepKeyword.Given, "step 2", Loaded),
          Step(StepKeyword.Given, "step 3", Loaded))
        )),
      None,
      List(meta1))
        
    val metaResult = FeatureResult(meta1, None, Nil, Duration.Zero)
    var featureResult = FeatureResult(feature1, None, List(metaResult), Duration(10, TimeUnit.MILLISECONDS))
    summary = summary + (featureResult, featureResult.elapsedTime)
    EvalStatus(summary.results.map(_.spec.evalStatus)).status should be (StatusKeyword.Passed)
    summary.results.size should be (1)
    summary.featureCounts should equal (Map((StatusKeyword.Passed -> 1)))
    summary.scenarioCounts should equal (Map((StatusKeyword.Passed -> 1)))
    summary.stepCounts should equal (Map((StatusKeyword.Passed -> 3)))
    summaryLines = summary.toString.split("\\r?\\n");
    summaryLines.size should be (5)
    summaryLines(0) should be ("1 feature: Passed 1, Failed 0, Skipped 0, Pending 0")
    summaryLines(1) should be ("1 scenario: Passed 1, Failed 0, Skipped 0, Pending 0")
    summaryLines(2) should be ("3 steps: Passed 3, Failed 0, Skipped 0, Pending 0")
    summaryLines(3) should be ("")
    summaryLines(4).startsWith("[10ms] Passed") should be (true)
    
    // add 1 failed scenario
    val feature2 = FeatureSpec(
      Feature("feature2", Nil), None, List(
        Scenario(Set[Tag](), "scenario1", Nil, None, List(
          Step(StepKeyword.Given, "step 1", Passed2),
          Step(StepKeyword.Given, "step 2", Failed3),
          Step(StepKeyword.Given, "step 3", Skipped))
        )))
    featureResult = FeatureResult(feature2, None, Nil, Duration(10, TimeUnit.MILLISECONDS))
    summary = summary + (featureResult, featureResult.elapsedTime)
    EvalStatus(summary.results.map(_.spec.evalStatus)).status should be (StatusKeyword.Failed)
    summary.results.size should be (2)
    summary.featureCounts should equal (Map((StatusKeyword.Passed -> 1), (StatusKeyword.Failed -> 1)))
    summary.scenarioCounts should equal (Map((StatusKeyword.Passed -> 1), (StatusKeyword.Failed -> 1)))
    summary.stepCounts should equal (Map((StatusKeyword.Passed -> 4), (StatusKeyword.Failed -> 1), (StatusKeyword.Skipped -> 1)))
    summaryLines = summary.toString.split("\\r?\\n");
    summaryLines.size should be (5)
    summaryLines(0) should be ("2 features: Passed 1, Failed 1, Skipped 0, Pending 0")
    summaryLines(1) should be ("2 scenarios: Passed 1, Failed 1, Skipped 0, Pending 0")
    summaryLines(2) should be ("6 steps: Passed 4, Failed 1, Skipped 1, Pending 0")
    summaryLines(3) should be ("")
    summaryLines(4).startsWith("[15ms] Failed") should be (true)
    
    // add 2 passed scenarios
    val feature3 = FeatureSpec(
      Feature("feature3", Nil), None, List(
        Scenario(Set[Tag](), "scenario1", Nil, None, List(
          Step(StepKeyword.Given, "step 1", Passed2),
          Step(StepKeyword.Given, "step 2", Passed1),
          Step(StepKeyword.Given, "step 3", Passed2))
        ), 
        Scenario(Set[Tag](), "scenario2", Nil, None, List(
          Step(StepKeyword.Given, "step 1", Passed2),
          Step(StepKeyword.Given, "step 2", Passed1),
          Step(StepKeyword.Given, "step 3", Passed2))
        )))
    featureResult = FeatureResult(feature3, None, Nil, Duration(10, TimeUnit.MILLISECONDS))
    summary = summary + (featureResult, featureResult.elapsedTime)
    EvalStatus(summary.results.map(_.spec.evalStatus)).status should be (StatusKeyword.Failed)
    summary.results.size should be (3)
    summary.featureCounts should equal (Map((StatusKeyword.Passed -> 2), (StatusKeyword.Failed -> 1)))
    summary.scenarioCounts should equal (Map((StatusKeyword.Passed -> 3), (StatusKeyword.Failed -> 1)))
    summary.stepCounts should equal (Map((StatusKeyword.Passed -> 10), (StatusKeyword.Failed -> 1), (StatusKeyword.Skipped -> 1)))
    summaryLines = summary.toString.split("\\r?\\n");
    summaryLines.size should be (5)
    summaryLines(0) should be ("3 features: Passed 2, Failed 1, Skipped 0, Pending 0")
    summaryLines(1) should be ("4 scenarios: Passed 3, Failed 1, Skipped 0, Pending 0")
    summaryLines(2) should be ("12 steps: Passed 10, Failed 1, Skipped 1, Pending 0")
    summaryLines(3) should be ("")
    summaryLines(4).startsWith("[25ms] Failed") should be (true)
    
    // add 1 skipped scenario
    val feature4 = FeatureSpec(
      Feature("feature4", Nil), None, List(
        Scenario(Set[Tag](), "scenario1", Nil, None, List(
          Step(StepKeyword.Given, "step 1", Skipped),
          Step(StepKeyword.Given, "step 2", Skipped),
          Step(StepKeyword.Given, "step 3", Skipped))
        )))
    featureResult = FeatureResult(feature4, None, Nil, Duration(10, TimeUnit.MILLISECONDS))
    summary = summary + (featureResult, featureResult.elapsedTime)
    EvalStatus(summary.results.map(_.spec.evalStatus)).status should be (StatusKeyword.Failed)
    summary.results.size should be (4)
    summary.featureCounts should equal (Map((StatusKeyword.Passed -> 2), (StatusKeyword.Failed -> 1), (StatusKeyword.Skipped -> 1)))
    summary.scenarioCounts should equal (Map((StatusKeyword.Passed -> 3), (StatusKeyword.Failed -> 1), (StatusKeyword.Skipped -> 1)))
    summary.stepCounts should equal (Map((StatusKeyword.Passed -> 10), (StatusKeyword.Failed -> 1), (StatusKeyword.Skipped -> 4)))
    summaryLines = summary.toString.split("\\r?\\n");
    summaryLines.size should be (5)
    summaryLines(0) should be ("4 features: Passed 2, Failed 1, Skipped 1, Pending 0")
    summaryLines(1) should be ("5 scenarios: Passed 3, Failed 1, Skipped 1, Pending 0")
    summaryLines(2) should be ("15 steps: Passed 10, Failed 1, Skipped 4, Pending 0")
    summaryLines(3) should be ("")
    summaryLines(4).startsWith("[25ms] Failed") should be (true)
    
    // add 1 pending scenario
    val feature5 = FeatureSpec(
      Feature("feature5", Nil), None, List(
        Scenario(Set[Tag](), "scenario1", Nil, None, List(
          Step(StepKeyword.Given, "step 1", Pending),
          Step(StepKeyword.Given, "step 2", Pending))
        )))
    featureResult = FeatureResult(feature5, None, Nil, Duration(10, TimeUnit.MILLISECONDS))
    summary = summary + (featureResult, featureResult.elapsedTime)
    EvalStatus(summary.results.map(_.spec.evalStatus)).status should be (StatusKeyword.Failed)
    summary.results.size should be (5)
    summary.featureCounts should equal (Map((StatusKeyword.Passed -> 2), (StatusKeyword.Failed -> 1), (StatusKeyword.Skipped -> 1), (StatusKeyword.Pending -> 1)))
    summary.scenarioCounts should equal (Map((StatusKeyword.Passed -> 3), (StatusKeyword.Failed -> 1), (StatusKeyword.Skipped -> 1), (StatusKeyword.Pending -> 1)))
    summary.stepCounts should equal (Map((StatusKeyword.Passed -> 10), (StatusKeyword.Failed -> 1), (StatusKeyword.Skipped -> 4), (StatusKeyword.Pending -> 2)))
    summaryLines = summary.toString.split("\\r?\\n");
    summaryLines.size should be (5)
    summaryLines(0) should be ("5 features: Passed 2, Failed 1, Skipped 1, Pending 1")
    summaryLines(1) should be ("6 scenarios: Passed 3, Failed 1, Skipped 1, Pending 1")
    summaryLines(2) should be ("17 steps: Passed 10, Failed 1, Skipped 4, Pending 2")
    summaryLines(3) should be ("")
    summaryLines(4).startsWith("[25ms] Failed") should be (true)
    
    // add 4 passed and 1 failed scenario
    val feature6 = FeatureSpec(
      Feature("feature6", Nil), None, List(
        Scenario(Set[Tag](), "scenario1", Nil, None, List(
          Step(StepKeyword.Given, "step 1", Passed2),
          Step(StepKeyword.Given, "step 2", Passed1),
          Step(StepKeyword.Given, "step 3", Passed2))
        ), 
        Scenario(Set[Tag](), "scenario2", Nil, None, List(
          Step(StepKeyword.Given, "step 1", Passed4),
          Step(StepKeyword.Given, "step 2", Passed1))
        ),
        Scenario(Set[Tag](), "scenario3", Nil, None, List(
          Step(StepKeyword.Given, "step 1", Passed2),
          Step(StepKeyword.Given, "step 2", Passed1),
          Step(StepKeyword.Given, "step 3", Passed2))
        ),
        Scenario(Set[Tag](), "scenario4", Nil, None, List(
          Step(StepKeyword.Given, "step 1", Passed2),
          Step(StepKeyword.Given, "step 2", Passed3))
        ),
        Scenario(Set[Tag](), "scenario5", Nil, None, List(
          Step(StepKeyword.Given, "step 1", Passed1),
          Step(StepKeyword.Given, "step 2", Failed4),
          Step(StepKeyword.Given, "step 3", Skipped),
          Step(StepKeyword.Given, "step 3", Skipped))
        )))
    featureResult = FeatureResult(feature6, None, Nil, Duration(10, TimeUnit.MILLISECONDS))
    summary = summary + (featureResult, featureResult.elapsedTime)
    EvalStatus(summary.results.map(_.spec.evalStatus)).status should be (StatusKeyword.Failed)
    summary.results.size should be (6)
    summary.featureCounts should equal (Map((StatusKeyword.Passed -> 2), (StatusKeyword.Failed -> 2), (StatusKeyword.Skipped -> 1), (StatusKeyword.Pending -> 1)))
    summary.scenarioCounts should equal (Map((StatusKeyword.Passed -> 7), (StatusKeyword.Failed -> 2), (StatusKeyword.Skipped -> 1), (StatusKeyword.Pending -> 1)))
    summary.stepCounts should equal (Map((StatusKeyword.Passed -> 21), (StatusKeyword.Failed -> 2), (StatusKeyword.Skipped -> 6), (StatusKeyword.Pending -> 2)))
    summaryLines = summary.toString.split("\\r?\\n");
    summaryLines.size should be (5)
    summaryLines(0) should be ("6 features: Passed 2, Failed 2, Skipped 1, Pending 1")
    summaryLines(1) should be ("11 scenarios: Passed 7, Failed 2, Skipped 1, Pending 1")
    summaryLines(2) should be ("31 steps: Passed 21, Failed 2, Skipped 6, Pending 2")
    summaryLines(3) should be ("")
    summaryLines(4).startsWith("[50ms] Failed") should be (true)
    
  }
  
}