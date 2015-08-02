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

class FeatureSummaryTest extends FlatSpec with Matchers {

  "Accumulated feature results in summary" should "sum correctly" in {
    
    var summary = FeatureSummary()
    var summaryLines = Array[String]()
    
    // add 1 passed scenario
    val feature1 = FeatureSpec(
      Feature("feature1", Nil), None, List(
        Scenario(Set[Tag](), "scenario1", None, List(
          Step(StepKeyword.Given, "step 1", Passed(2)),
          Step(StepKeyword.Given, "step 2", Passed(1)),
          Step(StepKeyword.Given, "step 3", Passed(2)))
        )))
    summary = summary + FeatureResult(feature1, None, Nil)
    EvalStatus(summary.summaryLines.map(_.evalStatus)).status should be (StatusKeyword.Passed)
    summary.summaryLines.size should be (1)
    summary.featureCounts should equal (Map((StatusKeyword.Passed -> 1)))
    summary.scenarioCounts should equal (Map((StatusKeyword.Passed -> 1)))
    summary.stepCounts should equal (Map((StatusKeyword.Passed -> 3)))
    summaryLines = summary.toString.split("\\r?\\n");
    summaryLines(0) should be ("1 feature: Passed 1, Failed 0, Skipped 0, Pending 0")
    summaryLines(1) should be ("1 scenario: Passed 1, Failed 0, Skipped 0, Pending 0")
    summaryLines(2) should be ("3 steps: Passed 3, Failed 0, Skipped 0, Pending 0")
    
    // add 1 failed scenario
    val feature2 = FeatureSpec(
      Feature("feature2", Nil), None, List(
        Scenario(Set[Tag](), "scenario1", None, List(
          Step(StepKeyword.Given, "step 1", Passed(2)),
          Step(StepKeyword.Given, "step 2", Failed(3, new Exception())),
          Step(StepKeyword.Given, "step 3", Skipped))
        )))
    summary = summary + FeatureResult(feature2, None, Nil)
    EvalStatus(summary.summaryLines.map(_.evalStatus)).status should be (StatusKeyword.Failed)
    summary.summaryLines.size should be (2)
    summary.featureCounts should equal (Map((StatusKeyword.Passed -> 1), (StatusKeyword.Failed -> 1)))
    summary.scenarioCounts should equal (Map((StatusKeyword.Passed -> 1), (StatusKeyword.Failed -> 1)))
    summary.stepCounts should equal (Map((StatusKeyword.Passed -> 4), (StatusKeyword.Failed -> 1), (StatusKeyword.Skipped -> 1)))
    summaryLines = summary.toString.split("\\r?\\n");
    summaryLines(0) should be ("2 features: Passed 1, Failed 1, Skipped 0, Pending 0")
    summaryLines(1) should be ("2 scenarios: Passed 1, Failed 1, Skipped 0, Pending 0")
    summaryLines(2) should be ("6 steps: Passed 4, Failed 1, Skipped 1, Pending 0")
    
    // add 2 passed scenarios
    val feature3 = FeatureSpec(
      Feature("feature3", Nil), None, List(
        Scenario(Set[Tag](), "scenario1", None, List(
          Step(StepKeyword.Given, "step 1", Passed(2)),
          Step(StepKeyword.Given, "step 2", Passed(1)),
          Step(StepKeyword.Given, "step 3", Passed(2)))
        ), 
        Scenario(Set[Tag](), "scenario2", None, List(
          Step(StepKeyword.Given, "step 1", Passed(2)),
          Step(StepKeyword.Given, "step 2", Passed(1)),
          Step(StepKeyword.Given, "step 3", Passed(2)))
        )))
    summary = summary + FeatureResult(feature3, None, Nil)
    EvalStatus(summary.summaryLines.map(_.evalStatus)).status should be (StatusKeyword.Failed)
    summary.summaryLines.size should be (3)
    summary.featureCounts should equal (Map((StatusKeyword.Passed -> 2), (StatusKeyword.Failed -> 1)))
    summary.scenarioCounts should equal (Map((StatusKeyword.Passed -> 3), (StatusKeyword.Failed -> 1)))
    summary.stepCounts should equal (Map((StatusKeyword.Passed -> 10), (StatusKeyword.Failed -> 1), (StatusKeyword.Skipped -> 1)))
    summaryLines = summary.toString.split("\\r?\\n");
    summaryLines(0) should be ("3 features: Passed 2, Failed 1, Skipped 0, Pending 0")
    summaryLines(1) should be ("4 scenarios: Passed 3, Failed 1, Skipped 0, Pending 0")
    summaryLines(2) should be ("12 steps: Passed 10, Failed 1, Skipped 1, Pending 0")
    
    // add 1 skipped scenario
    val feature4 = FeatureSpec(
      Feature("feature4", Nil), None, List(
        Scenario(Set[Tag](), "scenario1", None, List(
          Step(StepKeyword.Given, "step 1", Skipped),
          Step(StepKeyword.Given, "step 2", Skipped),
          Step(StepKeyword.Given, "step 3", Skipped))
        )))
    summary = summary + FeatureResult(feature4, None, Nil)
    EvalStatus(summary.summaryLines.map(_.evalStatus)).status should be (StatusKeyword.Failed)
    summary.summaryLines.size should be (4)
    summary.featureCounts should equal (Map((StatusKeyword.Passed -> 2), (StatusKeyword.Failed -> 1), (StatusKeyword.Skipped -> 1)))
    summary.scenarioCounts should equal (Map((StatusKeyword.Passed -> 3), (StatusKeyword.Failed -> 1), (StatusKeyword.Skipped -> 1)))
    summary.stepCounts should equal (Map((StatusKeyword.Passed -> 10), (StatusKeyword.Failed -> 1), (StatusKeyword.Skipped -> 4)))
    summaryLines = summary.toString.split("\\r?\\n");
    summaryLines(0) should be ("4 features: Passed 2, Failed 1, Skipped 1, Pending 0")
    summaryLines(1) should be ("5 scenarios: Passed 3, Failed 1, Skipped 1, Pending 0")
    summaryLines(2) should be ("15 steps: Passed 10, Failed 1, Skipped 4, Pending 0")
    
    // add 1 pending scenario
    val feature5 = FeatureSpec(
      Feature("feature5", Nil), None, List(
        Scenario(Set[Tag](), "scenario1", None, List(
          Step(StepKeyword.Given, "step 1", Pending),
          Step(StepKeyword.Given, "step 2", Pending))
        )))
    summary = summary + FeatureResult(feature5, None, Nil)
    EvalStatus(summary.summaryLines.map(_.evalStatus)).status should be (StatusKeyword.Failed)
    summary.summaryLines.size should be (5)
    summary.featureCounts should equal (Map((StatusKeyword.Passed -> 2), (StatusKeyword.Failed -> 1), (StatusKeyword.Skipped -> 1), (StatusKeyword.Pending -> 1)))
    summary.scenarioCounts should equal (Map((StatusKeyword.Passed -> 3), (StatusKeyword.Failed -> 1), (StatusKeyword.Skipped -> 1), (StatusKeyword.Pending -> 1)))
    summary.stepCounts should equal (Map((StatusKeyword.Passed -> 10), (StatusKeyword.Failed -> 1), (StatusKeyword.Skipped -> 4), (StatusKeyword.Pending -> 2)))
    summaryLines = summary.toString.split("\\r?\\n");
    summaryLines(0) should be ("5 features: Passed 2, Failed 1, Skipped 1, Pending 1")
    summaryLines(1) should be ("6 scenarios: Passed 3, Failed 1, Skipped 1, Pending 1")
    summaryLines(2) should be ("17 steps: Passed 10, Failed 1, Skipped 4, Pending 2")
    
    // add 4 passed and 1 failed scenario
    val feature6 = FeatureSpec(
      Feature("feature6", Nil), None, List(
        Scenario(Set[Tag](), "scenario1", None, List(
          Step(StepKeyword.Given, "step 1", Passed(2)),
          Step(StepKeyword.Given, "step 2", Passed(1)),
          Step(StepKeyword.Given, "step 3", Passed(2)))
        ), 
        Scenario(Set[Tag](), "scenario2", None, List(
          Step(StepKeyword.Given, "step 1", Passed(4)),
          Step(StepKeyword.Given, "step 2", Passed(1)))
        ),
        Scenario(Set[Tag](), "scenario3", None, List(
          Step(StepKeyword.Given, "step 1", Passed(2)),
          Step(StepKeyword.Given, "step 2", Passed(1)),
          Step(StepKeyword.Given, "step 3", Passed(2)))
        ),
        Scenario(Set[Tag](), "scenario4", None, List(
          Step(StepKeyword.Given, "step 1", Passed(2)),
          Step(StepKeyword.Given, "step 2", Passed(3)))
        ),
        Scenario(Set[Tag](), "scenario5", None, List(
          Step(StepKeyword.Given, "step 1", Passed(1)),
          Step(StepKeyword.Given, "step 2", Failed(4, new Exception())),
          Step(StepKeyword.Given, "step 3", Skipped),
          Step(StepKeyword.Given, "step 3", Skipped))
        )))
    summary = summary + FeatureResult(feature6, None, Nil)
    EvalStatus(summary.summaryLines.map(_.evalStatus)).status should be (StatusKeyword.Failed)
    summary.summaryLines.size should be (6)
    summary.featureCounts should equal (Map((StatusKeyword.Passed -> 2), (StatusKeyword.Failed -> 2), (StatusKeyword.Skipped -> 1), (StatusKeyword.Pending -> 1)))
    summary.scenarioCounts should equal (Map((StatusKeyword.Passed -> 7), (StatusKeyword.Failed -> 2), (StatusKeyword.Skipped -> 1), (StatusKeyword.Pending -> 1)))
    summary.stepCounts should equal (Map((StatusKeyword.Passed -> 21), (StatusKeyword.Failed -> 2), (StatusKeyword.Skipped -> 6), (StatusKeyword.Pending -> 2)))
    summaryLines = summary.toString.split("\\r?\\n");
    summaryLines(0) should be ("6 features: Passed 2, Failed 2, Skipped 1, Pending 1")
    summaryLines(1) should be ("11 scenarios: Passed 7, Failed 2, Skipped 1, Pending 1")
    summaryLines(2) should be ("31 steps: Passed 21, Failed 2, Skipped 6, Pending 2")
    
  }
  
}