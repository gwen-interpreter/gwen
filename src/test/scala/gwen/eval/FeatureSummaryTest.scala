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
    
    // add 1 passed scenario
    val feature1 = FeatureSpec(
      Feature("feature1", Nil), None, List(
        Scenario(Set[Tag](), "scenario1", None, List(
          Step(StepKeyword.Given, "step 1", Passed(2)),
          Step(StepKeyword.Given, "step 2", Passed(1)),
          Step(StepKeyword.Given, "step 3", Passed(2)))
        )))
    summary = summary + new FeatureResult(feature1, Nil, None)
    EvalStatus(summary.featureResults.map(_.evalStatus)).status should be (StatusKeyword.Passed)
    summary.featureResults.filter(_.evalStatus.status == StatusKeyword.Passed).size should be (1)
    summary.featureResults.filter(_.evalStatus.status == StatusKeyword.Failed).size should be (0)
    summary.featureResults.filter(_.evalStatus.status == StatusKeyword.Skipped).size should be (0)
    summary.featureResults.filter(_.evalStatus.status == StatusKeyword.Pending).size should be (0)
    summary.featureResults.size should be (1)
    summary.scenarioCounts(StatusKeyword.Passed) should be (1)
    summary.scenarioCounts(StatusKeyword.Failed) should be (0)
    summary.scenarioCounts(StatusKeyword.Skipped) should be (0)
    summary.scenarioCounts(StatusKeyword.Pending) should be (0)
    summary.stepCounts(StatusKeyword.Passed) should be (3)
    summary.stepCounts(StatusKeyword.Failed) should be (0)
    summary.stepCounts(StatusKeyword.Skipped) should be (0)
    summary.stepCounts(StatusKeyword.Pending) should be (0)
    
    // add 1 failed scenario
    val feature2 = FeatureSpec(
      Feature("feature2", Nil), None, List(
        Scenario(Set[Tag](), "scenario1", None, List(
          Step(StepKeyword.Given, "step 1", Passed(2)),
          Step(StepKeyword.Given, "step 2", Failed(3, new Exception())),
          Step(StepKeyword.Given, "step 3", Skipped))
        )))
    summary = summary + new FeatureResult(feature2 ,Nil, None)
    EvalStatus(summary.featureResults.map(_.evalStatus)).status should be (StatusKeyword.Failed)
    summary.featureResults.filter(_.evalStatus.status == StatusKeyword.Passed).size should be (1)
    summary.featureResults.filter(_.evalStatus.status == StatusKeyword.Failed).size should be (1)
    summary.featureResults.filter(_.evalStatus.status == StatusKeyword.Skipped).size should be (0)
    summary.featureResults.filter(_.evalStatus.status == StatusKeyword.Pending).size should be (0)
    summary.featureResults.size should be (2)
    summary.scenarioCounts(StatusKeyword.Passed) should be (1)
    summary.scenarioCounts(StatusKeyword.Failed) should be (1)
    summary.scenarioCounts(StatusKeyword.Skipped) should be (0)
    summary.scenarioCounts(StatusKeyword.Pending) should be (0)
    summary.stepCounts(StatusKeyword.Passed) should be (4)
    summary.stepCounts(StatusKeyword.Failed) should be (1)
    summary.stepCounts(StatusKeyword.Skipped) should be (1)
    summary.stepCounts(StatusKeyword.Pending) should be (0)
    
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
    summary = summary + new FeatureResult(feature3 ,Nil, None)
    EvalStatus(summary.featureResults.map(_.evalStatus)).status should be (StatusKeyword.Failed)
    summary.featureResults.filter(_.evalStatus.status == StatusKeyword.Passed).size should be (2)
    summary.featureResults.filter(_.evalStatus.status == StatusKeyword.Failed).size should be (1)
    summary.featureResults.filter(_.evalStatus.status == StatusKeyword.Skipped).size should be (0)
    summary.featureResults.filter(_.evalStatus.status == StatusKeyword.Pending).size should be (0)
    summary.featureResults.size should be (3)
    summary.scenarioCounts(StatusKeyword.Passed) should be (3)
    summary.scenarioCounts(StatusKeyword.Failed) should be (1)
    summary.scenarioCounts(StatusKeyword.Skipped) should be (0)
    summary.scenarioCounts(StatusKeyword.Pending) should be (0)
    summary.stepCounts(StatusKeyword.Passed) should be (10)
    summary.stepCounts(StatusKeyword.Failed) should be (1)
    summary.stepCounts(StatusKeyword.Skipped) should be (1)
    summary.stepCounts(StatusKeyword.Pending) should be (0)
    
    // add 1 skipped scenario
    val feature4 = FeatureSpec(
      Feature("feature4", Nil), None, List(
        Scenario(Set[Tag](), "scenario1", None, List(
          Step(StepKeyword.Given, "step 1", Skipped),
          Step(StepKeyword.Given, "step 2", Skipped),
          Step(StepKeyword.Given, "step 3", Skipped))
        )))
    summary = summary + new FeatureResult(feature4 ,Nil, None)
    EvalStatus(summary.featureResults.map(_.evalStatus)).status should be (StatusKeyword.Failed)
    summary.featureResults.filter(_.evalStatus.status == StatusKeyword.Passed).size should be (2)
    summary.featureResults.filter(_.evalStatus.status == StatusKeyword.Failed).size should be (1)
    summary.featureResults.filter(_.evalStatus.status == StatusKeyword.Skipped).size should be (1)
    summary.featureResults.filter(_.evalStatus.status == StatusKeyword.Pending).size should be (0)    
    summary.featureResults.size should be (4)
    summary.scenarioCounts(StatusKeyword.Passed) should be (3)
    summary.scenarioCounts(StatusKeyword.Failed) should be (1)
    summary.scenarioCounts(StatusKeyword.Skipped) should be (1)
    summary.scenarioCounts(StatusKeyword.Pending) should be (0)
    summary.stepCounts(StatusKeyword.Passed) should be (10)
    summary.stepCounts(StatusKeyword.Failed) should be (1)
    summary.stepCounts(StatusKeyword.Skipped) should be (4)
    summary.stepCounts(StatusKeyword.Pending) should be (0)
    
    // add 1 pending scenario
    val feature5 = FeatureSpec(
      Feature("feature5", Nil), None, List(
        Scenario(Set[Tag](), "scenario1", None, List(
          Step(StepKeyword.Given, "step 1", Pending),
          Step(StepKeyword.Given, "step 2", Pending))
        )))
    summary = summary + new FeatureResult(feature5 ,Nil, None)
    EvalStatus(summary.featureResults.map(_.evalStatus)).status should be (StatusKeyword.Failed)
    summary.featureResults.filter(_.evalStatus.status == StatusKeyword.Passed).size should be (2)
    summary.featureResults.filter(_.evalStatus.status == StatusKeyword.Failed).size should be (1)
    summary.featureResults.filter(_.evalStatus.status == StatusKeyword.Skipped).size should be (1)
    summary.featureResults.filter(_.evalStatus.status == StatusKeyword.Pending).size should be (1)
    summary.featureResults.size should be (5)
    summary.scenarioCounts(StatusKeyword.Passed) should be (3)
    summary.scenarioCounts(StatusKeyword.Failed) should be (1)
    summary.scenarioCounts(StatusKeyword.Skipped) should be (1)
    summary.scenarioCounts(StatusKeyword.Pending) should be (1)
    summary.stepCounts(StatusKeyword.Passed) should be (10)
    summary.stepCounts(StatusKeyword.Failed) should be (1)
    summary.stepCounts(StatusKeyword.Skipped) should be (4)
    summary.stepCounts(StatusKeyword.Pending) should be (2)
    
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
    summary = summary + new FeatureResult(feature6 ,Nil, None)
    EvalStatus(summary.featureResults.map(_.evalStatus)).status should be (StatusKeyword.Failed)
    summary.featureResults.filter(_.evalStatus.status == StatusKeyword.Passed).size should be (2)
    summary.featureResults.filter(_.evalStatus.status == StatusKeyword.Failed).size should be (2)
    summary.featureResults.filter(_.evalStatus.status == StatusKeyword.Skipped).size should be (1)
    summary.featureResults.filter(_.evalStatus.status == StatusKeyword.Pending).size should be (1)
    summary.featureResults.size should be (6)
    summary.scenarioCounts(StatusKeyword.Passed) should be (7)
    summary.scenarioCounts(StatusKeyword.Failed) should be (2)
    summary.scenarioCounts(StatusKeyword.Skipped) should be (1)
    summary.scenarioCounts(StatusKeyword.Pending) should be (1)
    summary.stepCounts(StatusKeyword.Passed) should be (21)
    summary.stepCounts(StatusKeyword.Failed) should be (2)
    summary.stepCounts(StatusKeyword.Skipped) should be (6)
    summary.stepCounts(StatusKeyword.Pending) should be (2)
    
  }
  
}