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
import org.scalatest.matchers.ShouldMatchers
import gwen.dsl.EvalStatus
import gwen.dsl.Failed
import gwen.dsl.Passed
import gwen.dsl.Pending
import gwen.dsl.Skipped
import gwen.dsl.StatusKeyword
import org.scalatest.FlatSpec

class FeatureSummaryTest extends FlatSpec with ShouldMatchers {

  "Accumulated feature results in summary" should "sum correctly" in {
    
    var summary = FeatureSummary()
    
    // add 1 passed scenario
    summary = summary.accumulate(
      FeatureResult("feature1", Passed(10), Some(new File("f1.feature")), Some(new File("f1.html"))),
      List(Passed(5)),
      List(Passed(2), Passed(1), Passed(2)))
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
    summary = summary.accumulate(
      FeatureResult("feature2", Failed(5, new Exception()), Some(new File("f2.feature")), Some(new File("f2.html"))),
      List(Failed(5, new Exception())),
      List(Passed(2), Failed(3, new Exception()), Skipped))
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
    summary = summary.accumulate(
      FeatureResult("feature3", Passed(10), Some(new File("f3.feature")), Some(new File("f3.html"))),
      List(Passed(5), Passed(5)),
      List(Passed(2), Passed(1), Passed(2), Passed(2), Passed(1), Passed(2)))
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
    summary = summary.accumulate(
      FeatureResult("feature4", Skipped, Some(new File("f4.feature")), Some(new File("f4.html"))),
      List(Skipped),
      List(Skipped, Skipped, Skipped))
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
    summary = summary.accumulate(
      FeatureResult("feature5", Pending, Some(new File("f5.feature")), Some(new File("f5.html"))),
      List(Pending),
      List(Pending, Pending))
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
    summary = summary.accumulate(
      FeatureResult("feature6", Failed(25, new Exception()), Some(new File("f6.feature")), Some(new File("f6.html"))),
      List(Passed(5), Passed(5), Passed(5), Passed(5), Failed(5, new Exception())),
      List(Passed(2), Passed(1), Passed(2), Passed(4), Passed(1), Passed(2), Passed(1), Passed(2), Passed(2), Passed(3), Passed(1), Failed(4, new Exception()), Skipped, Skipped))
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