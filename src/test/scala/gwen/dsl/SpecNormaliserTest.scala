/*
 * Copyright 2015 Branko Juric, Brady Wood
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

package gwen.dsl

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import gwen.errors.AmbiguousCaseException
import java.io.File

class SpecNormaliserTest extends FlatSpec with Matchers with SpecNormaliser {
  
  "Feature with no step defs" should "normalise without error" in {
    val feature = FeatureSpec(
    Feature("feature1", Nil), None, List(
      Scenario(Set[Tag](), "scenario1", None, List(
        Step(StepKeyword.Given, "step 1", Passed(2)),
        Step(StepKeyword.Given, "step 2", Passed(1)),
        Step(StepKeyword.Given, "step 3", Passed(2)))
      )))
  normalise(feature, None, None)
  }
  
  "Meta with one step def" should "normalise without error" in {
    val meta = FeatureSpec(
    Feature("meta1", Nil), None, List(
      Scenario(Set[Tag]("@StepDef"), "stepdef1", None, List(
        Step(StepKeyword.Given, "step 1", Passed(2)),
        Step(StepKeyword.When, "step 2", Passed(1)),
        Step(StepKeyword.Then, "step 3", Passed(2)))
      )))
  normalise(meta, None, None)
  }
  
  "Meta with multiple unique step defs" should "normalise without error" in {
    val meta = FeatureSpec(
    Feature("meta1", Nil), None, List(
      Scenario(Set[Tag]("@StepDef"), "stepdef1", None, List(
        Step(StepKeyword.Given, "step 1", Passed(2)),
        Step(StepKeyword.When, "step 2", Passed(1)),
        Step(StepKeyword.Then, "step 3", Passed(2)))
      ),
      Scenario(Set[Tag]("@StepDef"), "stepdef2", None, List(
        Step(StepKeyword.Given, "step 1", Passed(2)),
        Step(StepKeyword.When, "step 2", Passed(1)),
        Step(StepKeyword.Then, "step 3", Passed(2)))
      )))
  normalise(meta, None, None)
  }
  
  "Meta with duplicate step def" should "error" in {
    val meta = FeatureSpec(
    Feature("meta1", Nil), None, List(
      Scenario(Set[Tag]("@StepDef"), "stepdef1", None, List(
        Step(StepKeyword.Given, "step 1", Passed(2)),
        Step(StepKeyword.When, "step 2", Passed(1)),
        Step(StepKeyword.Then, "step 3", Passed(2)))
      ),
      Scenario(Set[Tag]("@StepDef"), "stepdef1", None, List(
        Step(StepKeyword.Given, "step 1", Passed(2)),
        Step(StepKeyword.When, "step 2", Passed(1)),
        Step(StepKeyword.Then, "step 3", Passed(2)))
      )))
      
  intercept[AmbiguousCaseException] {
    normalise(meta, None, None)
    }
  }
  
  "Meta with duplicate step def with params" should "error" in {
    val meta = FeatureSpec(
    Feature("meta1", Nil), None, List(
      Scenario(Set[Tag]("@StepDef"), "stepdef <number>", None, List(
        Step(StepKeyword.Given, "step 1", Passed(2)),
        Step(StepKeyword.When, "step 2", Passed(1)),
        Step(StepKeyword.Then, "step 3", Passed(2)))
      ),
      Scenario(Set[Tag]("@StepDef"), "stepdef <index>", None, List(
        Step(StepKeyword.Given, "step 1", Passed(2)),
        Step(StepKeyword.When, "step 2", Passed(1)),
        Step(StepKeyword.Then, "step 3", Passed(2)))
      )))
      
    intercept[AmbiguousCaseException] {
      normalise(meta, None, None)
    }
  }
  
  "Data driven feature with csv file" should "normalise without error" in {
    val csvFile = new File(getClass().getResource("/data-driven.csv").getFile())
    val feature = FeatureSpec(
    Feature("feature1", Nil), None, List(
      Scenario(Set[Tag](), "scenario1", None, List(
        Step(StepKeyword.Given, "I am a ${My Title}"),
        Step(StepKeyword.And, "I am a ${My Gender}"),
        Step(StepKeyword.And, "I am ${My Age} year(s) old"))
      )))
    val result = normalise(feature, None, Some(csvFile))
    result.scenarios.length should be (6)
    result.scenarios(0).tags.contains(Tag("Dataset"))
    result.scenarios(0).name should be ("Initialise dataset: data-driven.csv[1]")
    result.scenarios(0).steps(0).toString should be ("""Given My Title is "Mr"""")
    result.scenarios(0).steps(1).toString should be ("""And My Gender is "Male"""")
    result.scenarios(0).steps(2).toString should be ("""And My Age is "18"""")
    result.scenarios(1).name should be ("scenario1")
    result.scenarios(1).steps(0).toString should be ("Given I am a ${My Title}")
    result.scenarios(1).steps(1).toString should be ("And I am a ${My Gender}")
    result.scenarios(1).steps(2).toString should be ("And I am ${My Age} year(s) old")
    result.scenarios(2).tags.contains(Tag("Dataset"))
    result.scenarios(2).name should be ("Initialise dataset: data-driven.csv[2]")
    result.scenarios(2).steps(0).toString should be ("""Given My Title is "Mrs"""")
    result.scenarios(2).steps(1).toString should be ("""And My Gender is "Female"""")
    result.scenarios(2).steps(2).toString should be ("""And My Age is "18"""")
    result.scenarios(3).name should be ("scenario1")
    result.scenarios(3).steps(0).toString should be ("Given I am a ${My Title}")
    result.scenarios(3).steps(1).toString should be ("And I am a ${My Gender}")
    result.scenarios(3).steps(2).toString should be ("And I am ${My Age} year(s) old")
    result.scenarios(4).tags.contains(Tag("Dataset"))
    result.scenarios(4).name should be ("Initialise dataset: data-driven.csv[3]")
    result.scenarios(4).steps(0).toString should be ("""Given My Title is "Miss"""")
    result.scenarios(4).steps(1).toString should be ("""And My Gender is "Female"""")
    result.scenarios(4).steps(2).toString should be ("""And My Age is "22"""")
    result.scenarios(5).name should be ("scenario1")
    result.scenarios(5).steps(0).toString should be ("Given I am a ${My Title}")
    result.scenarios(5).steps(1).toString should be ("And I am a ${My Gender}")
    result.scenarios(5).steps(2).toString should be ("And I am ${My Age} year(s) old")
  }
  
}