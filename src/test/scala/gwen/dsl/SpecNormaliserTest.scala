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
import gwen.eval.DataRecord

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
    val feature = FeatureSpec(
    Feature("About me", Nil), None, List(
      Scenario(Set[Tag](), "What am I?", None, List(
        Step(StepKeyword.Given, "I am ${my age} year(s) old"),
        Step(StepKeyword.When, "I am a ${my gender}"),
        Step(StepKeyword.Then, "I am a ${my age} year old ${my title}"))
      )))
    val data = Map("my age" -> "18", "my gender" -> "male", "my title" -> "Mr")
    val dataRecord = new DataRecord("AboutMe.csv", 1, data)
    val result = normalise(feature, None, Some(dataRecord))
    result.feature.name should be ("About me, [1] my age=18..")
    result.scenarios.length should be (2)
    result.scenarios(0).tags should be (Set(Tag("""Data(file="AboutMe.csv", record=1)""")))
    result.scenarios(0).name should be ("Bind data attributes")
    result.scenarios(0).steps(0).toString should be ("""Given my age is "18"""")
    result.scenarios(0).steps(1).toString should be ("""And my gender is "male"""")
    result.scenarios(0).steps(2).toString should be ("""And my title is "Mr"""")
    result.scenarios(1).name should be ("What am I?")
    result.scenarios(1).steps(0).toString should be ("""Given I am ${my age} year(s) old""")
    result.scenarios(1).steps(1).toString should be ("""When I am a ${my gender}""")
    result.scenarios(1).steps(2).toString should be ("""Then I am a ${my age} year old ${my title}""")
  }
  
}