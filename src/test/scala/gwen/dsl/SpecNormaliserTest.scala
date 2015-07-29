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

class SpecNormaliserTest extends FlatSpec with Matchers with SpecNormaliser {
  
  "Feature with no step defs" should "normalise without error" in {
    val feature = FeatureSpec(
	  Feature("feature1", Nil), None, List(
	    Scenario(Set[Tag](), "scenario1", None, List(
	      Step(StepKeyword.Given, "step 1", Passed(2)),
	      Step(StepKeyword.Given, "step 2", Passed(1)),
	      Step(StepKeyword.Given, "step 3", Passed(2)))
	    )))
	normalise(feature, None)
  }
  
  "Meta with one step def" should "normalise without error" in {
    val meta = FeatureSpec(
	  Feature("meta1", Nil), None, List(
	    Scenario(Set[Tag]("@StepDef"), "stepdef1", None, List(
	      Step(StepKeyword.Given, "step 1", Passed(2)),
	      Step(StepKeyword.When, "step 2", Passed(1)),
	      Step(StepKeyword.Then, "step 3", Passed(2)))
	    )))
	normalise(meta, None)
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
	normalise(meta, None)
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
	  normalise(meta, None)
    }
  }
  
}