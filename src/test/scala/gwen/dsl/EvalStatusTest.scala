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

package gwen.dsl

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec

class EvalStatusTest extends FlatSpec with ShouldMatchers with SpecNormaliser with SpecParser {

  private val parse = parseAll(spec, _: String);

  private val featureString = """
   
   Feature: The testing manifesto
        
Background: The tester
      Given I am a tester
        And I am not a programmer
       When I do my testing
       Then I should always write tests
        And I should always run tests
        But I should never code tests

  Scenario: The useless test
      Given I am a test
        And I am generated from code
       When the code changes
       Then I change
        And so I won't fail
        And that's why I'm useless
  
  Scenario: The useful test
      Given I am a test
        And I am written by a tester
       When the code changes
       Then I don't
        And so I may fail
        But that's why I'm useful
    
  @StepDef
  Scenario: The composed step
      Given I can compose
       Then I don't need to implement"""
        
  "Non evaluated status" should "be Pending" in {
    
    // setup
    
    val featureSpec = normalise(parse(featureString).get)
    
    // assert
    
	featureSpec.evalStatus.status should be (StatusKeyword.Pending)
	featureSpec.scenarios(0).background.get.evalStatus.status should be (StatusKeyword.Pending)
	featureSpec.scenarios(0).evalStatus.status                should be (StatusKeyword.Pending)
    featureSpec.scenarios(1).background.get.evalStatus.status should be (StatusKeyword.Pending)
	featureSpec.scenarios(1).evalStatus.status                should be (StatusKeyword.Pending)
    featureSpec.scenarios(2).background                       should be (None)    
	featureSpec.scenarios(2).evalStatus.status                should be (StatusKeyword.Pending)
	featureSpec.steps foreach {
	  _.evalStatus.status should be (StatusKeyword.Pending)
	}
  
  }
  
  "Feature" should "pass when there are no failures" in {
    
    // setup
    
    var featureSpec = normalise(parse(featureString).get)
    
    featureSpec = FeatureSpec(
      featureSpec.feature,
      None,
      featureSpec.scenarios map {scenario => 
        Scenario(
          scenario.tags,
          scenario.name, 
          scenario.background map { background =>
            Background(background.name, background.steps map {step =>
              Step(step.keyword, step.expression, Passed(1))
            })
          }, 
          scenario.steps map {step =>
            Step(step.keyword, step.expression, if (scenario.isStepDef) Loaded else Passed(1))
          }) 
      })
    
    // assert
    
	featureSpec.evalStatus.status should be (StatusKeyword.Passed)
	featureSpec.scenarios(0).background.get.evalStatus.status should be (StatusKeyword.Passed)
	featureSpec.scenarios(0).evalStatus.status                should be (StatusKeyword.Passed)
    featureSpec.scenarios(1).background.get.evalStatus.status should be (StatusKeyword.Passed)
	featureSpec.scenarios(1).evalStatus.status                should be (StatusKeyword.Passed)
    featureSpec.scenarios(2).background                       should be (None)    
	featureSpec.scenarios(2).evalStatus.status                should be (StatusKeyword.Loaded)
  
  }
  
  "Scenario 1" should "fail when a background step in scenario 1 fails" in {
    
    // setup
    
    var featureSpec = normalise(parse(featureString).get)
    
    featureSpec = FeatureSpec(
      featureSpec.feature,
      None,
      featureSpec.scenarios.zipWithIndex map {zip =>
        val (scenario, scenarioIndex) = zip
        Scenario(
          scenario.tags,
          scenario.name, 
          scenario.background map { background =>
            Background(background.name, background.steps.zipWithIndex map {zip =>
              val (step, stepIndex) = zip
              Step(step.keyword, step.expression, stepIndex match {
                case 0 | 1 | 2 if (scenarioIndex == 0) => Passed(1)
                case 3 if (scenarioIndex == 0) => Failed(99, new Exception(StatusKeyword.Failed.toString()))
                case _ => step.evalStatus
              })
            }) 
          }, 
          scenario.steps) 
      })
    
    // assert
    
	featureSpec.evalStatus.status should be (StatusKeyword.Failed)
    
	featureSpec.scenarios(0).background.get.evalStatus.status          should be (StatusKeyword.Failed)
    featureSpec.scenarios(0).background.get.steps(0).evalStatus.status should be (StatusKeyword.Passed)
    featureSpec.scenarios(0).background.get.steps(1).evalStatus.status should be (StatusKeyword.Passed)
    featureSpec.scenarios(0).background.get.steps(2).evalStatus.status should be (StatusKeyword.Passed)
    featureSpec.scenarios(0).background.get.steps(3).evalStatus.status should be (StatusKeyword.Failed)
    featureSpec.scenarios(0).background.get.steps(4).evalStatus.status should be (StatusKeyword.Pending)
    featureSpec.scenarios(0).background.get.steps(5).evalStatus.status should be (StatusKeyword.Pending)
    
    featureSpec.scenarios(1).background.get.evalStatus.status          should be (StatusKeyword.Pending)
    featureSpec.scenarios(1).background.get.steps(0).evalStatus.status should be (StatusKeyword.Pending)
    featureSpec.scenarios(1).background.get.steps(1).evalStatus.status should be (StatusKeyword.Pending)
    featureSpec.scenarios(1).background.get.steps(2).evalStatus.status should be (StatusKeyword.Pending)
    featureSpec.scenarios(1).background.get.steps(3).evalStatus.status should be (StatusKeyword.Pending)
    featureSpec.scenarios(1).background.get.steps(4).evalStatus.status should be (StatusKeyword.Pending)
    featureSpec.scenarios(1).background.get.steps(5).evalStatus.status should be (StatusKeyword.Pending)
    
    featureSpec.scenarios(2).background should be (None)
    
	featureSpec.scenarios.tail foreach {
	  scenario => 
	    scenario.evalStatus.status should be (StatusKeyword.Pending)
	    scenario.steps foreach {
	      _.evalStatus.status should be (StatusKeyword.Pending)
	    }
	}
  
  }
  
  "Scenario 2" should "fail when a background step in scenario 2 fails" in {
    
    // setup
    
    var featureSpec = normalise(parse(featureString).get)
    
    featureSpec = FeatureSpec(
      featureSpec.feature,
      None,
      featureSpec.scenarios.zipWithIndex map {zip =>
        val (scenario, scenarioIndex) = zip
        Scenario(
          scenario.tags,
          scenario.name, 
          scenario.background map { background =>
            Background(background.name, background.steps.zipWithIndex map {zip =>
              val (step, stepIndex) = zip
              Step(step.keyword, step.expression, stepIndex match {
                case 0 | 1 | 2 if (scenarioIndex < 2) => Passed(1)
                case 3 if (scenarioIndex == 1) => Failed(99, new Exception(StatusKeyword.Failed.toString()))
                case _ => if (scenarioIndex < 1) Passed(1) else step.evalStatus
              })
            }) 
          }, 
          scenario.steps) 
      })
    
    // assert
    
	featureSpec.evalStatus.status should be (StatusKeyword.Failed)
    
    featureSpec.scenarios(0).background.get.evalStatus.status          should be (StatusKeyword.Passed)
    featureSpec.scenarios(0).background.get.steps(0).evalStatus.status should be (StatusKeyword.Passed)
    featureSpec.scenarios(0).background.get.steps(1).evalStatus.status should be (StatusKeyword.Passed)
    featureSpec.scenarios(0).background.get.steps(2).evalStatus.status should be (StatusKeyword.Passed)
    featureSpec.scenarios(0).background.get.steps(3).evalStatus.status should be (StatusKeyword.Passed)
    featureSpec.scenarios(0).background.get.steps(4).evalStatus.status should be (StatusKeyword.Passed)
    featureSpec.scenarios(0).background.get.steps(5).evalStatus.status should be (StatusKeyword.Passed)
    
	featureSpec.scenarios(1).background.get.evalStatus.status          should be (StatusKeyword.Failed)
    featureSpec.scenarios(1).background.get.steps(0).evalStatus.status should be (StatusKeyword.Passed)
    featureSpec.scenarios(1).background.get.steps(1).evalStatus.status should be (StatusKeyword.Passed)
    featureSpec.scenarios(1).background.get.steps(2).evalStatus.status should be (StatusKeyword.Passed)
    featureSpec.scenarios(1).background.get.steps(3).evalStatus.status should be (StatusKeyword.Failed)
    featureSpec.scenarios(1).background.get.steps(4).evalStatus.status should be (StatusKeyword.Pending)
    featureSpec.scenarios(1).background.get.steps(5).evalStatus.status should be (StatusKeyword.Pending)
    
    featureSpec.scenarios(2).background should be (None)
    
	featureSpec.scenarios.drop(2) foreach {
	  scenario => 
	    scenario.evalStatus.status should be (StatusKeyword.Pending)
	    scenario.steps foreach {
	      _.evalStatus.status should be (StatusKeyword.Pending)
	    }
	}
  
  }
  
  "Scenario 1" should "fail when a step in scenario 1 fails" in {
    
    // setup 
    
    var featureSpec = normalise(parse(featureString).get)
    
    featureSpec = FeatureSpec(
      featureSpec.feature,
      None,
      featureSpec.scenarios.zipWithIndex map {zip =>
        val (scenario, scenarioIndex) = zip
        Scenario(
          scenario.tags,
          scenario.name, 
          scenario.background map { background =>
            Background(background.name, background.steps map { step =>
              Step(step.keyword, step.expression, scenarioIndex match {
                case 0 => Passed(1)
                case _ => step.evalStatus
              })
            }) 
          }, 
          scenario.steps.zipWithIndex map { zip =>
            val (step, stepIndex) = zip
            Step(step.keyword, step.expression, stepIndex match {
              case 0 | 1 | 2 if (scenarioIndex == 0) => Passed(1)
              case 3 if (scenarioIndex == 0) => Failed(99, new Exception(StatusKeyword.Failed.toString))
              case _ => step.evalStatus
            })
          }) 
      })
    
    // assert
    
	featureSpec.evalStatus.status should be (StatusKeyword.Failed)
    
	featureSpec.scenarios(0).background.get.evalStatus.status should be (StatusKeyword.Passed)
    
    featureSpec.scenarios(0).evalStatus.status          should be (StatusKeyword.Failed)
    featureSpec.scenarios(0).steps(0).evalStatus.status should be (StatusKeyword.Passed)
    featureSpec.scenarios(0).steps(1).evalStatus.status should be (StatusKeyword.Passed)
    featureSpec.scenarios(0).steps(2).evalStatus.status should be (StatusKeyword.Passed)
    featureSpec.scenarios(0).steps(3).evalStatus.status should be (StatusKeyword.Failed)
    featureSpec.scenarios(0).steps(4).evalStatus.status should be (StatusKeyword.Pending)
    featureSpec.scenarios(0).steps(5).evalStatus.status should be (StatusKeyword.Pending)
    
    featureSpec.scenarios(1).background.get.evalStatus.status          should be (StatusKeyword.Pending)
    featureSpec.scenarios(1).background.get.steps(0).evalStatus.status should be (StatusKeyword.Pending)
    featureSpec.scenarios(1).background.get.steps(1).evalStatus.status should be (StatusKeyword.Pending)
    featureSpec.scenarios(1).background.get.steps(2).evalStatus.status should be (StatusKeyword.Pending)
    featureSpec.scenarios(1).background.get.steps(3).evalStatus.status should be (StatusKeyword.Pending)
    featureSpec.scenarios(1).background.get.steps(4).evalStatus.status should be (StatusKeyword.Pending)
    featureSpec.scenarios(1).background.get.steps(5).evalStatus.status should be (StatusKeyword.Pending)
    
    featureSpec.scenarios(2).background should be (None)
    
	featureSpec.scenarios.tail foreach {
	  scenario => 
	    scenario.evalStatus.status should be (StatusKeyword.Pending)
	    scenario.steps foreach {
	      _.evalStatus.status should be (StatusKeyword.Pending)
	    }
	}
  
  }
  
  "Scenario 2" should "fail when a step in scenario 2 fails" in {
    
    // setup
    
    var featureSpec = normalise(parse(featureString).get)
    
    featureSpec = FeatureSpec(
      featureSpec.feature,
      None,
      featureSpec.scenarios.zipWithIndex map {zip =>
        val (scenario, scenarioIndex) = zip
        Scenario(
          scenario.tags,
          scenario.name, 
          scenario.background map { background =>
            Background(background.name, background.steps map { step =>
              Step(step.keyword, step.expression, Passed(1))
            }) 
          }, 
          scenario.steps.zipWithIndex map { zip =>
            val (step, stepIndex) = zip
            Step(step.keyword, step.expression, stepIndex match {
              case 0 | 1 | 2 if (scenarioIndex < 2) => Passed(1)
              case 3 if (scenarioIndex == 1) => Failed(99, new Exception(StatusKeyword.Failed.toString))
              case _ => if (scenarioIndex == 0) Passed(1) else step.evalStatus
            })
          }) 
      })
    
    // assert 
    
	featureSpec.evalStatus.status should be (StatusKeyword.Failed)
    
	featureSpec.scenarios(0).background.get.evalStatus.status should be (StatusKeyword.Passed)
    featureSpec.scenarios(0).evalStatus.status                should be (StatusKeyword.Passed)
    featureSpec.scenarios(1).background.get.evalStatus.status should be (StatusKeyword.Passed)
    
    featureSpec.scenarios(1).evalStatus.status should be (StatusKeyword.Failed)
    featureSpec.scenarios(1).steps(0).evalStatus.status should be (StatusKeyword.Passed)
    featureSpec.scenarios(1).steps(1).evalStatus.status should be (StatusKeyword.Passed)
    featureSpec.scenarios(1).steps(2).evalStatus.status should be (StatusKeyword.Passed)
    featureSpec.scenarios(1).steps(3).evalStatus.status should be (StatusKeyword.Failed)
    featureSpec.scenarios(1).steps(4).evalStatus.status should be (StatusKeyword.Pending)
    featureSpec.scenarios(1).steps(5).evalStatus.status should be (StatusKeyword.Pending)
    
	featureSpec.scenarios.drop(2) foreach {
	  scenario => 
	    scenario.evalStatus.status should be (StatusKeyword.Pending)
	    scenario.steps foreach {
	    _.evalStatus.status should be (StatusKeyword.Pending)
	  }
	}
  
  }

}