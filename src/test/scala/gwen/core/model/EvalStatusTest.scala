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

package gwen.core.model

import gwen.core.TestModel
import gwen.core.engine.SpecNormaliser
import gwen.core.node.gherkin.GherkinParser
import gwen.core.node.gherkin.Spec

import org.scalatest.FlatSpec
import org.scalatest.Matchers


class EvalStatusTest extends FlatSpec with Matchers with SpecNormaliser with GherkinParser with TestModel {

  private val parse = parseSpec(_: String)

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
    
    val featureSpec = normaliseSpec(parse(featureString).get, None)
    
    // assert
    
    featureSpec.evalStatus                             should be (Pending)
    featureSpec.scenarios(0).background.get.evalStatus should be (Pending)
    featureSpec.scenarios(0).evalStatus                should be (Pending)
    featureSpec.scenarios(1).background.get.evalStatus should be (Pending)
    featureSpec.scenarios(1).evalStatus                should be (Pending)
    featureSpec.scenarios(2).background                should be (None)    
    featureSpec.scenarios(2).evalStatus                should be (Pending)
    featureSpec.steps foreach {
      _.evalStatus should be (Pending)
    }
    featureSpec.sustainedCount should be(0)
  
  }
  
  "Feature" should "pass when there are no failures" in {
    
    // setup
    
    var featureSpec = normaliseSpec(parse(featureString).get, None)
    
    featureSpec = Spec(
      featureSpec.feature,
      None,
      featureSpec.scenarios map { scenario => 
        Scenario(
          scenario.tags,
          scenario.name, 
          scenario.description,
          scenario.background map { background =>
            Background(background.name, background.description, background.steps map {step =>
              Step(step, step.keyword, step.name, Passed(1))
            })
          }, 
          scenario.steps map {step =>
            Step(step, step.keyword, step.name, if (scenario.isStepDef) Loaded else Passed(1))
          }) 
      },
      Nil,
      Nil)
    
    // assert
    
    featureSpec.evalStatus                             should be (Passed(24))
    featureSpec.scenarios(0).background.get.evalStatus should be (Passed(6))
    featureSpec.scenarios(0).evalStatus                should be (Passed(12))
    featureSpec.scenarios(1).background.get.evalStatus should be (Passed(6))
    featureSpec.scenarios(1).evalStatus                should be (Passed(12))
    featureSpec.scenarios(2).background                should be (None)    
    featureSpec.scenarios(2).evalStatus                should be (Loaded)
    featureSpec.sustainedCount should be(0)
  
  }

  "Feature" should "pass when there is a sustained error in background" in {

    // setup
    val sustained = new Exception(StatusKeyword.Sustained.toString)
    var featureSpec = normaliseSpec(parse(featureString).get, None)

    featureSpec = Spec(
      featureSpec.feature,
      None,
      featureSpec.scenarios map { scenario =>
        Scenario(
          scenario.tags,
          scenario.name,
          scenario.description,
          scenario.background map { background =>
            Background(background.name, background.description, background.steps map {step =>
              Step(step, step.keyword, step.name, if (step.name.contains("should")) Sustained(1, sustained) else Passed(1))
            })
          },
          scenario.steps map {step =>
            Step(step, step.keyword, step.name, if (scenario.isStepDef) Loaded else Passed(1))
          })
      },
      Nil,
      Nil)

    // assert

    featureSpec.evalStatus                             should be (Passed(24))
    featureSpec.scenarios(0).background.get.evalStatus should be (Passed(6))
    featureSpec.scenarios(0).evalStatus                should be (Passed(12))
    featureSpec.scenarios(1).background.get.evalStatus should be (Passed(6))
    featureSpec.scenarios(1).evalStatus                should be (Passed(12))
    featureSpec.scenarios(2).background                should be (None)
    featureSpec.scenarios(2).evalStatus                should be (Loaded)
    featureSpec.sustainedCount should be(6)

  }

  "Feature" should "pass when there is a sustained error in each scenario" in {

    // setup
    val sustained = new Exception(StatusKeyword.Sustained.toString)
    var featureSpec = normaliseSpec(parse(featureString).get, None)

    featureSpec = Spec(
      featureSpec.feature,
      None,
      featureSpec.scenarios map { scenario =>
        Scenario(
          scenario.tags,
          scenario.name,
          scenario.description,
          scenario.background map { background =>
            Background(background.name, background.description, background.steps map {step =>
              Step(step, step.keyword, step.name, Passed(1))
            })
          },
          scenario.steps.zipWithIndex map { case (step, index) =>
            Step(step, step.keyword, step.name, if (scenario.isStepDef) Loaded else if (index == 0) Sustained(1, sustained) else  Passed(1))
          })
      },
      Nil,
      Nil)

    // assert

    featureSpec.evalStatus                             should be (Passed(24))
    featureSpec.scenarios(0).background.get.evalStatus should be (Passed(6))
    featureSpec.scenarios(0).evalStatus                should be (Passed(12))
    featureSpec.scenarios(1).background.get.evalStatus should be (Passed(6))
    featureSpec.scenarios(1).evalStatus                should be (Passed(12))
    featureSpec.scenarios(2).background                should be (None)
    featureSpec.scenarios(2).evalStatus                should be (Loaded)
    featureSpec.sustainedCount should be(2)

  }

  "Feature" should "pass when there is a sustained error in one scenario" in {

    // setup
    val sustained = new Exception(StatusKeyword.Sustained.toString)
    var featureSpec = normaliseSpec(parse(featureString).get, None)

    var isSustained = false

    featureSpec = Spec(
      featureSpec.feature,
      None,
      featureSpec.scenarios map { scenario =>
        Scenario(
          scenario.tags,
          scenario.name,
          scenario.description,
          scenario.background map { background =>
            Background(background.name, background.description, background.steps map {step =>
              Step(step, step.keyword, step.name, Passed(1))
            })
          },
          scenario.steps map { step =>
            val status = if (scenario.isStepDef) {
              Loaded
            } else if(!isSustained) {
              isSustained = true
              Sustained(1, sustained)
            } else {
              Passed(1)
            }
            Step(step, step.keyword, step.name, status)
          })
      },
      Nil,
      Nil)

    // assert

    featureSpec.evalStatus                             should be (Passed(24))
    featureSpec.scenarios(0).background.get.evalStatus should be (Passed(6))
    featureSpec.scenarios(0).evalStatus                should be (Passed(12))
    featureSpec.scenarios(1).background.get.evalStatus should be (Passed(6))
    featureSpec.scenarios(1).evalStatus                should be (Passed(12))
    featureSpec.scenarios(2).background                should be (None)
    featureSpec.scenarios(2).evalStatus                should be (Loaded)
    featureSpec.sustainedCount should be(1)

  }
  
  "Scenario 1" should "fail when a background step in scenario 1 fails" in {
    
    // setup
    
    val error = new Exception(StatusKeyword.Failed.toString)
    var featureSpec = normaliseSpec(parse(featureString).get, None)
    
    featureSpec = Spec(
      featureSpec.feature,
      None,
      featureSpec.scenarios.zipWithIndex map { zip =>
        val (scenario, scenarioIndex) = zip
        Scenario(
          scenario.tags,
          scenario.name, 
          scenario.description,
          scenario.background map { background =>
            Background(background.name, background.description, background.steps.zipWithIndex map {zip =>
              val (step, stepIndex) = zip
              Step(step, step.keyword, step.name, stepIndex match {
                case 0 | 1 | 2 if scenarioIndex == 0 => Passed(1)
                case 3 if scenarioIndex == 0 => Failed(99, error)
                case _ => step.evalStatus
              })
            }) 
          }, 
          scenario.steps) 
      },
      Nil,
      Nil)
    
    // assert
    
    featureSpec.evalStatus should be (Failed(102, error))
    
    featureSpec.scenarios(0).background.get.evalStatus          should be (Failed(102, error))
    featureSpec.scenarios(0).background.get.steps(0).evalStatus should be (Passed(1))
    featureSpec.scenarios(0).background.get.steps(1).evalStatus should be (Passed(1))
    featureSpec.scenarios(0).background.get.steps(2).evalStatus should be (Passed(1))
    featureSpec.scenarios(0).background.get.steps(3).evalStatus should be (Failed(99, error))
    featureSpec.scenarios(0).background.get.steps(4).evalStatus should be (Pending)
    featureSpec.scenarios(0).background.get.steps(5).evalStatus should be (Pending)
    
    featureSpec.scenarios(1).background.get.evalStatus          should be (Pending)
    featureSpec.scenarios(1).background.get.steps(0).evalStatus should be (Pending)
    featureSpec.scenarios(1).background.get.steps(1).evalStatus should be (Pending)
    featureSpec.scenarios(1).background.get.steps(2).evalStatus should be (Pending)
    featureSpec.scenarios(1).background.get.steps(3).evalStatus should be (Pending)
    featureSpec.scenarios(1).background.get.steps(4).evalStatus should be (Pending)
    featureSpec.scenarios(1).background.get.steps(5).evalStatus should be (Pending)
    
    featureSpec.scenarios(2).background should be (None)
    
    featureSpec.scenarios.tail foreach {
      scenario => 
        scenario.evalStatus should be (Pending)
        scenario.steps foreach {
          _.evalStatus should be (Pending)
        }
    }

    featureSpec.sustainedCount should be(0)
  
  }
  
  "Scenario 2" should "fail when a background step in scenario 2 fails" in {
    
    // setup
    
    val error = new Exception(StatusKeyword.Failed.toString)
    var featureSpec = normaliseSpec(parse(featureString).get, None)
    
    featureSpec = Spec(
      featureSpec.feature,
      None,
      featureSpec.scenarios.zipWithIndex map { zip =>
        val (scenario, scenarioIndex) = zip
        Scenario(
          scenario.tags,
          scenario.name, 
          scenario.description, 
          scenario.background map { background =>
            Background(background.name, background.description, background.steps.zipWithIndex map {zip =>
              val (step, stepIndex) = zip
              Step(step, step.keyword, step.name, stepIndex match {
                case 0 | 1 | 2 if scenarioIndex < 2 => Passed(1)
                case 3 if scenarioIndex == 1 => Failed(99, error)
                case _ => if (scenarioIndex < 1) Passed(1) else step.evalStatus
              })
            }) 
          }, 
          scenario.steps) 
      },
      Nil,
      Nil)
    
    // assert
    
    featureSpec.evalStatus should be (Failed(108, error))
    
    featureSpec.scenarios(0).background.get.evalStatus          should be (Passed(6))
    featureSpec.scenarios(0).background.get.steps(0).evalStatus should be (Passed(1))
    featureSpec.scenarios(0).background.get.steps(1).evalStatus should be (Passed(1))
    featureSpec.scenarios(0).background.get.steps(2).evalStatus should be (Passed(1))
    featureSpec.scenarios(0).background.get.steps(3).evalStatus should be (Passed(1))
    featureSpec.scenarios(0).background.get.steps(4).evalStatus should be (Passed(1))
    featureSpec.scenarios(0).background.get.steps(5).evalStatus should be (Passed(1))
    
    featureSpec.scenarios(1).background.get.evalStatus          should be (Failed(102, error))
    featureSpec.scenarios(1).background.get.steps(0).evalStatus should be (Passed(1))
    featureSpec.scenarios(1).background.get.steps(1).evalStatus should be (Passed(1))
    featureSpec.scenarios(1).background.get.steps(2).evalStatus should be (Passed(1))
    featureSpec.scenarios(1).background.get.steps(3).evalStatus should be (Failed(99, error))
    featureSpec.scenarios(1).background.get.steps(4).evalStatus should be (Pending)
    featureSpec.scenarios(1).background.get.steps(5).evalStatus should be (Pending)
    
    featureSpec.scenarios(2).background should be (None)
    
    featureSpec.scenarios.drop(2) foreach {
      scenario => 
        scenario.evalStatus should be (Pending)
        scenario.steps foreach {
          _.evalStatus should be (Pending)
        }
    }

    featureSpec.sustainedCount should be(0)
  
  }
  
  "Scenario 1" should "fail when a step in scenario 1 fails" in {
    
    // setup 
    
    val error = new Exception(StatusKeyword.Failed.toString)
    var featureSpec = normaliseSpec(parse(featureString).get, None)
    
    featureSpec = Spec(
      featureSpec.feature,
      None,
      featureSpec.scenarios.zipWithIndex map { zip =>
        val (scenario, scenarioIndex) = zip
        Scenario(
          scenario.tags,
          scenario.name, 
          scenario.description, 
          scenario.background map { background =>
            Background(background.name, background.description, background.steps map { step =>
              Step(step, step.keyword, step.name, scenarioIndex match {
                case 0 => Passed(1)
                case _ => step.evalStatus
              })
            }) 
          }, 
          scenario.steps.zipWithIndex map { zip =>
            val (step, stepIndex) = zip
            Step(step, step.keyword, step.name, stepIndex match {
              case 0 | 1 | 2 if scenarioIndex == 0 => Passed(1)
              case 3 if scenarioIndex == 0 => Failed(99, error)
              case _ => step.evalStatus
            })
          }) 
      },
      Nil,
      Nil)
    
    // assert
    
    featureSpec.evalStatus.status should be (StatusKeyword.Failed)
    
    featureSpec.scenarios(0).background.get.evalStatus should be (Passed(6))
    
    featureSpec.scenarios(0).evalStatus          should be (Failed(108, error))
    featureSpec.scenarios(0).steps(0).evalStatus should be (Passed(1))
    featureSpec.scenarios(0).steps(1).evalStatus should be (Passed(1))
    featureSpec.scenarios(0).steps(2).evalStatus should be (Passed(1))
    featureSpec.scenarios(0).steps(3).evalStatus should be (Failed(99, error))
    featureSpec.scenarios(0).steps(4).evalStatus should be (Pending)
    featureSpec.scenarios(0).steps(5).evalStatus should be (Pending)
    
    featureSpec.scenarios(1).background.get.evalStatus          should be (Pending)
    featureSpec.scenarios(1).background.get.steps(0).evalStatus should be (Pending)
    featureSpec.scenarios(1).background.get.steps(1).evalStatus should be (Pending)
    featureSpec.scenarios(1).background.get.steps(2).evalStatus should be (Pending)
    featureSpec.scenarios(1).background.get.steps(3).evalStatus should be (Pending)
    featureSpec.scenarios(1).background.get.steps(4).evalStatus should be (Pending)
    featureSpec.scenarios(1).background.get.steps(5).evalStatus should be (Pending)
    
    featureSpec.scenarios(2).background should be (None)
    
    featureSpec.scenarios.tail foreach {
      scenario => 
        scenario.evalStatus should be (Pending)
        scenario.steps foreach {
          _.evalStatus should be (Pending)
        }
    }

    featureSpec.sustainedCount should be(0)
  
  }
  
  "Scenario 2" should "fail when a step in scenario 2 fails" in {
    
    // setup
    
    val error = new Exception(StatusKeyword.Failed.toString)
    var featureSpec = normaliseSpec(parse(featureString).get, None)
    
    featureSpec = Spec(
      featureSpec.feature,
      None,
      featureSpec.scenarios.zipWithIndex map { zip =>
        val (scenario, scenarioIndex) = zip
        Scenario(
          scenario.tags,
          scenario.name, 
          scenario.description, 
          scenario.background map { background =>
            Background(background.name, background.description, background.steps map { step =>
              Step(step, step.keyword, step.name, Passed(1))
            }) 
          }, 
          scenario.steps.zipWithIndex map { zip =>
            val (step, stepIndex) = zip
            Step(step, step.keyword, step.name, stepIndex match {
              case 0 | 1 | 2 if scenarioIndex < 2 => Passed(1)
              case 3 if scenarioIndex == 1 => Failed(99, error)
              case _ => if (scenarioIndex == 0) Passed(1) else step.evalStatus
            })
          }) 
      },
      Nil,
      Nil)
    
    // assert 
    
    featureSpec.evalStatus.status should be (StatusKeyword.Failed)
    
    featureSpec.scenarios(0).background.get.evalStatus should be (Passed(6))
    featureSpec.scenarios(0).evalStatus                should be (Passed(12))
    featureSpec.scenarios(1).background.get.evalStatus should be (Passed(6))
    
    featureSpec.scenarios(1).evalStatus should be (Failed(108, error))
    featureSpec.scenarios(1).steps(0).evalStatus should be (Passed(1))
    featureSpec.scenarios(1).steps(1).evalStatus should be (Passed(1))
    featureSpec.scenarios(1).steps(2).evalStatus should be (Passed(1))
    featureSpec.scenarios(1).steps(3).evalStatus should be (Failed(99, error))
    featureSpec.scenarios(1).steps(4).evalStatus should be (Pending)
    featureSpec.scenarios(1).steps(5).evalStatus should be (Pending)
    
    featureSpec.scenarios.drop(2) foreach {
      scenario => 
        scenario.evalStatus should be (Pending)
        scenario.steps foreach {
        _.evalStatus should be (Pending)
      }
    }

    featureSpec.sustainedCount should be(0)
  
  }


  "isEvaluated on Passed, Failed, and Sustained" should "return true, false otherwise" in {
    EvalStatus.isEvaluated(StatusKeyword.Passed) should be (true)
    EvalStatus.isEvaluated(StatusKeyword.Failed) should be (true)
    EvalStatus.isEvaluated(StatusKeyword.Disabled) should be (true)
    EvalStatus.isEvaluated(StatusKeyword.Sustained) should be (true)
    EvalStatus.isEvaluated(StatusKeyword.Skipped) should be (false)
    EvalStatus.isEvaluated(StatusKeyword.Pending) should be (false)
    EvalStatus.isEvaluated(StatusKeyword.Loaded) should be (false)
  }

  "Passed statuses with one Disabled" should "be Passed" in {
    EvalStatus(List(Disabled, Passed(10), Passed(10))).status should be (StatusKeyword.Passed)
    EvalStatus(List(Passed(10), Disabled, Passed(10))).status should be (StatusKeyword.Passed)
    EvalStatus(List(Passed(10), Passed(10), Disabled)).status should be (StatusKeyword.Passed)
  }

  "Passed statuses with some Disabled" should "be Passed" in {
    EvalStatus(List(Disabled, Passed(10), Passed(10), Disabled, Disabled)).status should be (StatusKeyword.Passed)
    EvalStatus(List(Passed(10), Disabled, Passed(10), Disabled, Disabled)).status should be (StatusKeyword.Passed)
    EvalStatus(List(Passed(10), Passed(10), Disabled, Disabled, Disabled)).status should be (StatusKeyword.Passed)
  }

  "All disabled statuses" should "be Skipped" in {
    EvalStatus(List(Disabled)).status should be (StatusKeyword.Skipped)
    EvalStatus(List(Disabled, Disabled)).status should be (StatusKeyword.Skipped)
    EvalStatus(List(Disabled, Disabled, Disabled)).status should be (StatusKeyword.Skipped)
  }

}