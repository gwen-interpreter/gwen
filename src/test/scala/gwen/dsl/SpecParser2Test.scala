/*
 * Copyright 2014-2015 Branko Juric, Brady Wood
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http:www.apache.org/licenses/LICENSE-2.0
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

class SpecParser2Test extends FlatSpec with Matchers with SpecParser {

  private val parse = parseAll(spec, _: String);
  
  private val featureString = """
   
     Feature: Gwen
       As a tester
       I want to automate tests

    Background: The butterfly effect
        Given a deterministic nonlinear system
         When a small change is initially applied
         Then a large change will eventually result
    
    Scenario: Evaluation
        Given any software behavior
         When expressed in Gherkin
         Then Gwen can evaluate it
    
    Scenario: The useless test
        Given I am a test
          And I am generated from code
         When the code changes
         Then I change
          And so I won't fail
          And that's why I'm useless
    
    Scenario: The useful test
        Given I am a test
          And I am written by a human
         When the code changes
         Then I don't
          And so I may fail
          But that's why I'm useful
 
 """

  "Feature" should "parse" in {
    
    val featureSpec = parse(featureString) match {
      case Success(featureSpec, _) => 
        featureSpec.feature should be (Feature("Gwen", List("As a tester", "I want to automate tests")))
        featureSpec.background.get should be {
          Background("The butterfly effect", 
            List(
              Step(StepKeyword.Given, "a deterministic nonlinear system"),
              Step(StepKeyword.When,  "a small change is initially applied"),
              Step(StepKeyword.Then,  "a large change will eventually result")
            )
          )
        }
        featureSpec.scenarios should be {
          List(
            Scenario(Set[Tag](), "Evaluation", None,
              List(
                Step(StepKeyword.Given, "any software behavior"),
                Step(StepKeyword.When,  "expressed in Gherkin"),
                Step(StepKeyword.Then,  "Gwen can evaluate it")
              )
            ),
            Scenario(Set[Tag](), "The useless test", None, 
              List(
                Step(StepKeyword.Given, "I am a test"),
                Step(StepKeyword.And,   "I am generated from code"),
                Step(StepKeyword.When,  "the code changes"),
                Step(StepKeyword.Then,  "I change"),
                Step(StepKeyword.And,   "so I won't fail"),
                Step(StepKeyword.And,   "that's why I'm useless")
              )
            ),
            Scenario(Set[Tag](), "The useful test", None, 
              List(
                Step(StepKeyword.Given, "I am a test"),
                Step(StepKeyword.And,   "I am written by a human"),
                Step(StepKeyword.When,  "the code changes"),
                Step(StepKeyword.Then,  "I don't"),
                Step(StepKeyword.And,   "so I may fail"),
                Step(StepKeyword.But,   "that's why I'm useful")
              )
            )
          )
        }
      case e => fail(e.toString)
     }
    
  }
  
  "Feature and scenario level tags" should "parse" in {
    
    val featureString = """
      
       @wip
       Feature: Work in progress
      
      Scenario: Work unit 1
          Given I do work 1
      
      @work @work
      Scenario: Work unit 2
          Given I do work 2
      
      @work 
      @play
      Scenario: Work unit 3
          Given I do work 3
      
      @wip @play
      Scenario: Work unit 4
          Given I do work 4"""
    
    val featureSpec = parse(featureString).get
    
    featureSpec.feature.tags.size should be (1)
    featureSpec.feature.tags.contains(Tag("wip")) should be (true)
    
    featureSpec.scenarios(0).tags.size should be (0)
    
    featureSpec.scenarios(1).tags.size should be (1)
    featureSpec.scenarios(1).tags.contains(Tag("work")) should be (true)
    
    featureSpec.scenarios(2).tags.size should be (2)
    featureSpec.scenarios(2).tags.contains(Tag("work")) should be (true)
    featureSpec.scenarios(2).tags.contains(Tag("play")) should be (true)
    
    featureSpec.scenarios(3).tags.size should be (2)
    featureSpec.scenarios(3).tags.contains(Tag("wip")) should be (true)
    featureSpec.scenarios(3).tags.contains(Tag("play")) should be (true)
  }
  
  "Scenario level tags" should "parse" in {
    
    val featureString = """
      
       Feature: Work in progress
      
      Scenario: Work unit 1
          Given I do work 1
      
      @work
      Scenario: Work unit 2
          Given I do work 2
      
      @play
      Scenario: Work unit 3
          Given I do work 3
      
      @wip @play
      Scenario: Work unit 4
          Given I do work 4"""
    
    val featureSpec = parse(featureString).get
    
    featureSpec.feature.tags.size should be (0)
    
    featureSpec.scenarios(0).tags.size should be (0)
    
    featureSpec.scenarios(1).tags.size should be (1)
    featureSpec.scenarios(1).tags.contains(Tag("work")) should be (true)
    
    featureSpec.scenarios(2).tags.size should be (1)
    featureSpec.scenarios(2).tags.contains(Tag("play")) should be (true)
    
    featureSpec.scenarios(3).tags.size should be (2)
    featureSpec.scenarios(3).tags.contains(Tag("wip")) should be (true)
    featureSpec.scenarios(3).tags.contains(Tag("play")) should be (true)
    
  }
  
  "Scenarios with invalid tag" should "not parse" in {
    
    val featureString = """
      
       Feature: Work in progress
      
      Scenario: Work unit 1
          Given I do work 1
      
      @work
      Scenario: Work unit 2
          Given I do work 2
      
      @play
      Scenario: Work unit 3
          Given I do work 3
      
      @wip play
      Scenario: Work unit 4
          Given I do work 4"""
    
    val featureSpec = parse(featureString).successful should be (false)
    
  }
    
}