/*
 * Copyright 2014-2021 Branko Juric, Brady Wood
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

package gwen.core.node.gherkin

import gwen.core.BaseTest
import gwen.core.TestModel

import scala.util.Success

import org.scalatest.matchers.should.Matchers

class GherkinParserTest extends BaseTest with Matchers with GherkinParser with TestModel {

  private val parse = parseSpec(_: String)
  
  private val featureString = """
   
     Feature: Gwen
       As a tester
       I want to automate tests
       So that gwen can run them

  Background: The butterfly effect
  
        Sensitivity to initial conditions
        
        Given a deterministic nonlinear system
         When a small change is initially applied
         Then a large change will eventually result
    
    Scenario: Evaluation
        Given any software behaviour
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
    
    parse(featureString) match {
      case Success(fspec) =>
        val feature = fspec.feature.copy(withSourceRef = None)
        val background = fspec.background.map { bg => 
          bg.copy(
            withSourceRef = None,
            withSteps = bg.steps.map(_.copy(withSourceRef = None))
            )
        }
        val scenarios = fspec.scenarios.map { s => 
          s.copy(
            withSourceRef = None,
            withSteps = s.steps.map(_.copy(withSourceRef = None))
          )
        }
        feature should be (Feature(None, "Gwen", List("As a tester", "I want to automate tests", "So that gwen can run them")))
        background.get should be {
          Background("The butterfly effect", List("Sensitivity to initial conditions"), 
            List(
              Step(StepKeyword.Given.toString, "a deterministic nonlinear system"),
              Step(StepKeyword.When.toString,  "a small change is initially applied"),
              Step(StepKeyword.Then.toString,  "a large change will eventually result")
            )
          )
        }
        scenarios should be {
          List(
            Scenario(List[Tag](), "Evaluation", Nil, None,
              List(
                Step(StepKeyword.Given.toString, "any software behaviour"),
                Step(StepKeyword.When.toString,  "expressed in Gherkin"),
                Step(StepKeyword.Then.toString,  "Gwen can evaluate it")
              )
            ),
            Scenario(List[Tag](), "The useless test", Nil, None, 
              List(
                Step(StepKeyword.Given.toString, "I am a test"),
                Step(StepKeyword.And.toString,   "I am generated from code"),
                Step(StepKeyword.When.toString,  "the code changes"),
                Step(StepKeyword.Then.toString,  "I change"),
                Step(StepKeyword.And.toString,   "so I won't fail"),
                Step(StepKeyword.And.toString,   "that's why I'm useless")
              )
            ),
            Scenario(List[Tag](), "The useful test", Nil, None, 
              List(
                Step(StepKeyword.Given.toString, "I am a test"),
                Step(StepKeyword.And.toString,   "I am written by a human"),
                Step(StepKeyword.When.toString,  "the code changes"),
                Step(StepKeyword.Then.toString,  "I don't"),
                Step(StepKeyword.And.toString,   "so I may fail"),
                Step(StepKeyword.But.toString,   "that's why I'm useful")
              )
            )
          )
        }
        fspec.feature.sourceRef.get.line should be (3)
        fspec.background.get.sourceRef.get.line should be (8)
        fspec.scenarios(0).sourceRef.get.line should be (16)
        fspec.scenarios(0).steps(0).sourceRef.get.line should be (17)
        fspec.scenarios(0).steps(1).sourceRef.get.line should be (18)
        fspec.scenarios(0).steps(2).sourceRef.get.line should be (19)
        fspec.scenarios(1).sourceRef.get.line should be (21)
        fspec.scenarios(1).steps(0).sourceRef.get.line should be (22)
        fspec.scenarios(1).steps(1).sourceRef.get.line should be (23)
        fspec.scenarios(1).steps(2).sourceRef.get.line should be (24)
        fspec.scenarios(1).steps(3).sourceRef.get.line should be (25)
        fspec.scenarios(1).steps(4).sourceRef.get.line should be (26)
        fspec.scenarios(1).steps(5).sourceRef.get.line should be (27)
        fspec.scenarios(2).sourceRef.get.line should be (29)
        fspec.scenarios(2).steps(0).sourceRef.get.line should be (30)
        fspec.scenarios(2).steps(1).sourceRef.get.line should be (31)
        fspec.scenarios(2).steps(2).sourceRef.get.line should be (32)
        fspec.scenarios(2).steps(3).sourceRef.get.line should be (33)
        fspec.scenarios(2).steps(4).sourceRef.get.line should be (34)
        fspec.scenarios(2).steps(5).sourceRef.get.line should be (35)
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
    featureSpec.feature.tags(0).name should be ("wip")
    featureSpec.feature.tags(0).sourceRef.get.line should be (3)

    featureSpec.scenarios(0).tags.size should be (0)

    featureSpec.scenarios(1).tags.size should be (2)
    featureSpec.scenarios(1).tags(0).name should be ("work")
    featureSpec.scenarios(1).tags(1).name should be ("work")
    featureSpec.scenarios(1).tags(0).sourceRef.get.line should be (9)
    featureSpec.scenarios(1).tags(1).sourceRef.get.line should be (9)

    featureSpec.scenarios(2).tags.size should be (2)
    featureSpec.scenarios(2).tags(0).name should be ("work")
    featureSpec.scenarios(2).tags(1).name should be ("play")
    featureSpec.scenarios(2).tags(0).sourceRef.get.line should be (13)
    featureSpec.scenarios(2).tags(1).sourceRef.get.line should be (14)
    
    featureSpec.scenarios(3).tags.size should be (2)
    featureSpec.scenarios(3).tags(0).name should be ("wip")
    featureSpec.scenarios(3).tags(1).name should be ("play")
    featureSpec.scenarios(3).tags(0).sourceRef.get.line should be (18)
    featureSpec.scenarios(3).tags(1).sourceRef.get.line should be (18)
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
    featureSpec.scenarios(1).tags(0).name should be ("work")
    
    featureSpec.scenarios(2).tags.size should be (1)
    featureSpec.scenarios(2).tags(0).name should be ("play")
    
    featureSpec.scenarios(3).tags.size should be (2)
    featureSpec.scenarios(3).tags(0).name should be ("wip")
    featureSpec.scenarios(3).tags(1).name should be ("play")
    
  }
  
  "Scenarios with two tags on one line" should "parse" in {
    
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
    
    val featureSpec = parse(featureString)
    featureSpec.isSuccess should be (true)
    featureSpec.get.scenarios(3).tags(0).name should be ("wip")
    featureSpec.get.scenarios(3).tags(1).name should be ("play")
    
  }
    
}