/*
 * Copyright 2014-2017 Branko Juric, Brady Wood
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

package gwen.model.gherkin

import gwen.TestModel
import gwen.model.Position
import gwen.model.StepKeyword
import gwen.model.Tag
import gwen.model.gherkin.GherkinParser

import scala.util.Success

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class GherkinParserTest extends FlatSpec with Matchers with GherkinParser with TestModel {

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
        feature should be (Feature("Gwen", List("As a tester", "I want to automate tests", "So that gwen can run them")))
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
                Step(StepKeyword.Given.toString, "any software behavior"),
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
        fspec.feature.sourceRef.get.pos should be (Position(3, 6, 0))
        fspec.background.get.sourceRef.get.pos should be (Position(8, 3, 0))
        fspec.scenarios(0).sourceRef.get.pos should be (Position(16, 5, 0))
        fspec.scenarios(0).steps(0).sourceRef.get.pos should be (Position(17, 9, 0))
        fspec.scenarios(0).steps(1).sourceRef.get.pos should be (Position(18, 10, 1))
        fspec.scenarios(0).steps(2).sourceRef.get.pos should be (Position(19, 10, 2))
        fspec.scenarios(1).sourceRef.get.pos should be (Position(21, 5, 1))
        fspec.scenarios(1).steps(0).sourceRef.get.pos should be (Position(22, 9, 0))
        fspec.scenarios(1).steps(1).sourceRef.get.pos should be (Position(23, 11, 1))
        fspec.scenarios(1).steps(2).sourceRef.get.pos should be (Position(24, 10, 2))
        fspec.scenarios(1).steps(3).sourceRef.get.pos should be (Position(25, 10, 3))
        fspec.scenarios(1).steps(4).sourceRef.get.pos should be (Position(26, 11, 4))
        fspec.scenarios(1).steps(5).sourceRef.get.pos should be (Position(27, 11, 5))
        fspec.scenarios(2).sourceRef.get.pos should be (Position(29, 5, 2))
        fspec.scenarios(2).steps(0).sourceRef.get.pos should be (Position(30, 9, 0))
        fspec.scenarios(2).steps(1).sourceRef.get.pos should be (Position(31, 11, 1))
        fspec.scenarios(2).steps(2).sourceRef.get.pos should be (Position(32, 10, 2))
        fspec.scenarios(2).steps(3).sourceRef.get.pos should be (Position(33, 10, 3))
        fspec.scenarios(2).steps(4).sourceRef.get.pos should be (Position(34, 11, 4))
        fspec.scenarios(2).steps(5).sourceRef.get.pos should be (Position(35, 11, 5))
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
    featureSpec.feature.tags(0).sourceRef.get.pos should be (Position(3, 8, 0))

    featureSpec.scenarios(0).tags.size should be (0)

    featureSpec.scenarios(1).tags.size should be (2)
    featureSpec.scenarios(1).tags(0).name should be ("work")
    featureSpec.scenarios(1).tags(1).name should be ("work")
    featureSpec.scenarios(1).tags(0).sourceRef.get.pos should be (Position(9, 7, 0))
    featureSpec.scenarios(1).tags(1).sourceRef.get.pos should be (Position(9, 13, 1))

    featureSpec.scenarios(2).tags.size should be (2)
    featureSpec.scenarios(2).tags(0).name should be ("work")
    featureSpec.scenarios(2).tags(1).name should be ("play")
    featureSpec.scenarios(2).tags(0).sourceRef.get.pos should be (Position(13, 7, 0))
    featureSpec.scenarios(2).tags(1).sourceRef.get.pos should be (Position(14, 7, 1))
    
    featureSpec.scenarios(3).tags.size should be (2)
    featureSpec.scenarios(3).tags(0).name should be ("wip")
    featureSpec.scenarios(3).tags(1).name should be ("play")
    featureSpec.scenarios(3).tags(0).sourceRef.get.pos should be (Position(18, 7, 0))
    featureSpec.scenarios(3).tags(1).sourceRef.get.pos should be (Position(18, 12, 1))
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