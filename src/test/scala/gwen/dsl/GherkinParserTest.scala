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

package gwen.dsl

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import scala.util.Success

class GherkinParserTest extends FlatSpec with Matchers with GherkinParser {

  object Feature {
   def apply(name: String, description: List[String]): Feature = new Feature("en", Nil, name, description)
  }

  object Scenario {
    def apply(tags: List[Tag], name: String, description: List[String], background: Option[Background], steps: List[Step]): Scenario =
      new Scenario(tags.distinct, FeatureKeyword.Scenario.toString, name, description, background, steps, isOutline = false, Nil, None)
  }

  private val parse = parseFeatureSpec(_: String)
  
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
    
    val featureSpec = parse(featureString) match {
      case Success(fspec) =>
        fspec.feature should be (Feature("Gwen", List("As a tester", "I want to automate tests", "So that gwen can run them")))
        fspec.background.get should be {
          Background("The butterfly effect", List("Sensitivity to initial conditions"), 
            List(
              Step(StepKeyword.Given, "a deterministic nonlinear system"),
              Step(StepKeyword.When,  "a small change is initially applied"),
              Step(StepKeyword.Then,  "a large change will eventually result")
            )
          )
        }
        fspec.scenarios should be {
          List(
            Scenario(List[Tag](), "Evaluation", Nil, None,
              List(
                Step(StepKeyword.Given, "any software behavior"),
                Step(StepKeyword.When,  "expressed in Gherkin"),
                Step(StepKeyword.Then,  "Gwen can evaluate it")
              )
            ),
            Scenario(List[Tag](), "The useless test", Nil, None, 
              List(
                Step(StepKeyword.Given, "I am a test"),
                Step(StepKeyword.And,   "I am generated from code"),
                Step(StepKeyword.When,  "the code changes"),
                Step(StepKeyword.Then,  "I change"),
                Step(StepKeyword.And,   "so I won't fail"),
                Step(StepKeyword.And,   "that's why I'm useless")
              )
            ),
            Scenario(List[Tag](), "The useful test", Nil, None, 
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
        fspec.feature.pos should be (Position(3, 6))
        fspec.background.get.pos should be (Position(8, 3))
        fspec.scenarios(0).pos should be (Position(16, 5))
        fspec.scenarios(0).steps(0).pos should be (Position(17, 9))
        fspec.scenarios(0).steps(1).pos should be (Position(18, 10))
        fspec.scenarios(0).steps(2).pos should be (Position(19, 10))
        fspec.scenarios(1).pos should be (Position(21, 5))
        fspec.scenarios(1).steps(0).pos should be (Position(22, 9))
        fspec.scenarios(1).steps(1).pos should be (Position(23, 11))
        fspec.scenarios(1).steps(2).pos should be (Position(24, 10))
        fspec.scenarios(1).steps(3).pos should be (Position(25, 10))
        fspec.scenarios(1).steps(4).pos should be (Position(26, 11))
        fspec.scenarios(1).steps(5).pos should be (Position(27, 11))
        fspec.scenarios(2).pos should be (Position(29, 5))
        fspec.scenarios(2).steps(0).pos should be (Position(30, 9))
        fspec.scenarios(2).steps(1).pos should be (Position(31, 11))
        fspec.scenarios(2).steps(2).pos should be (Position(32, 10))
        fspec.scenarios(2).steps(3).pos should be (Position(33, 10))
        fspec.scenarios(2).steps(4).pos should be (Position(34, 11))
        fspec.scenarios(2).steps(5).pos should be (Position(35, 11))
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
    featureSpec.feature.tags(0).pos should be (Position(3, 8))

    featureSpec.scenarios(0).tags.size should be (0)

    featureSpec.scenarios(1).tags.size should be (1)
    featureSpec.scenarios(1).tags.contains(Tag("work")) should be (true)
    featureSpec.scenarios(1).tags(0).pos should be (Position(9, 7))

    featureSpec.scenarios(2).tags.size should be (2)
    featureSpec.scenarios(2).tags.contains(Tag("work")) should be (true)
    featureSpec.scenarios(2).tags.contains(Tag("play")) should be (true)
    featureSpec.scenarios(2).tags(0).pos should be (Position(13, 7))
    featureSpec.scenarios(2).tags(1).pos should be (Position(14, 7))
    
    featureSpec.scenarios(3).tags.size should be (2)
    featureSpec.scenarios(3).tags.contains(Tag("wip")) should be (true)
    featureSpec.scenarios(3).tags.contains(Tag("play")) should be (true)
    featureSpec.scenarios(3).tags(0).pos should be (Position(18, 7))
    featureSpec.scenarios(3).tags(1).pos should be (Position(18, 12))
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
    featureSpec.get.scenarios(3).tags.contains(Tag("wip")) should be (true)
    featureSpec.get.scenarios(3).tags.contains(Tag("play")) should be (true)
    
  }
    
}