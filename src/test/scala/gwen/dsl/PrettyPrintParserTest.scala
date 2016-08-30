/*
 * Copyright 2014-2015 Branko Juric, Brady Wood
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
import scala.util.Success
import scala.util.Success

class PrettyPrintParserTest extends FlatSpec with Matchers with SpecNormaliser with GherkinParser {

  private val parse = parseFeatureSpec(_: String);

  private val featureString = """
   
     @wip
     Feature: Gwen
              As a tester
              I want to automate tests
              So that gwen can run them

  Background: The butterfly effect
              Sensitivity to initial conditions
        Given a deterministic nonlinear system
         When a small change is initially applied
         Then a large change will eventually result

    @wip @test
    Scenario: Evaluation
              Gwen for executable specifications
              Business specs mapped to meta
        Given any software behavior
         When expressed in Gherkin
         Then Gwen can evaluate it
 
    Scenario: Evaluation
        Given any software behavior
         When expressed in Gherkin
         Then Gwen can evaluate it
 """
 
  "parsing pretty printed Gwen feature" should "yield same AST" in {
    
    val ast1 = parse(featureString)
    val ast2 = parse(prettyPrint(ast1.get))
    
    ast2 match {
      case Success(featureSpec) => 
        featureSpec.feature should be (Feature(List(Tag("wip")), "Gwen", List("As a tester", "I want to automate tests", "So that gwen can run them")))
        featureSpec.background.get should be {
          Background("The butterfly effect", List("Sensitivity to initial conditions"),
            List(
              Step(Position(9, 7), StepKeyword.Given, "a deterministic nonlinear system"),
              Step(Position(10, 8), StepKeyword.When,  "a small change is initially applied"),
              Step(Position(11, 8), StepKeyword.Then,  "a large change will eventually result")
            )
          )
        }
        featureSpec.scenarios should be {
          List(
            Scenario(List(Tag("wip"), Tag("test")), "Evaluation", List("Gwen for executable specifications", "Business specs mapped to meta"), None,
              List(
                Step(Position(17, 7), StepKeyword.Given, "any software behavior"),
                Step(Position(18, 8), StepKeyword.When,  "expressed in Gherkin"),
                Step(Position(19, 8), StepKeyword.Then,  "Gwen can evaluate it")
              )
            ),
            Scenario(List[Tag](), "Evaluation", Nil, None, 
              List(
                Step(Position(22, 7), StepKeyword.Given, "any software behavior"),
                Step(Position(23, 8), StepKeyword.When,  "expressed in Gherkin"),
                Step(Position(24, 8), StepKeyword.Then,  "Gwen can evaluate it")
              )
            )
          )
        }
      case e => fail(e.toString)
     }
    
  }
  
  "pretty print of normalised Gwen feature" should "replicate background for each scenario" in {
    
    val specFeature = normalise(parse(featureString).get, None, None)
    prettyPrint(specFeature).replace("\r", "") should be ("""   @wip
   Feature: Gwen
            As a tester
            I want to automate tests
            So that gwen can run them

Background: The butterfly effect
            Sensitivity to initial conditions
      Given a deterministic nonlinear system
       When a small change is initially applied
       Then a large change will eventually result

  @wip @test
  Scenario: Evaluation
            Gwen for executable specifications
            Business specs mapped to meta
      Given any software behavior
       When expressed in Gherkin
       Then Gwen can evaluate it

Background: The butterfly effect
            Sensitivity to initial conditions
      Given a deterministic nonlinear system
       When a small change is initially applied
       Then a large change will eventually result

  Scenario: Evaluation
      Given any software behavior
       When expressed in Gherkin
       Then Gwen can evaluate it""".replace("\r", ""))
    
  }
    
}