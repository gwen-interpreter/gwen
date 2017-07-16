/*
 * Copyright 2014-2017 Branko Juric, Brady Wood
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

class PrettyPrintParserTest extends FlatSpec with Matchers with SpecNormaliser with GherkinParser {

  object Scenario {
    def apply(tags: List[Tag], name: String, description: List[String], background: Option[Background], steps: List[Step]): Scenario =
      new Scenario(tags.distinct, name, description, background, steps, isOutline = false, Nil, None)
  }

  private val parse = parseFeatureSpec(_: String)

  private val featureString = s"""# language: en
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

    @Outline
    Scenario Outline: Join two strings together
              This scenario is evaluated at the point where the outline is declared
        Given string 1 is "<string 1>"
          And string 2 is "<string 2>"
         When I join the two strings
         Then the result should be "<result>"
    Examples: Basic string concatenation
              The header row contains the placeholder names. The body rows that
              follow contain the data that is bound to each scenario that is evaluated.
              | string 1 | string 2 | result   |
              | howdy    | doo      | howdydoo |
              | any      | thing    | anything |

    Scenario: Numbers as words
        Given a mapping of words to numbers
         Then the word should match the number
         | one   | 1 |
         | two   | 2 |
         | three | 3 |

    Scenario: Multiline DocString
        Given my line is
              ${"\"\"\""}
              Gwen is a Gherkin interpreter that turns
              Given-When-Then steps into automation instructions.
              ${"\"\"\""}"""
 
  "parsing pretty printed Gwen feature" should "yield same AST" in {
    
    val ast1 = parse(featureString)
    val ast2 = parse(prettyPrint(ast1.get))
    
    ast2 match {
      case Success(featureSpec) => 
        featureSpec.feature should be (Feature("en", List(Tag("wip")), "Gwen", List("As a tester", "I want to automate tests", "So that gwen can run them")))
        featureSpec.background.get should be {
          Background("The butterfly effect", List("Sensitivity to initial conditions"),
            List(
              Step(StepKeyword.Given, "a deterministic nonlinear system"),
              Step(StepKeyword.When,  "a small change is initially applied"),
              Step(StepKeyword.Then,  "a large change will eventually result")
            )
          )
        }
        featureSpec.scenarios(0) should be {

          Scenario(List(Tag("wip"), Tag("test")), "Evaluation", List("Gwen for executable specifications", "Business specs mapped to meta"), None,
            List(
              Step(StepKeyword.Given, "any software behavior"),
              Step(StepKeyword.When,  "expressed in Gherkin"),
              Step(StepKeyword.Then,  "Gwen can evaluate it")
            )
          )
        }
        featureSpec.scenarios(1) should be {
          Scenario(List[Tag](), "Evaluation", Nil, None,
            List(
              Step(StepKeyword.Given, "any software behavior"),
              Step(StepKeyword.When,  "expressed in Gherkin"),
              Step(StepKeyword.Then,  "Gwen can evaluate it")
            )
          )
        }
      case e => fail(e.toString)
     }
    
  }
  
  "pretty print of parsed Gwen feature" should "not be normalised" in {
    
    val specFeature = parse(featureString).get
    prettyPrint(specFeature).replace("\r", "") should be (s"""   @wip
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

  @Outline
  Scenario Outline: Join two strings together
            This scenario is evaluated at the point where the outline is declared
      Given string 1 is "<string 1>"
        And string 2 is "<string 2>"
       When I join the two strings
       Then the result should be "<result>"
  Examples: Basic string concatenation
            The header row contains the placeholder names. The body rows that
            follow contain the data that is bound to each scenario that is evaluated.
            | string 1 | string 2 | result   |
            | howdy    | doo      | howdydoo |
            | any      | thing    | anything |

  Scenario: Numbers as words
      Given a mapping of words to numbers
       Then the word should match the number
            | one   | 1 |
            | two   | 2 |
            | three | 3 |

  Scenario: Multiline DocString
      Given my line is
            ${"\"\"\""}
            Gwen is a Gherkin interpreter that turns
            Given-When-Then steps into automation instructions.
            ${"\"\"\""}""".replace("\r", ""))
    
  }

  "pretty print of normalised Gwen feature" should "replicate background for each expanded scenario" in {

    val specFeature = normalise(parse(featureString).get, None, None)
    prettyPrint(specFeature).replace("\r", "") should be (s"""   @wip
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
       Then Gwen can evaluate it

Background: The butterfly effect
            Sensitivity to initial conditions
      Given a deterministic nonlinear system
       When a small change is initially applied
       Then a large change will eventually result

  @Outline
  Scenario: Join two strings together -- Example 1.1 Basic string concatenation
            This scenario is evaluated at the point where the outline is declared
      Given string 1 is "howdy"
        And string 2 is "doo"
       When I join the two strings
       Then the result should be "howdydoo"

Background: The butterfly effect
            Sensitivity to initial conditions
      Given a deterministic nonlinear system
       When a small change is initially applied
       Then a large change will eventually result

  @Outline
  Scenario: Join two strings together -- Example 1.2 Basic string concatenation
            This scenario is evaluated at the point where the outline is declared
      Given string 1 is "any"
        And string 2 is "thing"
       When I join the two strings
       Then the result should be "anything"

Background: The butterfly effect
            Sensitivity to initial conditions
      Given a deterministic nonlinear system
       When a small change is initially applied
       Then a large change will eventually result

  Scenario: Numbers as words
      Given a mapping of words to numbers
       Then the word should match the number
            | one   | 1 |
            | two   | 2 |
            | three | 3 |

Background: The butterfly effect
            Sensitivity to initial conditions
      Given a deterministic nonlinear system
       When a small change is initially applied
       Then a large change will eventually result

  Scenario: Multiline DocString
      Given my line is
            ${"\"\"\""}
            Gwen is a Gherkin interpreter that turns
            Given-When-Then steps into automation instructions.
            ${"\"\"\""}""".replace("\r", ""))

  }
    
}