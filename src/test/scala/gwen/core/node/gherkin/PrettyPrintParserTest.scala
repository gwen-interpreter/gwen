/*
 * Copyright 2014-2024 Branko Juric, Brady Wood
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

package gwen.core.node.gherkin

import gwen.core.BaseTest
import gwen.core.GwenOptions
import gwen.core.node.Root
import gwen.core.node.gherkin.GherkinParser
import gwen.core.node.gherkin.SpecNormaliser

import org.scalatest.matchers.should.Matchers

class PrettyPrintParserTest extends BaseTest with Matchers with SpecNormaliser with GherkinParser {

  private val parse = parseSpec(_: String)
  private val printer = new SpecPrinter(deep = true, verbatim = false, colors = false)

  private val featureString = s"""@wip
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

    Given any software behaviour
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
          ${"\"\"\""}
"""
 
  "parsing pretty printed Gwen feature" should "yield same AST" in {
    val ast1 = parse(featureString)
    val ast2 = parse(printer.prettyPrint(Root, ast1.get))
    printer.prettyPrint(Root, ast2.get).replace("\r", "") should be (featureString.replace("\r", ""))
  }

  "pretty print of normalised Gwen feature" should "replicate background for each expanded scenario" in {

    val options = GwenOptions(dryRun = false, parallel = false)
    val specFeature = normaliseSpec(parse(featureString).get, None , options)
    printer.prettyPrint(Root, specFeature).replace("\r", "") should be (s"""@wip
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

    Given any software behaviour
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

  Background: The butterfly effect + Data table record [1 of 2]

    Sensitivity to initial conditions

    Given @Data string 1 is "howdy"
      And @Data string 2 is "doo"
      And @Data result is "howdydoo"
      And a deterministic nonlinear system
     When a small change is initially applied
     Then a large change will eventually result

  @Outline
  Scenario: Join two strings together -- Basic string concatenation [1 of 2]

    This scenario is evaluated at the point where the outline is declared

    Given string 1 is "howdy"
      And string 2 is "doo"
     When I join the two strings
     Then the result should be "howdydoo"

  Background: The butterfly effect + Data table record [2 of 2]

    Sensitivity to initial conditions

    Given @Data string 1 is "any"
      And @Data string 2 is "thing"
      And @Data result is "anything"
      And a deterministic nonlinear system
     When a small change is initially applied
     Then a large change will eventually result

  @Outline
  Scenario: Join two strings together -- Basic string concatenation [2 of 2]

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
          ${"\"\"\""}
""".replace("\r", ""))

  }
    
}