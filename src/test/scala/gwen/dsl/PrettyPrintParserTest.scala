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

class PrettyPrintParserTest extends FlatSpec with Matchers with SpecNormaliser with SpecParser {

  private val parse = parseAll(spec, _: String);

  private val featureString = """
   
     @wip
     Feature: Gwen
         As a tester
       I want to automate tests
      So that gwen can run them

  Background: The butterfly effect
        Given a deterministic nonlinear system
         When a small change is initially applied
         Then a large change will eventually result

    @wip @test
    Scenario: Evaluation
        Given any software behavior
         When expressed in Gherkin
         Then Gwen can evaluate it
 
    Scenario: Evaluation
        Given any software behavior
         When expressed in Gherkin
         Then Gwen can evaluate it
 """
 
  "parsing pretty printed Gwen feature" should "yield same AST" in {
    
    val ast1 = parse(featureString).get
    val ast2 = parse(prettyPrint(ast1)).get
    
    ast1 should be (ast2)
    
  }
  
  "pretty print of normalised Gwen feature" should "replicate background for each scenario" in {
    
    val specFeature = normalise(parse(featureString).get, None, None)
    println(prettyPrint(specFeature).replace("\r", ""))
    prettyPrint(specFeature).replace("\r", "") should be ("""   @wip
   Feature: Gwen
       As a tester
     I want to automate tests
    So that gwen can run them

Background: The butterfly effect
      Given a deterministic nonlinear system
       When a small change is initially applied
       Then a large change will eventually result

  @wip @test
  Scenario: Evaluation
      Given any software behavior
       When expressed in Gherkin
       Then Gwen can evaluate it

Background: The butterfly effect
      Given a deterministic nonlinear system
       When a small change is initially applied
       Then a large change will eventually result

  Scenario: Evaluation
      Given any software behavior
       When expressed in Gherkin
       Then Gwen can evaluate it""".replace("\r", ""))
    
  }
    
}