/*
 * Copyright 2019-2021 Branko Juric, Brady Wood
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

import scala.util.Success

import org.scalatest.Matchers
import org.scalatest.FlatSpec

class RuleTest extends FlatSpec with Matchers with GherkinParser {

  private def parse(input: String) = parseSpec(s"Feature: ftest\n$input").filter(_.rules.nonEmpty).map(_.rules.head)
  
  private def background = """
    Background: The butterfly effect
                Sensitivity to initial conditions
          Given a deterministic nonlinear system
           When a small change is initially applied
           Then a large change will eventually result"""

  private def scenario1 = """
    Scenario: A butterfly might impact a tornado
        Given a butterfly
          And a tornado
         When the butterfly flaps its wings
         Then the change in atmosphere might seam neglibile
          But it could alter the path of the tornado"""

  private def scenario2 = """
    Scenario: Declarative to imperative mapping
              Gwen for executable specifications
              Business specs mapped to meta
        Given any software behavior
         When expressed in Gherkin
         Then Gwen can evaluate it"""
  
  private val comment1 = "# I am single line hash comment"
  
  "Valid rules with no background" should "parse" in {
      
    assertRule("Rule:", "", Nil, false, 0)
    assertRule("Rule:\n", "", Nil, false, 0)
      
    assertRule(s"Rule:name\n$scenario1", "name", Nil, false, 1)
    assertRule(s"Rule: name\n$scenario1", "name", Nil, false, 1)
    
    assertRule(s"Rule:name\nI am a test scenario\n$scenario1", "name", List("I am a test scenario"), false, 1)
    assertRule(s"Rule: name\nI am another\nmultiline\n\nscenario\n$scenario1", "name", List("I am another", "multiline", "", "scenario"), false, 1)
    
    assertRule(s"\tRule:name\n$scenario1", "name", Nil, false, 1)
    assertRule(s"Rule:\tname\n$scenario1", "name", Nil, false, 1)
    assertRule(s"Rule:\tname\t\n$scenario1", "name", Nil, false, 1)
    assertRule(s"Rule:\tname \n$scenario1", "name", Nil, false, 1)
    assertRule(s"Rule:\tname\t \n$scenario1", "name", Nil, false, 1)
    
    assertRule(s"Rule: name\n$scenario1\n$scenario2", "name", Nil, false, 2)
    
    assertRule(s"Rule: name\n$scenario1\n$comment1", "name", Nil, false, 1)
    assertRule(s"Rule: name\n$scenario1\n$scenario2\n$comment1", "name", Nil, false, 2)
    assertRule(s"Rule: name\n$scenario1\n$comment1\n$scenario2", "name", Nil, false, 2)
    assertRule(s"Rule: name\n$comment1\n$scenario1\n$scenario2", "name", Nil, false, 2)
    
    assertRule(s"Rule:\n$scenario1\n$scenario2", "", Nil, false, 2)
    assertRule(s"Rule: \n$scenario1\n$scenario2", "", Nil, false, 2)
    
    assertRule("Rule: I dont have any scenarios", "I dont have any scenarios", Nil, false, 0)
    
  }

  "Valid rules with background" should "parse" in {
      
    assertRule(s"Rule:\n$background", "", Nil, true, 0)
    assertRule(s"Rule:\n\n$background", "", Nil, true, 0)
      
    assertRule(s"Rule:name\n$background\n$scenario1", "name", Nil, true, 1)
    assertRule(s"Rule: name\n$background\n$scenario1", "name", Nil, true, 1)
    
    assertRule(s"Rule:name\nI am a test scenario\n$background\n$scenario1", "name", List("I am a test scenario"), true, 1)
    assertRule(s"Rule: name\nI am another\nmultiline\n\nscenario\n$background\n$scenario1", "name", List("I am another", "multiline", "", "scenario"), true, 1)
    
    assertRule(s"\tRule:name\n$background\n$scenario1", "name", Nil, true, 1)
    assertRule(s"Rule:\tname\n$background\n$scenario1", "name", Nil, true, 1)
    assertRule(s"Rule:\tname\t\n$background\n$scenario1", "name", Nil, true, 1)
    assertRule(s"Rule:\tname \n$background\n$scenario1", "name", Nil, true, 1)
    assertRule(s"Rule:\tname\t \n$background\n$scenario1", "name", Nil, true, 1)
    
    assertRule(s"Rule: name\n$background\n$scenario1\n$scenario2", "name", Nil, true, 2)
    
    assertRule(s"Rule: name\n$background\n$scenario1\n$comment1", "name", Nil, true, 1)
    assertRule(s"Rule: name\n$background\n$scenario1\n$scenario2\n$comment1", "name", Nil, true, 2)
    assertRule(s"Rule: name\n$background\n$scenario1\n$comment1\n$scenario2", "name", Nil, true, 2)
    assertRule(s"Rule: name\n$background\n$comment1\n$scenario1\n$scenario2", "name", Nil, true, 2)
    
    assertRule(s"Rule:\n$background\n$scenario1\n$scenario2", "", Nil, true, 2)
    assertRule(s"Rule: \n$background\n$scenario1\n$scenario2", "", Nil, true, 2)
    
    assertRule(s"Rule: I dont have any scenarios\n$background", "I dont have any scenarios", Nil, true, 0)
    
  }

  private def assertRule(ruleStr: String, name: String, description: List[String], hasBackground: Boolean, noOfScenarios: Int): Unit = {
    def rule = parse(ruleStr).get
    rule.name should be (name)
    rule.description should be (description)
    rule.background.nonEmpty should be (hasBackground)
    rule.scenarios.size should be (noOfScenarios)
  }
  
  "Invalid rules" should "not parse" in {

    assertFail(s"Rule :name\n$scenario1")
    assertFail(s"Rule\t:name\n$scenario1")
    assertFail(s"\tRule\t:name\n$scenario1")
    assertFail(s"\tRule\t:\tname\n$scenario1")
    
    assertFail("Rule")
    assertFail("I am not a valid rule")
    assertFail("Rule no colon after 'Rule:'")
     
  }
  
  private def assertFail(input: String): Unit = {
    parse(input) match {
      case Success(_) => fail("failure expected") 
      case _ => 
    }
  }
  
}