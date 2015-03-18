/*
 * Copyright 2014 Branko Juric, Brady Wood
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

import org.scalatest.FunSuite
import org.scalatest.FlatSpec
import org.scalatest.Matchers

class FeatureParserTest extends FlatSpec with Matchers with SpecParser{

  private val parse = parseAll(feature, _: String);
  
  "Valid features" should "parse" in {

      parse("Feature:").get     should be (Feature(""))
      parse("Feature:\n\n").get should be (Feature(""))
    
      parse(s"Feature: Let me show you a feature\n").get       should be (Feature("Let me show you a feature"))
      parse(s"Feature:Let me show you a feature\n").get        should be (Feature("Let me show you a feature"))
      parse(s"Feature\t:Let me show you a feature\n").get      should be (Feature("Let me show you a feature"))
      parse(s"\tFeature\t:Let me show you a feature\n").get    should be (Feature("Let me show you a feature"))
      parse(s"\tFeature\t:\tLet me show you a feature\n").get  should be (Feature("Let me show you a feature"))
      
      parse(s"Feature :Let me show you a feature\n").get should be (Feature("Let me show you a feature"))
      
      parse(s"\tFeature:Let me show you a feature\n").get     should be (Feature("Let me show you a feature"))
      parse(s"Feature:\tLet me show you a feature\n").get     should be (Feature("Let me show you a feature"))
      parse(s"Feature:\tLet me show you a feature\t\n").get   should be (Feature("Let me show you a feature"))
      parse(s"Feature:\tLet me show you a feature \n").get    should be (Feature("Let me show you a feature"))
      parse(s"Feature:\tLet me show you a feature\t \n").get  should be (Feature("Let me show you a feature"))
      
      parse(s"Feature: Let me show you a Background feature\n").get       should be (Feature("Let me show you a Background feature"))
      parse(s"Feature: Let me show you a Background: feature\n").get      should be (Feature("Let me show you a Background: feature"))
      
      parse(s"Feature: Let me show you a Scenario feature\n").get       should be (Feature("Let me show you a Scenario feature"))
      parse(s"Feature: Let me show you a Scenario: feature\n").get      should be (Feature("Let me show you a Scenario: feature"))
      
      StepKeyword.values foreach { keyword =>
        parse(s"Feature: I contain a $keyword keyword\n").get should be (Feature(s"I contain a $keyword keyword"))
      }
  }
  
  "Invalid features" should "not parse" in {

    val expected = "Invalid feature declaration, expected >> Feature: name<eol>, [As ..<eol> I want ..<eol> [So that ..<eol>]]"
    assertFail("Feature", expected)
    assertFail("I am not a valid feature", expected)
    assertFail("Feature hey I dont have a colon after 'Feature'", expected)
    assertFail("Hey I dont start with 'Feature:'", expected)
    
  }
  
  "Features with valid narratives" should "parse" in {

    var feature = parse(
      """Feature: let me tell you something
           As a tester
           I want to test all behavior
           So that there are no suprises""").get
     
    feature.name should be ("let me tell you something")
    feature.narrative.mkString(" ") should be ("As a tester I want to test all behavior So that there are no suprises")
    
    feature = parse(
      """Feature: let me tell you something
           As a tester
           I want to test all behavior""").get
     
    feature.name should be ("let me tell you something")
    feature.narrative.mkString(" ") should be ("As a tester I want to test all behavior")
    
    feature = parse(
      """Feature: let me tell you something
           As an experienced tester
           I want to test all behavior
           So that there are no suprises""").get
     
    feature.name should be ("let me tell you something")
    feature.narrative.mkString(" ") should be ("As an experienced tester I want to test all behavior So that there are no suprises")
    
    feature = parse(
      """Feature: let me tell you something
           As TESTCO
           I want to test all behavior
           So that there are no suprises""").get
     
    feature.name should be ("let me tell you something")
    feature.narrative.mkString(" ") should be ("As TESTCO I want to test all behavior So that there are no suprises")
    
  }
  
  "Features with invalid narratives" should "not parse" in {
    assertFail("""Feature: let me tell you something
                    As a tester
                    I wish to test all behavior
                    So that there are no suprises""", "I want ..<eol> expected")
     
    assertFail("""Feature: let me tell you something
                    As an experienced tester
                    I want to test all behavior
                    So there are no suprises""", """string matching regex `\z' expected but `S' found""")
  }
  
  private def assertFail(input: String, expected: String) {
    parse(input) match {
      case f: Failure => f.msg should be (expected)
      case _ => fail("failure expected")
    }
  }
  
}