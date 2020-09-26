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

class FeatureParserTest extends FlatSpec with Matchers with GherkinParser {

  object Feature {
   def apply(name: String, description: List[String]): Feature = new Feature("en", Nil, "Feature", name, description)
  }

  private def parse(input: String) = parseFeatureSpec(input).map(_.feature)
  
  "Valid features" should "parse" in {

      parse("Feature:").get     should be (Feature("", Nil))
      parse("Feature:\n\n").get should be (Feature("", Nil))
    
      parse(s"Feature: Let me show you a feature\n").get       should be (Feature("Let me show you a feature", Nil))
      parse(s"Feature:Let me show you a feature\n").get        should be (Feature("Let me show you a feature", Nil))
      
      parse(s"\tFeature:Let me show you a feature\n").get     should be (Feature("Let me show you a feature", Nil))
      parse(s"Feature:\tLet me show you a feature\n").get     should be (Feature("Let me show you a feature", Nil))
      parse(s"Feature:\tLet me show you a feature\t\n").get   should be (Feature("Let me show you a feature", Nil))
      parse(s"Feature:\tLet me show you a feature \n").get    should be (Feature("Let me show you a feature", Nil))
      parse(s"Feature:\tLet me show you a feature\t \n").get  should be (Feature("Let me show you a feature", Nil))
      
      parse(s"Feature: Let me show you a Background feature\n").get       should be (Feature("Let me show you a Background feature", Nil))
      parse(s"Feature: Let me show you a Background: feature\n").get      should be (Feature("Let me show you a Background: feature", Nil))
      
      parse(s"Feature: Let me show you a Scenario feature\n").get       should be (Feature("Let me show you a Scenario feature", Nil))
      parse(s"Feature: Let me show you a Scenario: feature\n").get      should be (Feature("Let me show you a Scenario: feature", Nil))
      
      StepKeyword.values foreach { keyword =>
        parse(s"Feature: I contain a $keyword keyword\n").get should be (Feature(s"I contain a $keyword keyword", Nil))
      }
  }
  
  "Invalid features" should "not parse" in {

    assertFail("Feature\t:Let me show you a feature\n")
    assertFail("\tFeature\t:Let me show you a feature\n")
    assertFail("\tFeature\t:\tLet me show you a feature\n")
    assertFail("Feature :Let me show you a feature\n")
    assertFail("Feature")
    assertFail("I am not a valid feature")
    assertFail("Feature hey I dont have a colon after 'Feature'")
    assertFail("Hey I dont start with 'Feature:'")
    
  }
  
  "Features with story-like description" should "parse" in {

    var feature = parse(
      """Feature: let me tell you something
           As a tester
           I want to test all behavior
           So that there are no suprises""").get
     
    feature.name should be ("let me tell you something")
    feature.description.length should be (3)
    feature.description(0) should be ("As a tester")
    feature.description(1) should be ("I want to test all behavior")
    feature.description(2) should be ("So that there are no suprises")
    
    feature = parse(
      """Feature: let me tell you something
           As a tester
           I want to test all behavior""").get
     
    feature.name should be ("let me tell you something")
    feature.description.length should be (2)
    feature.description(0) should be ("As a tester")
    feature.description(1) should be ("I want to test all behavior")
    
    feature = parse(
      """Feature: let me tell you something
           As an experienced tester
           I want to test all behavior
           So that there are no suprises""").get
     
    feature.name should be ("let me tell you something")
    feature.description.length should be (3)
    feature.description(0) should be ("As an experienced tester")
    feature.description(1) should be ("I want to test all behavior")
    feature.description(2) should be ("So that there are no suprises")
    
    feature = parse(
      """Feature: let me tell you something
           As TESTCO
           I want to test all behavior
           So that there are no suprises""").get
     
    feature.name should be ("let me tell you something")
    feature.description.length should be (3)
    feature.description(0) should be ("As TESTCO")
    feature.description(1) should be ("I want to test all behavior")
    feature.description(2) should be ("So that there are no suprises")
    
  }
  
  "Features with non story-like description" should "parse" in {
    val feature = parse(
      """Feature: let me tell you something
            Some miscellaneous random
            text
            
            and some more text after a blank line""").get
     
    feature.name should be ("let me tell you something")
    feature.description.length should be (4)
    feature.description(0) should be ("Some miscellaneous random")
    feature.description(1) should be ("text")
    feature.description(2) should be ("")
    feature.description(3) should be ("and some more text after a blank line")
  }
  
  private def assertFail(input: String): Unit = {
    parse(input) match {
      case Success(_) => fail("failure expected") 
      case _ => 
    }
  }
  
}