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
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec

class FeatureParserTest extends FlatSpec with ShouldMatchers with SpecParser{

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

    assertFail("Feature",    "`:' expected but end of source found")
    assertFail("I am not a valid feature",                      "'Feature' expected")
    assertFail("Feature hey I dont have a colon after 'Feature'", "`:' expected but `h' found")
    assertFail("Hey I dont start with 'Feature:'",              "'Feature' expected")
    
  }
  
  private def assertFail(input: String, expected: String) {
    parse(input) match {
      case f: Failure => f.msg should be (expected)
      case _ => fail("failure expected")
    }
  }
  
}