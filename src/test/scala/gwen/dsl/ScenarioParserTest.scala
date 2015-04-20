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
import org.scalatest.Matchers
import org.scalatest.FlatSpec

class ScenarioParserTest extends FlatSpec with Matchers with SpecParser {

  private val parse = parseAll(scenario, _: String);
  
  private val step1 = Step(StepKeyword.Given, "I am step 1")
  private val step2 = Step(StepKeyword.Then, "I am not step 1")
  
  private val comment1 = "# I am single line hash comment"
  
  "Valid scenarios" should "parse" in {
      
      parse("Scenario:").get   should be (Scenario(Set[Tag](), "", None, Nil))
      parse("Scenario:\n").get should be (Scenario(Set[Tag](), "", None, Nil))
      
      parse(s"Scenario :name\n$step1").get      should be (Scenario(Set[Tag](), "name", None, List(step1)))
      parse(s"Scenario\t:name\n$step1").get     should be (Scenario(Set[Tag](), "name", None, List(step1)))
      parse(s"\tScenario\t:name\n$step1").get   should be (Scenario(Set[Tag](), "name", None, List(step1)))
      parse(s"\tScenario\t:\tname\n$step1").get should be (Scenario(Set[Tag](), "name", None, List(step1)))
    
      parse(s"Scenario: name\n$step1").get should be (Scenario(Set[Tag](), "name", None, List(step1)))
      parse(s"Scenario:name\n$step1").get  should be (Scenario(Set[Tag](), "name", None, List(step1)))
      
      parse(s"\tScenario:name\n$step1").get     should be (Scenario(Set[Tag](), "name", None, List(step1)))
      parse(s"Scenario:\tname\n$step1").get     should be (Scenario(Set[Tag](), "name", None, List(step1)))
      parse(s"Scenario:\tname\t\n$step1").get   should be (Scenario(Set[Tag](), "name", None, List(step1)))
      parse(s"Scenario:\tname \n$step1").get    should be (Scenario(Set[Tag](), "name", None, List(step1)))
      parse(s"Scenario:\tname\t \n$step1").get  should be (Scenario(Set[Tag](), "name", None, List(step1)))
      
      parse(s"Scenario: name\n$step1\n$step2").get should be (Scenario(Set[Tag](), "name", None, List(step1, step2)))
      
      parse(s"Scenario: name\n$step1\n$comment1").get            should be (Scenario(Set[Tag](), "name", None, List(step1)))
      parse(s"Scenario: name\n$step1\n$step2\n$comment1").get    should be (Scenario(Set[Tag](), "name", None, List(step1, step2)))
      parse(s"Scenario: name\n$step1\n$comment1\n$step2").get    should be (Scenario(Set[Tag](), "name", None, List(step1, step2)))
      parse(s"Scenario: name\n$comment1\n$step1\n$step2").get    should be (Scenario(Set[Tag](), "name", None, List(step1, step2)))
      
      parse(s"Scenario:\n$step1\n$step2").get    should be (Scenario(Set[Tag](), s"$step1", None, List(step2)))
      parse(s"Scenario: \n$step1\n$step2").get   should be (Scenario(Set[Tag](), s"$step1", None, List(step2)))
      parse(s"Scenario:\t\n$step1\n$step2").get  should be (Scenario(Set[Tag](), s"$step1", None, List(step2)))
      parse(s"Scenario:\t \n$step1\n$step2").get should be (Scenario(Set[Tag](), s"$step1", None, List(step2)))
      parse(s"Scenario: \t\n$step1\n$step2").get should be (Scenario(Set[Tag](), s"$step1", None, List(step2)))
      
      parse("Scenario: I dont have any steps").get should be (Scenario(Set[Tag](), "I dont have any steps", None, Nil))
      
      StepKeyword.values foreach { keyword =>
        parse(s"Scenario: I contain a $keyword keyword in name\n$step1").get should be (Scenario(Set[Tag](), s"I contain a $keyword keyword in name", None, List(step1)))
      }
  }
  
  "Invalid scenarios" should "not parse" in {

    assertFail("Scenario",                            "`:' expected but end of source found")
    assertFail("I am not a valid scenario",           "'Scenario|Given|When|Then|And|But' expected")
    assertFail("Scenario no colon after 'Scenario:'", "`:' expected but `n' found")
     
  }
  
  private def assertFail(input: String, expected: String) {
    parse(input) match {
      case f: Failure => f.msg should be (expected)
      case _ => fail("failure expected")
    }
  }
  
}