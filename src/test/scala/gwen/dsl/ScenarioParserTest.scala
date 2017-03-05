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

import org.scalatest.Matchers
import org.scalatest.FlatSpec

import scala.util.{Success, Try}

class ScenarioParserTest extends FlatSpec with Matchers with GherkinParser {

  private def parse(input: String) = parseFeatureSpec(s"Feature: ftest\n$input").filter(_.scenarios.nonEmpty).map(_.scenarios.head)
  private def parseScenarioOutline(input: String) = parseFeatureSpec(s"Feature: ftest\n$input").filter(_.scenarios.nonEmpty).map(_.scenarios)
  
  private val step1 = Step(StepKeyword.Given, "I am step 1")
  private val step2 = Step(StepKeyword.Then, "I am not step 1")
  
  private val comment1 = "# I am single line hash comment"
  
  "Valid scenarios" should "parse" in {
      
      parse("Scenario:").get   should be (Scenario(List[Tag](), "", Nil, None, Nil))
      parse("Scenario:\n").get should be (Scenario(List[Tag](), "", Nil, None, Nil))
      
      parse(s"Scenario:name\n$step1").get   should be (Scenario(List[Tag](), "name", Nil, None, List(Step(step1, Position(3, 1)))))
      parse(s"Scenario: name\n$step1").get  should be (Scenario(List[Tag](), "name", Nil, None, List(Step(step1, Position(3, 1)))))
      
      parse(s"Scenario:name\nI am a test scenario\n$step1").get   should be (Scenario(List[Tag](), "name", List("I am a test scenario"), None, List(Step(step1, Position(4, 1)))))
      parse(s"Scenario: name\nI am another\nmultiline\n\nscenario\n$step1").get  should be (Scenario(List[Tag](), "name", List("I am another", "multiline", "", "scenario"), None, List(Step(step1, Position(7, 1)))))
      
      parse(s"\tScenario:name\n$step1").get     should be (Scenario(List[Tag](), "name", Nil, None, List(Step(step1, Position(3, 1)))))
      parse(s"Scenario:\tname\n$step1").get     should be (Scenario(List[Tag](), "name", Nil, None, List(Step(step1, Position(3, 1)))))
      parse(s"Scenario:\tname\t\n$step1").get   should be (Scenario(List[Tag](), "name", Nil, None, List(Step(step1, Position(3, 1)))))
      parse(s"Scenario:\tname \n$step1").get    should be (Scenario(List[Tag](), "name", Nil, None, List(Step(step1, Position(3, 1)))))
      parse(s"Scenario:\tname\t \n$step1").get  should be (Scenario(List[Tag](), "name", Nil, None, List(Step(step1, Position(3, 1)))))
      
      parse(s"Scenario: name\n$step1\n$step2").get should be (Scenario(List[Tag](), "name", Nil, None, List(Step(step1, Position(3, 1)), Step(step2, Position(4, 1)))))
      
      parse(s"Scenario: name\n$step1\n$comment1").get            should be (Scenario(List[Tag](), "name", Nil, None, List(Step(step1, Position(3, 1)))))
      parse(s"Scenario: name\n$step1\n$step2\n$comment1").get    should be (Scenario(List[Tag](), "name", Nil, None, List(Step(step1, Position(3, 1)), Step(step2, Position(4, 1)))))
      parse(s"Scenario: name\n$step1\n$comment1\n$step2").get    should be (Scenario(List[Tag](), "name", Nil, None, List(Step(step1, Position(3, 1)), Step(step2, Position(5, 1)))))
      parse(s"Scenario: name\n$comment1\n$step1\n$step2").get    should be (Scenario(List[Tag](), "name", Nil, None, List(Step(step1, Position(4, 1)), Step(step2, Position(5, 1)))))
      
      parse(s"Scenario:\n$step1\n$step2").get    should be (Scenario(List[Tag](), s"", Nil, None, List(Step(step1, Position(3, 1)), Step(step2, Position(4, 1)))))
      parse(s"Scenario: \n$step1\n$step2").get   should be (Scenario(List[Tag](), s"", Nil, None, List(Step(step1, Position(3, 1)), Step(step2, Position(4, 1)))))
      
      parse("Scenario: I dont have any steps").get should be (Scenario(List[Tag](), "I dont have any steps", Nil, None, Nil))
      
      StepKeyword.values foreach { keyword =>
        parse(s"Scenario: I contain a $keyword keyword in name\n$step1").get should be (Scenario(List[Tag](), s"I contain a $keyword keyword in name", Nil, None, List(Step(step1, Position(3, 1)))))
      }
  }
  
  "Invalid scenarios" should "not parse" in {

    assertFail(s"Scenario :name\n$step1")
    assertFail(s"Scenario\t:name\n$step1")
    assertFail(s"\tScenario\t:name\n$step1")
    assertFail(s"\tScenario\t:\tname\n$step1")
    
    assertFail("Scenario")
    assertFail("I am not a valid scenario")
    assertFail("Scenario no colon after 'Scenario:'")
     
  }

  "Valid scenario outlines" should "parse" in {

    val outline = """
    Scenario Outline: Join strings
    Given string 1 is "<string 1>"
      And string 2 is "<string 2>"
     When I join the two strings
     Then the result should be "<result>"
    Examples:
      | string 1 | string 2 | result   |
      | howdy    | doo      | howdydoo |
      | any      | thing    | anything |
    """

    val scenarios = parseScenarioOutline(outline).get.toList

    scenarios.size should be (2)

    val scenario1 = scenarios(0)
    scenario1.tags should be (List(Tag("""ScenarioOutline(example="| howdy | doo | howdydoo |")""")))
    scenario1.name should be ("Join strings")
    scenario1.background should be (None)
    scenario1.description should be (Nil)
    scenario1.steps(0) should be (Step(Step(StepKeyword.Given, """string 1 is "howdy""""), Position(4, 5)))
    scenario1.steps(1) should be (Step(Step(StepKeyword.And, """string 2 is "doo""""), Position(5, 7)))
    scenario1.steps(2) should be (Step(Step(StepKeyword.When, "I join the two strings"), Position(6, 6)))
    scenario1.steps(3) should be (Step(Step(StepKeyword.Then, """the result should be "howdydoo""""), Position(7, 6)))

    val scenario2 = scenarios(1)
    scenario2.tags should be (List(Tag("""ScenarioOutline(example="| any | thing | anything |")""")))
    scenario2.name should be ("Join strings")
    scenario2.background should be (None)
    scenario2.description should be (Nil)
    scenario2.steps(0) should be (Step(Step(StepKeyword.Given, """string 1 is "any""""), Position(4, 5)))
    scenario2.steps(1) should be (Step(Step(StepKeyword.And, """string 2 is "thing""""), Position(5, 7)))
    scenario2.steps(2) should be (Step(Step(StepKeyword.When, "I join the two strings"), Position(6, 6)))
    scenario2.steps(3) should be (Step(Step(StepKeyword.Then, """the result should be "anything""""), Position(7, 6)))

  }
  
  private def assertFail(input: String) {
    parse(input) match {
      case Success(_) => fail("failure expected") 
      case _ => 
    }
  }
  
}