/*
 * Copyright 2014-2020 Branko Juric, Brady Wood
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

package gwen.model.gherkin

import gwen.TestModel
import gwen.model.Position
import gwen.model.StepKeyword
import gwen.model.Tag
import gwen.model.gherkin.GherkinParser

import scala.util.Success

import org.scalatest.Matchers
import org.scalatest.FlatSpec

class ScenarioTest extends FlatSpec with Matchers with GherkinParser with TestModel {

  private def parse(input: String, clearPos: Boolean = true) = { 
    val scenario = parseSpecification(s"Feature: ftest\n$input").filter(_.scenarios.nonEmpty).map(_.scenarios.head)
    if (clearPos) { 
      scenario.map { sc => 
        sc.copy(
          withSourceRef = None,
          withSteps = sc.steps map { s => 
            s.copy(withSourceRef =  None)
          })
      }
    }
    else scenario
  }
  
  private val step1 = Step(StepKeyword.Given.toString, "I am step 1")
  private val step2 = Step(StepKeyword.Then.toString, "I am not step 1")
  
  private val comment1 = "# I am single line hash comment"
  
  "Valid scenarios" should "parse" in {
    
      parse("Scenario:").get   should be (Scenario(List[Tag](), "", Nil, None, Nil))
      parse("Scenario:\n").get should be (Scenario(List[Tag](), "", Nil, None, Nil))
      
      parse(s"Scenario:name\n$step1").get   should be (Scenario(List[Tag](), "name", Nil, None, List(step1)))
      parse(s"Scenario: name\n$step1").get  should be (Scenario(List[Tag](), "name", Nil, None, List(step1)))
      
      parse(s"Scenario:name\nI am a test scenario\n$step1").get   should be (Scenario(List[Tag](), "name", List("I am a test scenario"), None, List(step1)))
      parse(s"Scenario: name\nI am another\nmultiline\n\nscenario\n$step1").get  should be (Scenario(List[Tag](), "name", List("I am another", "multiline", "", "scenario"), None, List(step1)))
      
      parse(s"\tScenario:name\n$step1").get     should be (Scenario(List[Tag](), "name", Nil, None, List(step1)))
      parse(s"Scenario:\tname\n$step1").get     should be (Scenario(List[Tag](), "name", Nil, None, List(step1)))
      parse(s"Scenario:\tname\t\n$step1").get   should be (Scenario(List[Tag](), "name", Nil, None, List(step1)))
      parse(s"Scenario:\tname \n$step1").get    should be (Scenario(List[Tag](), "name", Nil, None, List(step1)))
      parse(s"Scenario:\tname\t \n$step1").get  should be (Scenario(List[Tag](), "name", Nil, None, List(step1)))
      
      parse(s"Scenario: name\n$step1\n$step2").get should be (Scenario(List[Tag](), "name", Nil, None, List(step1, step2)))
      
      parse(s"Scenario: name\n$step1\n$comment1").get            should be (Scenario(List[Tag](), "name", Nil, None, List(step1)))
      parse(s"Scenario: name\n$step1\n$step2\n$comment1").get    should be (Scenario(List[Tag](), "name", Nil, None, List(step1, step2)))
      parse(s"Scenario: name\n$step1\n$comment1\n$step2").get    should be (Scenario(List[Tag](), "name", Nil, None, List(step1, step2)))
      parse(s"Scenario: name\n$comment1\n$step1\n$step2").get    should be (Scenario(List[Tag](), "name", Nil, None, List(step1, step2)))
      
      parse(s"Scenario:\n$step1\n$step2").get    should be (Scenario(List[Tag](), s"", Nil, None, List(step1, step2)))
      parse(s"Scenario: \n$step1\n$step2").get   should be (Scenario(List[Tag](), s"", Nil, None, List(step1, step2)))
      
      parse("Scenario: I dont have any steps").get should be (Scenario(List[Tag](), "I dont have any steps", Nil, None, Nil))
      
      StepKeyword.values foreach { keyword =>
        parse(s"Scenario: I contain a $keyword keyword in name\n$step1").get should be (Scenario(List[Tag](), s"I contain a $keyword keyword in name", Nil, None, List(step1)))
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

    val feature = """
    @UnitTest
    Scenario Outline: Joining <string 1> and <string 2> should yield <result>

    Substituting..
    string 1 = <string 1>
    string 2 = <string 2>
    result = <result>

    Given string 1 is "<string 1>"
      And string 2 is "<string 2>"
     When I join the two strings
     Then the result should be "<result>"

    Examples: Compound words

      | string 1 | string 2 | result     |
      | basket   | ball     | basketball |
      | any      | thing    | anything   |

    Examples: Nonsensical compound words

      Words that don't make any sense at all
      (for testing multiple examples)

      | string 1 | string 2 | result   |
      | howdy    | doo      | howdydoo |
      | yep      | ok       | yepok    |

    Examples:

      | string 1 | string 2 | result   |
      | ding     | dong     | dingdong |
    """

    val outline = parse(feature, clearPos=false).get

    outline.sourceRef.get.pos should be (Position(4, 5))

    outline.tags.map(_.name) should be (List("UnitTest"))
    outline.tags(0).sourceRef.get.pos should be (Position(3, 5))
    outline.name should be ("Joining <string 1> and <string 2> should yield <result>")
    outline.background should be (None)
    outline.description should be (List("Substituting..", "string 1 = <string 1>", "string 2 = <string 2>", "result = <result>"))
    outline.steps(0) should be (Step(Position(11, 5), StepKeyword.Given.toString, """string 1 is "<string 1>""""))
    outline.steps(1) should be (Step(Position(12, 7), StepKeyword.And.toString, """string 2 is "<string 2>""""))
    outline.steps(2) should be (Step(Position(13, 6), StepKeyword.When.toString, "I join the two strings"))
    outline.steps(3) should be (Step(Position(14, 6), StepKeyword.Then.toString, """the result should be "<result>""""))

    val examples = outline.examples
    examples.size should be (3)

    val example1 = examples(0)
    example1.sourceRef.get.pos should be (Position(16, 5))
    example1.name should be ("Compound words")
    example1.description should be (Nil)
    example1.table.size should be (3)
    example1.table(0) should be ((18, List("string 1", "string 2", "result")))
    example1.table(1) should be ((19, List("basket", "ball", "basketball")))
    example1.table(2) should be ((20, List("any", "thing", "anything")))
    example1.scenarios.size should be (0)

    val example2 = examples(1)
    example2.sourceRef.get.pos should be (Position(22, 5))
    example2.name should be ("Nonsensical compound words")
    example2.description.size should be (2)
    example2.description(0) should be ("Words that don't make any sense at all")
    example2.description(1) should be ("(for testing multiple examples)")
    example2.table.size should be (3)
    example2.table(0) should be ((27, List("string 1", "string 2", "result")))
    example2.table(1) should be ((28, List("howdy", "doo", "howdydoo")))
    example2.table(2) should be ((29, List("yep", "ok", "yepok")))
    example2.scenarios.size should be (0)

    val example3 = examples(2)
    example3.sourceRef.get.pos should be (Position(31, 5))
    example3.name should be ("")
    example3.description should be (Nil)
    example3.table.size should be (2)
    example3.table(0) should be ((33, List("string 1", "string 2", "result")))
    example3.table(1) should be ((34, List("ding", "dong", "dingdong")))
    example3.scenarios.size should be (0)

  }
  
  private def assertFail(input: String): Unit = {
    parse(input) match {
      case Success(_) => fail("failure expected") 
      case _ => 
    }
  }
  
}