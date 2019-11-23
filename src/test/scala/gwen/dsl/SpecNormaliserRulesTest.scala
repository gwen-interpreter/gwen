/*
 * Copyright 2019 Branko Juric, Brady Wood
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
import gwen.errors.AmbiguousCaseException
import gwen.eval.DataRecord

class SpecNormaliserRulesTest extends FlatSpec with Matchers with SpecNormaliser with GherkinParser {

  object Feature {
   def apply(name: String, description: List[String]): Feature = new Feature("en", Nil, name, description)
  }

  object Scenario {
    def apply(tags: List[Tag], name: String, description: List[String], background: Option[Background], steps: List[Step]): Scenario =
      new Scenario(tags.distinct, FeatureKeyword.Scenario.toString, name, description, background, steps, isOutline = false, Nil, None)
  }

  private val parse = parseFeatureSpec(_: String)

  val background = Background(
    "background",
    Nil,
    List(Step(StepKeyword.Given, "background step 1", Passed(2)))
  )

  "Feature with no background and no step defs" should "normalise without error" in {
    val feature = FeatureSpec(
    Feature("feature1", Nil),
      None,
      List(
      Scenario(List[Tag](), "scenario1", Nil, None, List(
        Step(StepKeyword.Given, "step 1", Passed(2)),
        Step(StepKeyword.When, "step 2", Passed(1)),
        Step(StepKeyword.Then, "step 3", Passed(2)))
      )),
      Nil)

    val result = normalise(feature, None, None)

    val scenario = result.scenarios(0)
    scenario.tags should be(Nil)
    scenario.name should be("scenario1")
    scenario.background should be(None)
    scenario.description should be(Nil)
    scenario.steps(0).toString should be("Given step 1")
    scenario.steps(1).toString should be("When step 2")
    scenario.steps(2).toString should be("Then step 3")
  }

  "Feature with background and no step defs" should "normalise without error" in {
    val feature = FeatureSpec(
      Feature("feature1", Nil),
      Some(background),
      List(
        Scenario(List[Tag](), "scenario1", Nil, None, List(
          Step(StepKeyword.Given, "step 1", Passed(2)),
          Step(StepKeyword.When, "step 2", Passed(1)),
          Step(StepKeyword.Then, "step 3", Passed(2)))
        )),
        Nil)

    val result = normalise(feature, None, None)

    result.background should be (None)

    val scenario = result.scenarios(0)

    scenario.tags should be(Nil)
    scenario.name should be("scenario1")
    scenario.background should be (Some(background))
    scenario.description should be(Nil)
    scenario.steps(0).toString should be("Given step 1")
    scenario.steps(1).toString should be("When step 2")
    scenario.steps(2).toString should be("Then step 3")
  }
  
  "StepDef without background and one step def" should "normalise without error" in {
    val meta = FeatureSpec(
    Feature("meta1", Nil), None, List(
      Scenario(List[Tag]("@StepDef"), "stepdef1", Nil, None, List(
        Step(StepKeyword.Given, "step 1", Passed(2)),
        Step(StepKeyword.When, "step 2", Passed(1)),
        Step(StepKeyword.Then, "step 3", Passed(2)))
      )), Nil)

    val result = normalise(meta, None, None)

    val scenario = result.scenarios(0)
    scenario.tags should be(List(Tag("StepDef")))
    scenario.name should be("stepdef1")
    scenario.background should be(None)
    scenario.description should be(Nil)
    scenario.steps(0).toString should be("Given step 1")
    scenario.steps(1).toString should be("When step 2")
    scenario.steps(2).toString should be("Then step 3")
  }

  "StepDef with background and one step def" should "normalise without error" in {
    val meta = FeatureSpec(
      Feature("meta1", Nil), Some(background), List(
        Scenario(List[Tag]("@StepDef"), "stepdef1", Nil, None, List(
          Step(StepKeyword.Given, "step 1", Passed(2)),
          Step(StepKeyword.When, "step 2", Passed(1)),
          Step(StepKeyword.Then, "step 3", Passed(2)))
        )), Nil)

    val result = normalise(meta, None, None)
    result.background should be (None)

    val scenario = result.scenarios(0)
    scenario.tags should be(List(Tag("StepDef")))
    scenario.name should be("stepdef1")
    scenario.background should be(None)
    scenario.description should be(Nil)
    scenario.steps(0).toString should be("Given step 1")
    scenario.steps(1).toString should be("When step 2")
    scenario.steps(2).toString should be("Then step 3")
  }
  
  "Meta with multiple unique step defs" should "normalise without error" in {
    val meta = FeatureSpec(
    Feature("meta1", Nil), None, List(
      Scenario(List[Tag]("@StepDef"), "stepdef1", Nil, None, List(
        Step(StepKeyword.Given, "step 1", Passed(2)),
        Step(StepKeyword.When, "step 2", Passed(1)),
        Step(StepKeyword.Then, "step 3", Passed(2)))
      ),
      Scenario(List[Tag]("@StepDef"), "stepdef2", Nil, None, List(
        Step(StepKeyword.Given, "step 1", Passed(2)),
        Step(StepKeyword.When, "step 2", Passed(1)),
        Step(StepKeyword.Then, "step 3", Passed(2)))
      )), Nil)
  normalise(meta, None, None)
  }
  
  "Meta with duplicate step def" should "error" in {
    val meta = FeatureSpec(
    Feature("meta1", Nil), None, List(
      Scenario(List[Tag]("@StepDef"), "stepdef1", Nil, None, List(
        Step(StepKeyword.Given, "step 1", Passed(2)),
        Step(StepKeyword.When, "step 2", Passed(1)),
        Step(StepKeyword.Then, "step 3", Passed(2)))
      ),
      Scenario(List[Tag]("@StepDef"), "stepdef1", Nil, None, List(
        Step(StepKeyword.Given, "step 1", Passed(2)),
        Step(StepKeyword.When, "step 2", Passed(1)),
        Step(StepKeyword.Then, "step 3", Passed(2)))
      )), Nil)
      
  intercept[AmbiguousCaseException] {
    normalise(meta, None, None)
    }
  }
  
  "Meta with duplicate step def with params" should "error" in {
    val meta = FeatureSpec(
    Feature("meta1", Nil), None, List(
      Scenario(List[Tag]("@StepDef"), "stepdef <number>", Nil, None, List(
        Step(StepKeyword.Given, "step 1", Passed(2)),
        Step(StepKeyword.When, "step 2", Passed(1)),
        Step(StepKeyword.Then, "step 3", Passed(2)))
      ),
      Scenario(List[Tag]("@StepDef"), "stepdef <index>", Nil, None, List(
        Step(StepKeyword.Given, "step 1", Passed(2)),
        Step(StepKeyword.When, "step 2", Passed(1)),
        Step(StepKeyword.Then, "step 3", Passed(2)))
      )), Nil)
      
    intercept[AmbiguousCaseException] {
      normalise(meta, None, None)
    }
  }
  
  "Data driven feature with csv file" should "normalise without error" in {
    val feature = FeatureSpec(
    Feature("About me", Nil), Some(background), List(
      Scenario(List[Tag](), "What am I?", Nil, None, List(
        Step(StepKeyword.Given, "I am ${my age} year(s) old"),
        Step(StepKeyword.When, "I am a ${my gender}"),
        Step(StepKeyword.Then, "I am a ${my age} year old ${my title}"))
      )), Nil)
    val data = List(("my age", "18"), ("my gender", "male"), ("my title", "Mr"))
    val dataRecord = new DataRecord("AboutMe.csv", 1, data)
    val result = normalise(feature, None, Some(dataRecord))
    result.background should be (None)
    result.feature.name should be ("About me, [1] my age=18..")
    result.scenarios.length should be (1)
    result.scenarios(0).background.get.name should be (s"${background.name} (plus input data)")
    result.scenarios(0).background.get.description should be (List("""@Data(file="AboutMe.csv", record=1)"""))
    result.scenarios(0).background.get.steps.size should be (4)
    result.scenarios(0).background.get.steps(0).toString should be ("""Given my age is "18"""")
    result.scenarios(0).background.get.steps(1).toString should be ("""And my gender is "male"""")
    result.scenarios(0).background.get.steps(2).toString should be ("""And my title is "Mr"""")
    result.scenarios(0).background.get.steps(3).toString should be ("""Given background step 1""")
    result.scenarios(0).name should be ("What am I?")
    result.scenarios(0).description should be (Nil)
    result.scenarios(0).steps(0).toString should be ("""Given I am ${my age} year(s) old""")
    result.scenarios(0).steps(1).toString should be ("""When I am a ${my gender}""")
    result.scenarios(0).steps(2).toString should be ("""Then I am a ${my age} year old ${my title}""")
  }

  "Valid scenario outline" should "normalise" in {

    val featureString = """
    Feature: Outline

    Background: background
       Given background step 1

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

    val feature = parse(featureString).get

    val result = normalise(feature, None, None)
    result.feature.pos should be (Position(2, 5))

    result.background should be (None)

    val outline = result.scenarios(0)

    outline.pos should be (Position(8, 5))
    outline.tags should be(List(Tag("UnitTest")))
    outline.tags(0).pos should be (Position(7, 5))
    outline.name should be("Joining <string 1> and <string 2> should yield <result>")
    outline.background should be(None)
    outline.description should be(List("Substituting..", "string 1 = <string 1>", "string 2 = <string 2>", "result = <result>"))
    outline.steps(0) should be(Step(Step(StepKeyword.Given, """string 1 is "<string 1>""""), Position(11, 5)))
    outline.steps(1) should be(Step(Step(StepKeyword.And, """string 2 is "<string 2>""""), Position(12, 7)))
    outline.steps(2) should be(Step(Step(StepKeyword.When, "I join the two strings"), Position(13, 6)))
    outline.steps(3) should be(Step(Step(StepKeyword.Then, """the result should be "<result>""""), Position(14, 6)))

    val examples = outline.examples
    examples.size should be(3)

    val example1 = examples(0)
    example1.pos should be (Position(20, 5))
    example1.name should be("Compound words")
    example1.description should be(Nil)
    example1.table.size should be(3)
    example1.table(0) should be((22, List("string 1", "string 2", "result")))
    example1.table(1) should be((23, List("basket", "ball", "basketball")))
    example1.table(2) should be((24, List("any", "thing", "anything")))
    example1.scenarios.size should be(2)

    val scenario1 = example1.scenarios(0)
    scenario1.pos should be (Position(8, 5))
    scenario1.tags should be(List(Tag("UnitTest")))
    scenario1.tags(0).pos should be (Position(7, 5))
    scenario1.name should be("Joining basket and ball should yield basketball -- Example 1.1 Compound words")
    scenario1.background.get.pos should be (Position(4, 5))
    scenario1.background.get.name should be ("background")
    scenario1.background.get.steps(0) should be(Step(Step(StepKeyword.Given, "background step 1"), Position(5, 8)))
    scenario1.description should be(List("Substituting..", "string 1 = basket", "string 2 = ball", "result = basketball"))
    scenario1.steps(0) should be(Step(Step(StepKeyword.Given, """string 1 is "basket""""), Position(15, 5)))
    scenario1.steps(1) should be(Step(Step(StepKeyword.And, """string 2 is "ball""""), Position(16, 7)))
    scenario1.steps(2) should be(Step(Step(StepKeyword.When, "I join the two strings"), Position(17, 6)))
    scenario1.steps(3) should be(Step(Step(StepKeyword.Then, """the result should be "basketball""""), Position(18, 6)))

    val scenario2 = example1.scenarios(1)
    scenario2.pos should be (Position(8, 5))
    scenario2.tags should be(List(Tag("UnitTest")))
    scenario2.tags(0).pos should be (Position(7, 5))
    scenario2.name should be("Joining any and thing should yield anything -- Example 1.2 Compound words")
    scenario2.background.get.pos should be (Position(4, 5))
    scenario2.background.get.name should be ("background")
    scenario2.background.get.steps(0) should be(Step(Step(StepKeyword.Given, "background step 1"), Position(5, 8)))
    scenario2.description should be(List("Substituting..", "string 1 = any", "string 2 = thing", "result = anything"))
    scenario2.steps(0) should be(Step(Step(StepKeyword.Given, """string 1 is "any""""), Position(15, 5)))
    scenario2.steps(1) should be(Step(Step(StepKeyword.And, """string 2 is "thing""""), Position(16, 7)))
    scenario2.steps(2) should be(Step(Step(StepKeyword.When, "I join the two strings"), Position(17, 6)))
    scenario2.steps(3) should be(Step(Step(StepKeyword.Then, """the result should be "anything""""), Position(18, 6)))

    val example2 = examples(1)
    example2.pos should be (Position(26, 5))
    example2.name should be("Nonsensical compound words")
    example2.description.size should be(2)
    example2.description(0) should be("Words that don't make any sense at all")
    example2.description(1) should be("(for testing multiple examples)")
    example2.table.size should be(3)
    example2.table(0) should be((31, List("string 1", "string 2", "result")))
    example2.table(1) should be((32, List("howdy", "doo", "howdydoo")))
    example2.table(2) should be((33, List("yep", "ok", "yepok")))
    example2.scenarios.size should be(2)

    val scenario3 = example2.scenarios(0)
    scenario3.pos should be (Position(8, 5))
    scenario3.tags should be(List(Tag("UnitTest")))
    scenario3.tags(0).pos should be (Position(7, 5))
    scenario3.name should be("Joining howdy and doo should yield howdydoo -- Example 2.1 Nonsensical compound words")
    scenario3.background.get.pos should be (Position(4, 5))
    scenario3.background.get.name should be ("background")
    scenario3.background.get.steps(0) should be(Step(Step(StepKeyword.Given, "background step 1"), Position(5, 8)))
    scenario3.description should be(List("Substituting..", "string 1 = howdy", "string 2 = doo", "result = howdydoo"))
    scenario3.steps(0) should be(Step(Step(StepKeyword.Given, """string 1 is "howdy""""), Position(15, 5)))
    scenario3.steps(1) should be(Step(Step(StepKeyword.And, """string 2 is "doo""""), Position(16, 7)))
    scenario3.steps(2) should be(Step(Step(StepKeyword.When, "I join the two strings"), Position(17, 6)))
    scenario3.steps(3) should be(Step(Step(StepKeyword.Then, """the result should be "howdydoo""""), Position(18, 6)))

    val scenario4 = example2.scenarios(1)
    scenario4.pos should be (Position(8, 5))
    scenario4.tags should be(List(Tag("UnitTest")))
    scenario4.tags(0).pos should be (Position(7, 5))
    scenario4.name should be("Joining yep and ok should yield yepok -- Example 2.2 Nonsensical compound words")
    scenario4.background.get.pos should be (Position(4, 5))
    scenario4.background.get.name should be ("background")
    scenario4.background.get.steps(0) should be(Step(Step(StepKeyword.Given, "background step 1"), Position(5, 8)))
    scenario4.description should be(List("Substituting..", "string 1 = yep", "string 2 = ok", "result = yepok"))
    scenario4.steps(0) should be(Step(Step(StepKeyword.Given, """string 1 is "yep""""), Position(15, 5)))
    scenario4.steps(1) should be(Step(Step(StepKeyword.And, """string 2 is "ok""""), Position(16, 7)))
    scenario4.steps(2) should be(Step(Step(StepKeyword.When, "I join the two strings"), Position(17, 6)))
    scenario4.steps(3) should be(Step(Step(StepKeyword.Then, """the result should be "yepok""""), Position(18, 6)))

    val example3 = examples(2)
    example3.pos should be (Position(35, 5))
    example3.name should be("")
    example3.description should be(Nil)
    example3.table.size should be(2)
    example3.table(0) should be((37, List("string 1", "string 2", "result")))
    example3.table(1) should be((38, List("ding", "dong", "dingdong")))
    example3.scenarios.size should be(1)

    val scenario5 = example3.scenarios(0)
    scenario5.pos should be (Position(8, 5))
    scenario5.tags should be(List(Tag("UnitTest")))
    scenario5.tags(0).pos should be (Position(7, 5))
    scenario5.name should be("Joining ding and dong should yield dingdong -- Example 3.1 ")
    scenario5.background.get.pos should be (Position(4, 5))
    scenario5.background.get.name should be ("background")
    scenario5.background.get.steps(0) should be(Step(Step(StepKeyword.Given, "background step 1"), Position(5, 8)))
    scenario5.description should be(List("Substituting..", "string 1 = ding", "string 2 = dong", "result = dingdong"))
    scenario5.steps(0) should be(Step(Step(StepKeyword.Given, """string 1 is "ding""""), Position(15, 5)))
    scenario5.steps(1) should be(Step(Step(StepKeyword.And, """string 2 is "dong""""), Position(16, 7)))
    scenario5.steps(2) should be(Step(Step(StepKeyword.When, "I join the two strings"), Position(17, 6)))
    scenario5.steps(3) should be(Step(Step(StepKeyword.Then, """the result should be "dingdong""""), Position(18, 6)))

    val scenarios = outline.examples.flatMap(_.scenarios)
    scenarios.size should be(5)
    scenarios(0) should be(scenario1)
    scenarios(1) should be(scenario2)
    scenarios(2) should be(scenario3)
    scenarios(3) should be(scenario4)
    scenarios(4) should be(scenario5)
  }
  
}