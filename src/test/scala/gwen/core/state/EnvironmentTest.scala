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

package gwen.core.state

import gwen.core.BaseTest
import gwen.core.Errors._
import gwen.core.TestModel
import gwen.core.node.gherkin.StepKeyword
import gwen.core.node.gherkin.Tag

import org.scalatest.prop.TableDrivenPropertyChecks.forAll
import org.scalatest.matchers.should.Matchers

class EnvironmentTest extends BaseTest with Matchers with TestModel {
  
  "New env context" should "contain no StepDefs" in {
    val env = newEnv(StateLevel.feature)
    env.getStepDef("google it", None) should be (None)
  }
  
  "New StepDef added to env context" should "be accessible" in {
    
    val steps = List(
      Step(StepKeyword.Given.toString, """I enter "gwen" in the search field"""),
      Step(StepKeyword.And.toString, """I submit the search field""")
    )
    
    val stepdef = Scenario(List(Tag("@StepDef")), """I search for "gwen"""", Nil, None, steps)
    
    val env = newEnv(StateLevel.feature)
    env.addStepDef(stepdef)
    env.getStepDef("""I search for "gwen"""", None) should be (Some(stepdef))
    env.getStepDef("I am not defined", None) should be (None)
    
  }
  
  "New StepDef added to env context" should "not be accessible after feature level reset" in {
    
    val steps = List(
      Step(StepKeyword.Given.toString, """I enter "gwen" in the search field"""),
      Step(StepKeyword.And.toString, """I submit the search field""")
    )
    
    val stepdef = Scenario(List(Tag("@StepDef")), """I search for "gwen"""", Nil, None, steps)
    
    val env = newEnv(StateLevel.feature)
    env.addStepDef(stepdef) 
    env.getStepDef("""I search for "gwen"""", None) should be (Some(stepdef))
    env.reset(StateLevel.feature)
    env.getStepDef("""I search for "gwen"""", None) should be (None)
    
  }

  "Feature impolicits and New StepDef added to env context" should "still be accessible after scenario level reset" in {
    
    val steps = List(
      Step(StepKeyword.Given.toString, """I enter "gwen" in the search field"""),
      Step(StepKeyword.And.toString, """I submit the search field""")
    )
    
    val stepdef = Scenario(List(Tag("@StepDef")), """I search for "gwen"""", Nil, None, steps)

    val env = newEnv(StateLevel.feature)
    env.topScope.set("engineName", "Gwen-Core")
    env.featureScope.set("gwen.feature.file.name", "file.feature")
    env.featureScope.set("gwen.feature.file.simpleName", "file")
    env.featureScope.set("gwen.feature.file.path", "path/file.feature")
    env.featureScope.set("gwen.feature.file.absolutePath", "/absolute/path/file.feature")
    env.featureScope.set("gwen.feature.name", "feature")
    env.ruleScope.set("gwen.rule.name", "rule")
    env.addStepDef(stepdef)
    env.getStepDef("""I search for "gwen"""", None) should be (Some(stepdef))
    env.reset(StateLevel.scenario)
    env.topScope.getOpt("engineName") should be (None)
    env.getStepDef("""I search for "gwen"""", None) should be (Some(stepdef))
    env.topScope.get("gwen.feature.file.name") should be ("file.feature")
    env.topScope.get("gwen.feature.file.simpleName") should be ("file")
    env.topScope.get("gwen.feature.file.path") should be ("path/file.feature")
    env.topScope.get("gwen.feature.file.absolutePath") should be ("/absolute/path/file.feature")
    env.topScope.get("gwen.feature.name") should be ("feature")
    env.topScope.get("gwen.rule.name") should be ("rule")
    
  }
  
  "StepDef with params" should "resolve" in {
    
    val stepdef1 = Scenario(List(Tag("@StepDef")), """I enter "<searchTerm>" in the search field""", Nil, None, Nil)
    val stepdef2 = Scenario(List(Tag("@StepDef")), """I enter "<search term>" in the search field again""", Nil, None, Nil)
    val stepdef3 = Scenario(List(Tag("@StepDef")), "z = <x> + 1", Nil, None, Nil)
    val stepdef4 = Scenario(List(Tag("@StepDef")), "z = 1 + <x>", Nil, None, Nil)
    val stepdef5 = Scenario(List(Tag("@StepDef")), "z = <x> - <y>", Nil, None, Nil)
    
    val env = newEnv(StateLevel.feature)

    env.addStepDef(stepdef1)
    env.addStepDef(stepdef2)
    env.addStepDef(stepdef3)
    env.addStepDef(stepdef4)
    env.addStepDef(stepdef5)
    
    env.getStepDef("""I enter "gwen" in the search field""", None) should be (Some(stepdef1.copy(withParams = List(("searchTerm", "gwen")))))
    env.getStepDef("""I enter "gwen" in the search field again""", None) should be (Some(stepdef2.copy(withParams = List(("search term", "gwen")))))
    env.getStepDef("z = 2 + 1", None) should be (Some(stepdef3.copy(withParams = List(("x", "2")))))
    env.getStepDef("z = 1 + 3", None) should be (Some(stepdef4.copy(withParams = List(("x", "3")))))
    env.getStepDef("z = 2 - 2", None) should be (Some(stepdef5.copy(withParams = List(("x", "2"), ("y", "2")))))

    val stepdef6 = Scenario(List(Tag("@StepDef")), "z = <x> * <x>", Nil, None, Nil)
    env.addStepDef(stepdef6)
    intercept[AmbiguousCaseException] {
      env.getStepDef("z = 3 * 4", None)
    }
    
  }

  "StepDef with docString param" should "resolve" in {
    
    val stepdef = Scenario(List(Tag("@StepDef")), """I do a search for <searchTerm>""", Nil, None, Nil)
    
    val env = newEnv(StateLevel.feature)
    env.addStepDef(stepdef)
    
    val docString1 = "DocString"
    val step1 = Step("I do a search for", Some(docString1))
    val result1 = env.getStepDef(step1.expression, step1.docString.map(_._2)).get
    result1.name should be (stepdef.name)
    result1.params should be (List(("searchTerm", docString1)))

    val docString2 = 
      """|Multi line
         |DocString""".stripMargin
    val step2 = Step("I do a search for", Some(docString2))
    val result2 = env.getStepDef(step2.expression, step2.docString.map(_._2)).get
    result2.name should be (stepdef.name)
    result2.params should be (List(("searchTerm", docString2)))
    
  }
  
  "Sample math StepDefs with parameters" should "resolve" in {
    
    val stepdef1 = Scenario(List(Tag("@StepDef")), "++x", Nil, None, Nil)
    val stepdef2 = Scenario(List(Tag("@StepDef")), "c = a + <b>", Nil, None, Nil)
    val stepdef3 = Scenario(List(Tag("@StepDef")), "z = <x> + <y>", Nil, None, Nil)
    
    val env = newEnv(StateLevel.feature)

    env.addStepDef(stepdef1)
    env.addStepDef(stepdef2)
    env.addStepDef(stepdef3)
    
    env.getStepDef("z = 2 + 3", None) should be (Some(stepdef3.copy(withParams = List(("x", "2"), ("y", "3")))))
    env.getStepDef("z = 2 + 2", None) should be (Some(stepdef3.copy(withParams = List(("x", "2"), ("y", "2")))))
    env.getStepDef("z = 3 + 2", None) should be (Some(stepdef3.copy(withParams = List(("x", "3"), ("y", "2")))))
    env.getStepDef("z = 2 + 3", None) should be (Some(stepdef3.copy(withParams = List(("x", "2"), ("y", "3")))))
    env.getStepDef("z = 5 + y", None) should be (Some(stepdef3.copy(withParams = List(("x", "5"), ("y", "y")))))
    env.getStepDef("c = a + y", None) should be (Some(stepdef2.copy(withParams = List(("b", "y")))))
    
  }
  
  "Ambiguous math StepDefs with parameters" should "be detected" in {
    
    val stepdef1 = Scenario(List(Tag("@StepDef")), "z = a + <b>", Nil, None, Nil)
    val stepdef2 = Scenario(List(Tag("@StepDef")), "z = <x> + <y>", Nil, None, Nil)
    
    val env = newEnv(StateLevel.feature)

    env.addStepDef(stepdef1)
    env.addStepDef(stepdef2)
    
    intercept[AmbiguousCaseException] {
      env.getStepDef("z = a + 3", None)
    }
    
  }
  
  forAll (levels) { level =>
    "New feature env context" should s"have $level scope" in {
      val env = newEnv(StateLevel.valueOf(level))
      env.topScope.scope should be (level)
      env.topScope.isTopScope should be (true)
    }
  }
  
  forAll (levels) { level =>
    s"Bound $level scope attribute" should "be removed after reset" in {
      val env = newEnv(StateLevel.valueOf(level))
      env.topScope.set("engineName", "Gwen-Core")
      env.topScope.get("engineName") should be ("Gwen-Core")
      env.reset(StateLevel.valueOf(level))
      env.topScope.getOpt("engineName") should be (None)
      env.topScope.scope should be (level)
    }
  }

  "Implicit top scope attribute" should "be removed after feature level reset" in {
    val env = newEnv(StateLevel.feature)
    env.topScope.set("engineName", "Gwen-Core")
    env.topScope.get("engineName") should be ("Gwen-Core")
    env.topScope.set("gwen.feature.file.name", "file.feature")
    env.topScope.set("gwen.feature.file.simpleName", "file")
    env.topScope.set("gwen.feature.file.path", "path/file.feature")
    env.topScope.set("gwen.feature.file.absolutePath", "/absolute/path/file.feature")
    env.topScope.set("gwen.feature.name", "feature")
    env.topScope.set("gwen.rule.name", "rule")
    env.reset(StateLevel.feature)
    env.topScope.getOpt("engineName") should be (None)
    env.topScope.getOpt("gwen.feature.file.name") should be (None)
    env.topScope.getOpt("gwen.feature.file.simpleName") should be (None)
    env.topScope.getOpt("gwen.feature.file.path") should be(None)
    env.topScope.getOpt("gwen.feature.file.absolutePath") should be (None)
    env.topScope.getOpt("gwen.feature.name") should be (None)
    env.topScope.getOpt("gwen.rule.name") should be (None)
  }

  forAll (levels) { level =>
    s"Bound $level level scope attribute" should "show up in asString" in {
      val env = newEnv(StateLevel.valueOf(level))
      env.topScope.set("firstName", "Gwen")
      env.topScope.get("firstName") should be ("Gwen")
      env.asString(all = true, env = true) should be (
        s"""|env : "implicits" {
            |  scope : "feature" { }
            |  scope : "rule" { }
            |  scope : "examples" { }
            |  scope : "scenario" { }
            |  scope : "stepDef" { }
            |  scope : "params" { }
            |  scope : "iteration" { }
            |}
            |env {
            |  scope : "$level" {
            |    firstName : "Gwen"
            |  }
            |}""".stripMargin)
      env.asString(all = true, env = false) should be (
        s"""|scope : "feature" { }
            |scope : "rule" { }
            |scope : "examples" { }
            |scope : "scenario" { }
            |scope : "stepDef" { }
            |scope : "params" { }
            |scope : "iteration" { }
            |scope : "$level" {
            |  firstName : "Gwen"
            |}""".stripMargin)
      env.asString(all = false, env = true) should be (
        s"""|env {
            |  scope : "$level" {
            |    firstName : "Gwen"
            |  }
            |}""".stripMargin)
      env.asString(all = false, env = false) should be (
        s"""|scope : "$level" {
            |  firstName : "Gwen"
            |}""".stripMargin)
    }
  }
  
  "env.asString on new env context" should "contain empty scopes" in {
    val env = newEnv(StateLevel.feature)
    env.asString(all = true, env = true) should be (
      s"""|env : "implicits" {
          |  scope : "feature" { }
          |  scope : "rule" { }
          |  scope : "examples" { }
          |  scope : "scenario" { }
          |  scope : "stepDef" { }
          |  scope : "params" { }
          |  scope : "iteration" { }
          |}
          |env {
          |  scope : "feature" { }
          |}""".stripMargin
    )
  }
  
  "StepDef names" should "not start with a keyword" in {
    val env = newEnv(StateLevel.feature)
    StepKeyword.names foreach { keyword =>
      val stepdef = Scenario(List(Tag("@StepDef")), s"""$keyword I search for "gwen"""", Nil, None, Nil)
      intercept[InvalidStepDefException] {
        env.addStepDef(stepdef)
      }
    }
  }

  "Getting bound objects from cache" should "get those objects" in {
    val env = newEnv(StateLevel.feature)
    env.topScope.pushObject("greeting", "howdy")
    env.topScope.pushObject("gwen", "interpreter")
    env.topScope.getObject("greeting") should be (Some("howdy"))
    env.topScope.getObject("gwen") should be (Some("interpreter"))
  }

  "Popping bound object from cache" should "clear that object" in {
    val env = newEnv(StateLevel.feature)
    env.topScope.pushObject("greeting", "howdy")
    env.topScope.pushObject("gwen", "interpreter")
    env.topScope.popObject("greeting") should be (Some("howdy"))
    env.topScope.getObject("greeting") should be (None)
    env.topScope.getObject("gwen") should be (Some("interpreter"))
  }

  "Resetting context" should "should clear all objects from cache" in {
    val env = newEnv(StateLevel.feature)
    env.topScope.pushObject("greeting", "howdy")
    env.topScope.pushObject("gwen", "interpreter")
    env.reset(StateLevel.feature)
    env.topScope.getObject("greeting") should be (None)
    env.topScope.getObject("gwen") should be (None)
  }

  "Managing bound and shadowed objects from cache" should "work as expected" in {
    val env = newEnv(StateLevel.feature)
    env.topScope.pushObject("greeting", "howdy")
    env.topScope.pushObject("gwen", "interpreter 1")
    env.topScope.pushObject("gwen", "interpreter 2")

    env.topScope.getObject("greeting") should be (Some("howdy"))
    env.topScope.getObject("gwen") should be (Some("interpreter 2"))

    env.topScope.popObject("gwen") should be (Some("interpreter 2"))
    env.topScope.getObject("greeting") should be (Some("howdy"))
    env.topScope.getObject("gwen") should be (Some("interpreter 1"))

    env.topScope.popObject("gwen") should be (Some("interpreter 1"))
    env.topScope.getObject("greeting") should be (Some("howdy"))
    env.topScope.getObject("gwen") should be (None)

    env.reset(StateLevel.feature)
    env.topScope.getObject("greeting") should be (None)
    env.topScope.getObject("gwen") should be (None)

  }

  "Issue #40: Stepdef with empty parameters" should "resolve" in {

    val stepdef1 = Scenario(List(Tag("@StepDef")), """I type name "<name>"""", Nil, None, Nil)
    val stepdef2 = Scenario(List(Tag("@StepDef")), """I enter name "<name>", age "<age>"""", Nil, None, Nil)
    val stepdef3 = Scenario(List(Tag("@StepDef")), """I provide name "<name>", age "<age>" and gender "<gender>"""", Nil, None, Nil)
    val stepdef4 = Scenario(List(Tag("@StepDef")), """I give name "<name>", age "<age>", gender "<gender>" and title "<title>"""", Nil, None, Nil)
    val stepdef5 = Scenario(List(Tag("@StepDef")), """I test "<param1>" "<param2>"""", Nil, None, Nil)

    val env = newEnv(StateLevel.feature)
    env.addStepDef(stepdef1)
    env.addStepDef(stepdef2)
    env.addStepDef(stepdef3)
    env.addStepDef(stepdef4)
    env.addStepDef(stepdef5)

    env.getStepDef("""I type name """"", None) should be (
      Some(stepdef1.copy(withParams = List(("name", ""))))
    )

    env.getStepDef("""I enter name "", age """"", None) should be (
      Some(stepdef2.copy(withParams = List(("name", ""), ("age", ""))))
    )
    env.getStepDef("""I enter name "gwen", age """"", None) should be (
      Some(stepdef2.copy(withParams = List(("name", "gwen"), ("age", ""))))
    )
    env.getStepDef("""I enter name "", age "24"""", None) should be (
      Some(stepdef2.copy(withParams = List(("name", ""), ("age", "24"))))
    )

    env.getStepDef("""I provide name "", age "" and gender """"", None) should be (
      Some(stepdef3.copy(withParams = List(("name", ""), ("age", ""), ("gender", ""))))
    )
    env.getStepDef("""I provide name "gwen", age "" and gender """"", None) should be (
      Some(stepdef3.copy(withParams = List(("name", "gwen"), ("age", ""), ("gender", ""))))
    )
    env.getStepDef("""I provide name "", age "24" and gender """"", None) should be (
      Some(stepdef3.copy(withParams = List(("name", ""), ("age", "24"), ("gender", ""))))
    )
    env.getStepDef("""I provide name "", age "" and gender "female"""", None) should be (
      Some(stepdef3.copy(withParams = List(("name", ""), ("age", ""), ("gender", "female"))))
    )
    env.getStepDef("""I provide name "gwen", age "24" and gender """"", None) should be (
      Some(stepdef3.copy(withParams = List(("name", "gwen"), ("age", "24"), ("gender", ""))))
    )
    env.getStepDef("""I provide name "", age "24" and gender "female"""", None) should be (
      Some(stepdef3.copy(withParams = List(("name", ""), ("age", "24"), ("gender", "female"))))
    )
    env.getStepDef("""I provide name "gwen", age "" and gender "female"""", None) should be (
      Some(stepdef3.copy(withParams = List(("name", "gwen"), ("age", ""), ("gender", "female"))))
    )

    env.getStepDef("""I give name "", age "", gender "" and title """"", None) should be (
      Some(stepdef4.copy(withParams = List(("name", ""), ("age", ""), ("gender", ""), ("title", ""))))
    )
    env.getStepDef("""I give name "gwen", age "", gender "" and title """"", None) should be (
      Some(stepdef4.copy(withParams = List(("name", "gwen"), ("age", ""), ("gender", ""), ("title", ""))))
    )
    env.getStepDef("""I give name "", age "24", gender "" and title """"", None) should be (
      Some(stepdef4.copy(withParams = List(("name", ""), ("age", "24"), ("gender", ""), ("title", ""))))
    )
    env.getStepDef("""I give name "", age "", gender "female" and title """"", None) should be (
      Some(stepdef4.copy(withParams = List(("name", ""), ("age", ""), ("gender", "female"), ("title", ""))))
    )
    env.getStepDef("""I give name "", age "", gender "" and title "miss"""", None) should be (
      Some(stepdef4.copy(withParams = List(("name", ""), ("age", ""), ("gender", ""), ("title", "miss"))))
    )
    env.getStepDef("""I give name "gwen", age "24", gender "" and title """"", None) should be (
      Some(stepdef4.copy(withParams = List(("name", "gwen"), ("age", "24"), ("gender", ""), ("title", ""))))
    )
    env.getStepDef("""I give name "gwen", age "", gender "female" and title """"", None) should be (
      Some(stepdef4.copy(withParams = List(("name", "gwen"), ("age", ""), ("gender", "female"), ("title", ""))))
    )
    env.getStepDef("""I give name "gwen", age "", gender "" and title "miss"""", None) should be (
      Some(stepdef4.copy(withParams = List(("name", "gwen"), ("age", ""), ("gender", ""), ("title", "miss"))))
    )
    env.getStepDef("""I give name "", age "24", gender "female" and title """"", None) should be (
      Some(stepdef4.copy(withParams = List(("name", ""), ("age", "24"), ("gender", "female"), ("title", ""))))
    )
    env.getStepDef("""I give name "", age "24", gender "" and title "miss"""", None) should be (
      Some(stepdef4.copy(withParams = List(("name", ""), ("age", "24"), ("gender", ""), ("title", "miss"))))
    )
    env.getStepDef("""I give name "", age "", gender "female" and title "miss"""", None) should be (
      Some(stepdef4.copy(withParams = List(("name", ""), ("age", ""), ("gender", "female"), ("title", "miss"))))
    )
    env.getStepDef("""I give name "gwen", age "24", gender "female" and title """"", None) should be (
      Some(stepdef4.copy(withParams = List(("name", "gwen"), ("age", "24"), ("gender", "female"), ("title", ""))))
    )
    env.getStepDef("""I give name "gwen", age "24", gender "" and title "miss"""", None) should be (
      Some(stepdef4.copy(withParams = List(("name", "gwen"), ("age", "24"), ("gender", ""), ("title", "miss"))))
    )
    env.getStepDef("""I give name "gwen", age "", gender "female" and title "miss"""", None) should be (
      Some(stepdef4.copy(withParams = List(("name", "gwen"), ("age", ""), ("gender", "female"), ("title", "miss"))))
    )
    env.getStepDef("""I give name "", age "24", gender "female" and title "miss"""", None) should be (
      Some(stepdef4.copy(withParams = List(("name", ""), ("age", "24"), ("gender", "female"), ("title", "miss"))))
    )

    env.getStepDef("""I test "" """"", None) should be (
      Some(stepdef5.copy(withParams = List(("param1", ""), ("param2", ""))))
    )
    env.getStepDef("""I test "1" """"", None) should be (
      Some(stepdef5.copy(withParams = List(("param1", "1"), ("param2", ""))))
    )
    env.getStepDef("""I test "" "1"""", None) should be (
      Some(stepdef5.copy(withParams = List(("param1", ""), ("param2", "1"))))
    )

  }

  "Issue gwen-web#55: Conflict of step def.." should "result in ambiguous error" in {
    val stepdef1 = Scenario(List(Tag("@StepDef")), """I "<a>" on "<b>" and "<c>"""", Nil, None, Nil)
    val stepdef2 = Scenario(List(Tag("@StepDef")), """I "<a>" on "<b>"""", Nil, None, Nil)

    val env = newEnv(StateLevel.feature)
    env.addStepDef(stepdef1)
    env.addStepDef(stepdef2)

    intercept[AmbiguousCaseException] {
      env.getStepDef("""I "1" on "2" and "3"""", None)
    }

  }

  "Issue gwen-web#55-1: Conflict of step def.." should "result in ambiguous error" in {
    val stepdef1 = Scenario(List(Tag("@StepDef")), """I <a> on <b> and <c>""", Nil, None, Nil)
    val stepdef2 = Scenario(List(Tag("@StepDef")), """I <a> on <b>""", Nil, None, Nil)

    val env = newEnv(StateLevel.feature)
    env.addStepDef(stepdef1)
    env.addStepDef(stepdef2)

    intercept[AmbiguousCaseException] {
      env.getStepDef("""I 1 on 2 and 3""", None)
    }

  }
  
  private def newEnv(stateLevel: StateLevel): Environment = new Environment(EnvState(stateLevel)) { 
    override def getBoundValue(name: String): String = topScope.get(name)
  }

}
