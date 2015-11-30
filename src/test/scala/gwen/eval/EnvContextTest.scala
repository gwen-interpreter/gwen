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

package gwen.eval

import org.scalatest.Matchers
import gwen.dsl.Scenario
import gwen.dsl.Step
import gwen.dsl.StepKeyword
import gwen.dsl.Tag
import org.scalatest.FlatSpec
import gwen.errors.`package`.AmbiguousCaseException

class EnvContextTest extends FlatSpec with Matchers {
  
  "New env context" should "contain no StepDefs" in {
    val env = newEnv
    env.getStepDef("google it") should be (None)
  }
  
  "New StepDef added to env context" should "be accessible" in {
    
    val steps = List(
      Step(StepKeyword.Given, """I enter "gwen" in the search field"""),
      Step(StepKeyword.And, """I submit the search field""")
    )
    
    val stepdef = Scenario(Set(Tag("StepDef")), """I search for "gwen"""", Nil, None, steps)
    val env = newEnv
    env.addStepDef(stepdef)
    
    env.getStepDef("""I search for "gwen"""") should be (Some((stepdef, Nil)))
    env.getStepDef("I am not defined") should be (None)
    
  }
  
  "New StepDef added to env context" should "not be accessible after reset" in {
    
    val steps = List(
      Step(StepKeyword.Given, """I enter "gwen" in the search field"""),
      Step(StepKeyword.And, """I submit the search field""")
    )
    
    val stepdef = Scenario(Set(Tag("StepDef")), """I search for "gwen"""", Nil, None, steps)
    val env = newEnv
    env.addStepDef(stepdef)
    
    env.getStepDef("""I search for "gwen"""") should be (Some((stepdef, Nil)))
    env.reset
    env.getStepDef("""I search for "gwen"""") should be (None)
    
  }
  
  "StepDef with params" should "resolve" in {
    
    val stepdef1 = Scenario(Set(Tag("StepDef")), """I enter "<searchTerm>" in the search field""", Nil, None, Nil)
    val stepdef2 = Scenario(Set(Tag("StepDef")), """I enter "<search term>" in the search field again""", Nil, None, Nil)
    val stepdef3 = Scenario(Set(Tag("StepDef")), "z = <x> + 1", Nil, None, Nil)
    val stepdef4 = Scenario(Set(Tag("StepDef")), "z = 1 + <x>", Nil, None, Nil)
    val stepdef5 = Scenario(Set(Tag("StepDef")), "z = <x> - <y>", Nil, None, Nil)
    
    val env = newEnv
    env.addStepDef(stepdef1)
    env.addStepDef(stepdef2)
    env.addStepDef(stepdef3)
    env.addStepDef(stepdef4)
    env.addStepDef(stepdef5)
    
    env.getStepDef("""I enter "gwen" in the search field""") should be (Some((stepdef1, List(("<searchTerm>", "gwen")))))
    env.getStepDef("""I enter "gwen" in the search field again""") should be (Some((stepdef2, List(("<search term>", "gwen")))))
    env.getStepDef("z = 2 + 1") should be (Some((stepdef3, List(("<x>", "2")))))
    env.getStepDef("z = 1 + 3") should be (Some((stepdef4, List(("<x>", "3")))))
    env.getStepDef("z = 2 - 2") should be (Some((stepdef5, List(("<x>", "2"), ("<y>", "2")))))

    val stepdef6 = Scenario(Set(Tag("StepDef")), "z = <x> * <x>", Nil, None, Nil)
    env.addStepDef(stepdef6)
    intercept[AmbiguousCaseException] {
      env.getStepDef("z = 3 * 4")
    }
    
  }
  
  "Sample math StepDefs with parameters" should "resolve" in {
    
    val stepdef1 = Scenario(Set(Tag("StepDef")), "++x", Nil, None, Nil)
    val stepdef2 = Scenario(Set(Tag("StepDef")), "z = x + <y>", Nil, None, Nil)
    val stepdef3 = Scenario(Set(Tag("StepDef")), "z = <x> + <y>", Nil, None, Nil)
    
    val env = newEnv
    env.addStepDef(stepdef1)
    env.addStepDef(stepdef2)
    env.addStepDef(stepdef3)
    
    env.getStepDef("z = x + 3") should be (Some((stepdef2, List(("<y>", "3")))))
    env.getStepDef("z = 2 + 2") should be (Some((stepdef3, List(("<x>", "2"), ("<y>", "2")))))
    env.getStepDef("z = 3 + 2") should be (Some((stepdef3, List(("<x>", "3"), ("<y>", "2")))))
    env.getStepDef("z = 2 + 3") should be (Some((stepdef3, List(("<x>", "2"), ("<y>", "3")))))
    env.getStepDef("z = 5 + y") should be (Some((stepdef3, List(("<x>", "5"), ("<y>", "y")))))
    
  }
  
  "New feature env context" should "have global feature scope" in {
    val env = newEnv
    env.featureScope.scope should be ("feature")
  }
  
  "Bound feature scope attribute" should "be removed after reset" in {
    val env = newEnv
    env.featureScope.set("engineName", "Gwen-Core")
    env.featureScope.get("engineName") should be ("Gwen-Core")
    env.reset
    env.featureScope.getOpt("engineName") should be (None)
    env.featureScope.scope should be ("feature")
  }
  
  "Bound feature scope attribute" should "show up in JSON string" in {
    val env = newEnv
    env.featureScope.set("firstName", "Gwen")
    env.featureScope.get("firstName") should be ("Gwen")
    env.json.toString should be ("""{"scopes":[{"scope":"feature","atts":[{"firstName":"Gwen"}]}]}""")
                                      
  }
  
  "json.toString on new env context" should "contain empty scopes" in {
    val env = newEnv
    env.json.toString should be ("""{"scopes":[{"scope":"feature","atts":[]}]}""")
  }
  
  "visibleScopes.json.toString on new env context" should "contain empty scopes" in {
    val env = newEnv
    env.visibleScopes.json.toString should be ("""{"scopes":[{"scope":"feature","atts":[]}]}""")
  }
  
  "json.toString on new env context with bound var in global scope" should "print the var" in {
    val env = newEnv
    env.addScope("vars").set("howdy", "partner")
    env.json.toString should be ("""{"scopes":[{"scope":"feature","atts":[]},{"scope":"vars","atts":[{"howdy":"partner"}]}]}""")
  }
  
  "visibleScopes.json.toString on new env context with bound var in global scope" should "print the var" in {
    val env = newEnv
    env.addScope("vars").set("howdy", "partner")
    env.visibleScopes.json.toString should be ("""{"scopes":[{"scope":"feature","atts":[]},{"scope":"vars","atts":[{"howdy":"partner"}]}]}""")
  }
  
  "json.toString on reset env context" should "contain empty scopes" in {
    val env = newEnv
    env.addScope("vars").set("howdy", "partner")
    env.reset
    env.json.toString should be ("""{"scopes":[{"scope":"feature","atts":[]}]}""")
  }
  
  "visibleScopes.json.toString on reset env context" should "contain empty scopes" in {
    val env = newEnv
    env.addScope("vars").set("howdy", "partner")
    env.reset
    env.visibleScopes.json.toString should be ("""{"scopes":[{"scope":"feature","atts":[]}]}""")
  }
  
  "json.toString on closed env context" should "contain empty scopes" in {
    val env = newEnv
    env.addScope("vars").set("howdy", "partner")
    env.close
    env.json.toString should be ("""{"scopes":[{"scope":"feature","atts":[]}]}""")
  }
  
  "visibleScopes.json.toString on closed env context" should "contain empty scopes" in {
    val env = newEnv
    env.addScope("vars").set("howdy", "partner")
    env.close
    env.visibleScopes.json.toString should be ("""{"scopes":[{"scope":"feature","atts":[]}]}""")
  }
  
  "visibleScopes.json.toString on loaded env context" should "returned only visible data" in {
    val env = newEnv
    val vars = env.addScope("vars")
    vars.set("howdy", "partner")
    vars.set("page", "home")
    env.visibleScopes.json.toString should be ("""{"scopes":[{"scope":"feature","atts":[]},{"scope":"vars","atts":[{"howdy":"partner"},{"page":"home"}]}]}""")
    vars.set("page", "dashboard")
    env.visibleScopes.json.toString should be ("""{"scopes":[{"scope":"feature","atts":[]},{"scope":"vars","atts":[{"howdy":"partner"},{"page":"home"},{"page":"dashboard"}]}]}""")
  }
  
  "env.filterAtts on loaded env context" should "should filter attributes correctly" in {
    val env = newEnv
    val vars = env.addScope("vars")
    vars.set("howdy", "partner")
    vars.set("page", "home")
    vars.set("page", "dashboard")
    env.filterAtts { case (name, _) => name == "page"}.visible.json.toString should be ("""{"scopes":[{"scope":"vars","atts":[{"page":"home"},{"page":"dashboard"}]}]}""")
  }
  
  "env.filterAtts on empty context" should "should return empty value" in {
    val env = newEnv
    env.filterAtts { case (name, _) => name == "page"}.visible.json.toString should be ("""{"scopes":[]}""")
  }
  
  "dry run" should "not call instruction" in {
    val env = newEnv(GwenOptions(dryRun = true))
    env.execute(sys.error("Execution not expected"))
  }
  
  "non dry run" should "call instruction" in {
    val env = newEnv(GwenOptions(dryRun = false))
    intercept[Exception] {
      env.execute(sys.error("Execution expected"))
    }
  }
  
  private def newEnv: EnvContext = newEnv(GwenOptions())
  
  private def newEnv(options: GwenOptions): EnvContext = new EnvContext(options, new ScopedDataStack()) { 
    var closed = false
    override def close() {
      super.reset()
      closed = true
    }
  }
}
