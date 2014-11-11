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

package gwen.eval

import org.scalatest.Matchers
import gwen.dsl.Scenario
import gwen.dsl.Step
import gwen.dsl.StepKeyword
import gwen.dsl.Tag
import org.scalatest.FlatSpec

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
    
    val stepdef = Scenario(Set(Tag("StepDef")), """I search for "gwen"""", steps)
    val env = newEnv
    env.addStepDef(stepdef)
    
    env.getStepDef("""I search for "gwen"""") should be (Some(stepdef))
    env.getStepDef("I am not defined") should be (None)
    
  }
  
  "New StepDef added to env context" should "not be accessible after reset" in {
    
    val steps = List(
      Step(StepKeyword.Given, """I enter "gwen" in the search field"""),
      Step(StepKeyword.And, """I submit the search field""")
    )
    
    val stepdef = Scenario(Set(Tag("StepDef")), """I search for "gwen"""", steps)
    val env = newEnv
    env.addStepDef(stepdef)
    
    env.getStepDef("""I search for "gwen"""") should be (Some(stepdef))
    env.reset
    env.getStepDef("""I search for "gwen"""") should be (None)
    
  }
  
  "New feature env context" should "have global feature scope" in {
    val env = newEnv
    env.featureScope.current.get.scope should be ("feature")
    env.featureScope.current.get.name should be ("global") 
  }
  
  "Bound feature scope attribute" should "be removed after reset" in {
    val env = newEnv
    env.featureScope.set("engineName", "Gwen-Core")
    env.featureScope.get("engineName") should be ("Gwen-Core")
    env.reset
    env.featureScope.getOpt("engineName") should be (None)
    env.featureScope.current.get.scope should be ("feature")
    env.featureScope.current.get.name should be ("global")
  }
  
  "Bound feature scope attribute" should "show up in JSON string" in {
    val env = newEnv
    env.featureScope.set("firstName", "Gwen")
    env.featureScope.get("firstName") should be ("Gwen")
    env.toJson.toString should be ("""{"env -all":{"data":[{"feature":[{"scope":"global","atts":[{"firstName":"Gwen"}]}]}]}}""")
  }
  
  "toJson.toString on new env context" should "contain empty scopes" in {
    val env = newEnv
    env.toJson.toString should be ("""{"env -all":{"data":[]}}""")
  }
  
  "visibleJson.toString on new env context" should "contain empty scopes" in {
    val env = newEnv
    env.visibleJson.toString should be ("""{"env -visible":{"data":[]}}""")
  }
  
  "toJson.toString on new env context with bound var in global scope" should "print the var" in {
    val env = newEnv
    env.dataScope("vars").set("howdy", "partner")
    env.toJson.toString should be ("""{"env -all":{"data":[{"vars":[{"scope":"global","atts":[{"howdy":"partner"}]}]}]}}""")
  }
  
  "visibleJson.toString on new env context with bound var in global scope" should "print the var" in {
    val env = newEnv
    env.dataScope("vars").set("howdy", "partner")
    env.visibleJson.toString should be ("""{"env -visible":{"data":[{"vars":[{"scope":"global","atts":[{"howdy":"partner"}]}]}]}}""")
  }
  
  "toJson.toString on reset env context" should "contain empty scopes" in {
    val env = newEnv
    env.dataScope("vars").set("howdy", "partner")
    env.reset
    env.toJson.toString should be ("""{"env -all":{"data":[]}}""")
  }
  
  "visibleJson.toString on reset env context" should "contain empty scopes" in {
    val env = newEnv
    env.dataScope("vars").set("howdy", "partner")
    env.reset
    env.visibleJson.toString should be ("""{"env -visible":{"data":[]}}""")
  }
  
  "toJson.toString on closed env context" should "contain empty scopes" in {
    val env = newEnv
    env.dataScope("vars").set("howdy", "partner")
    env.close
    env.toJson.toString should be ("""{"env -all":{"data":[]}}""")
  }
  
  "visibleJson.toString on closed env context" should "contain empty scopes" in {
    val env = newEnv
    env.dataScope("vars").set("howdy", "partner")
    env.close
    env.visibleJson.toString should be ("""{"env -visible":{"data":[]}}""")
  }
  
  "visibleJson.toString on loaded env context" should "returned only visible data" in {
    val env = newEnv
    val vars = env.dataScope("vars")
    vars.set("howdy", "partner")
    vars.addScope("home page").set("page", "home")
    env.visibleJson.toString should be ("""{"env -visible":{"data":[{"vars":[{"scope":"global","atts":[{"howdy":"partner"}]},{"scope":"home page","atts":[{"page":"home"}]}]}]}}""")
    vars.addScope("landing page").set("page", "dashboard")
    env.visibleJson.toString should be ("""{"env -visible":{"data":[{"vars":[{"scope":"global","atts":[{"howdy":"partner"}]},{"scope":"landing page","atts":[{"page":"dashboard"}]}]}]}}""")
  }
  
  private def newEnv: EnvContext = new EnvContext(new DataScopes()) { 
    var closed = false
    override def close() {
      super.reset()
      closed = true
    }
  }
}
