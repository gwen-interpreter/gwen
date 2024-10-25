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

package gwen.core.state

import gwen.core.BaseTest
import gwen.core.Errors.UnboundAttributeException

import org.scalatest.matchers.should.Matchers

class TransientStackTest extends BaseTest with Matchers {

  "get on parameter stack" should "throw error when there is not data" in {
    
    val tStack = TransientStack.paramsStack
    tStack.toString should be ("params : { }")
    
    intercept[UnboundAttributeException] { tStack.get("<username>") }
    intercept[UnboundAttributeException] { tStack.get("<password>") }
    intercept[UnboundAttributeException] { tStack.get("<firstName>") }
    intercept[UnboundAttributeException] { tStack.get("<lastName>") }
    
  }

  "get on non-parameter stack" should "throw error when there is not data" in {
    
    val tStack = TransientStack.scenarioStack
    tStack.toString should be ("scenario : { }")
    
    intercept[UnboundAttributeException] { tStack.get("<username>") }
    intercept[UnboundAttributeException] { tStack.get("<password>") }
    intercept[UnboundAttributeException] { tStack.get("<firstName>") }
    intercept[UnboundAttributeException] { tStack.get("<lastName>") }
    
  }
  
  "get on parameter stack" should "get visible data" in {
    
    val tStack = TransientStack.paramsStack
    
    tStack.push("login", List(("username", "gwen"), ("password", "pwd")))
    tStack.toString should be ("params : { scope: login, entries : [ { username: gwen }, { password: pwd } ] }")

    tStack.push("register", List(("password", "secret")))
    tStack.toString should be ("params : { scope: register, entries : [ { password: secret } ] }")
    
    intercept[UnboundAttributeException] {
      tStack.get("<username>")  should be ("gwen")
    }
    
    tStack.get("<password>")   should be ("secret")

    tStack.pop()

    tStack.get("<username>")  should be ("gwen")
    tStack.get("<password>")   should be ("pwd")
    
  }

  "get on non-parameter stack" should "get visible data" in {
    
    val tStack = TransientStack.scenarioStack
    
    tStack.push("login", List(("username", "gwen"), ("password", "pwd")))
    tStack.toString.matches("""scenario : \{ scope: login, entries : \[ \{ gwen\.scenario\.name: login \}, \{ gwen\.scenario\.eval\.start\.msecs: \d+ \}, \{ gwen\.scenario\.eval\.started: .+ \}, \{ gwen\.scenario\.eval\.status\.keyword: Passed \}, \{ gwen\.scenario\.eval\.status\.message:  \}, \{ username: gwen \}, \{ password: pwd \} \] \}""") should be (true)

    tStack.push("register", List(("password", "secret")))
    tStack.toString.matches("""scenario : \{ scope: register, entries : \[ \{ gwen\.scenario\.name: register \}, \{ gwen\.scenario\.eval\.start\.msecs: \d+ \}, \{ gwen\.scenario\.eval\.started: .+ \}, \{ gwen\.scenario\.eval\.status\.keyword: Passed \}, \{ gwen\.scenario\.eval\.status\.message:  \}, \{ password: secret \} \] \}""") should be (true)
    
    intercept[UnboundAttributeException] {
      tStack.get("username")  should be ("gwen")
    }
    
    tStack.get("password")   should be ("secret")

    tStack.pop()

    tStack.get("username")  should be ("gwen")
    tStack.get("password")   should be ("pwd")
    
  }
  
  "parameter stack" should "behave correctly with combinations of empty and non-empty data" in {
    
    val tStack = TransientStack.paramsStack
    
    tStack.push("stepdef1", Nil)
    tStack.toString should be ("params : { }")
    intercept[UnboundAttributeException] {
      tStack.get("<username>")  should be ("gwen")
    }
    
    tStack.push("stepdef2", List(("username", "gwen")))
    tStack.toString should be ("params : { scope: stepdef2, entries : [ { username: gwen } ] }")

    tStack.get("<username>")  should be ("gwen")
    
    tStack.push("stepdef3", Nil)
    tStack.toString should be ("params : { }")
    intercept[UnboundAttributeException] {
      tStack.get("<username>")  should be ("gwen")
    }

    tStack.pop()
    tStack.toString should be ("params : { scope: stepdef2, entries : [ { username: gwen } ] }")
    tStack.get("<username>")  should be ("gwen")
    
    tStack.pop()
    tStack.toString should be ("params : { }")
    intercept[UnboundAttributeException] {
      tStack.get("<username>")  should be ("gwen")
    }
    
    tStack.pop()
    tStack.toString should be ("params : { }")
    intercept[UnboundAttributeException] {
      tStack.get("<username>")  should be ("gwen")
    }

    intercept[NoSuchElementException] {
      tStack.pop()
    }
    tStack.toString should be ("params : { }")
    
  }

  "non-parameter stack" should "behave correctly with combinations of empty and non-empty data" in {
    
    val tStack = TransientStack.stepDefStack
    
    tStack.push("stepdef1", Nil)
    tStack.toString.matches("""stepDef : \{ scope: stepdef1, entries : \[ \{ gwen\.stepDef\.name: stepdef1 \}, \{ gwen\.stepDef\.eval\.start\.msecs: \d+ \}, \{ gwen\.stepDef\.eval\.started: .+ \}, \{ gwen\.stepDef\.eval\.status\.keyword: Passed \}, \{ gwen\.stepDef\.eval\.status\.message:  \} \] \}""") should be (true)
    intercept[UnboundAttributeException] {
      tStack.get("username")  should be ("gwen")
    }
    
    tStack.push("stepdef2", List(("username", "gwen")))
    tStack.toString.matches("""stepDef : \{ scope: stepdef2, entries : \[ \{ gwen\.stepDef\.name: stepdef2 \}, \{ gwen\.stepDef\.eval\.start\.msecs: \d+ \}, \{ gwen\.stepDef\.eval\.started: .+ \}, \{ gwen\.stepDef\.eval\.status\.keyword: Passed \}, \{ gwen\.stepDef\.eval\.status\.message:  \}, \{ username: gwen \} \] \}""") should be (true)

    tStack.get("username")  should be ("gwen")
    
    tStack.push("stepdef3", Nil)
    tStack.toString.matches("""stepDef : \{ scope: stepdef3, entries : \[ \{ gwen\.stepDef\.name: stepdef3 \}, \{ gwen\.stepDef\.eval\.start\.msecs: \d+ \}, \{ gwen\.stepDef\.eval\.started: .+ \}, \{ gwen\.stepDef\.eval\.status\.keyword: Passed \}, \{ gwen\.stepDef\.eval\.status\.message:  \} \] \}""") should be (true)
    intercept[UnboundAttributeException] {
      tStack.get("username")  should be ("gwen")
    }

    tStack.pop()
    tStack.toString.matches("""stepDef : \{ scope: stepdef2, entries : \[ \{ gwen\.stepDef\.name: stepdef2 \}, \{ gwen\.stepDef\.eval\.start\.msecs: \d+ \}, \{ gwen\.stepDef\.eval\.started: .+ \}, \{ gwen\.stepDef\.eval\.status\.keyword: Passed \}, \{ gwen\.stepDef\.eval\.status\.message:  \}, \{ username: gwen \} \] \}""") should be (true)
    tStack.get("username")  should be ("gwen")
    
    tStack.pop()
    tStack.toString.matches("""stepDef : \{ scope: stepdef1, entries : \[ \{ gwen\.stepDef\.name: stepdef1 \}, \{ gwen\.stepDef\.eval\.start\.msecs: \d+ \}, \{ gwen\.stepDef\.eval\.started: .+ \}, \{ gwen\.stepDef\.eval\.status\.keyword: Passed \}, \{ gwen\.stepDef\.eval\.status\.message:  \} \] \}""") should be (true)
    intercept[UnboundAttributeException] {
      tStack.get("username")  should be ("gwen")
    }
    
    tStack.pop()
    tStack.toString should be ("stepDef : { }")
    intercept[UnboundAttributeException] {
      tStack.get("username")  should be ("gwen")
    }

    intercept[NoSuchElementException] {
      tStack.pop()
    }
    tStack.toString should be ("stepDef : { }")
    
  }

}
