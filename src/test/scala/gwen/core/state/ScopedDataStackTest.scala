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
import gwen.core.Errors.UnboundAttributeException

import org.scalatest.matchers.should.Matchers

class ScopedDataStackTest extends BaseTest with Matchers {

  "get on parameter stack" should "throw error when there is not data" in {
    
    val sdStack = ScopedDataStack.paramsStack
    sdStack.asString should be ("""scope : "params" { }""")
    
    intercept[UnboundAttributeException] { sdStack.get("<username>") }
    intercept[UnboundAttributeException] { sdStack.get("<password>") }
    intercept[UnboundAttributeException] { sdStack.get("<firstName>") }
    intercept[UnboundAttributeException] { sdStack.get("<lastName>") }
    
  }

  "get on non-parameter stack" should "throw error when there is not data" in {
    
    val sdStack = ScopedDataStack.scenarioStack
    sdStack.asString should be ("""scope : "scenario" { }""")
    
    intercept[UnboundAttributeException] { sdStack.get("<username>") }
    intercept[UnboundAttributeException] { sdStack.get("<password>") }
    intercept[UnboundAttributeException] { sdStack.get("<firstName>") }
    intercept[UnboundAttributeException] { sdStack.get("<lastName>") }
    
  }
  
  "get on parameter stack" should "get visible data" in {
    
    val pStack = ScopedDataStack.paramsStack
    pStack.boundary("login", List(("username", "gwen"), ("password", "pwd"))) {
      pStack.asString should be (
        """|scope : "params:login" {
           |  username : "gwen"
           |  password : "pwd"
           |}""".stripMargin
      )
      pStack.boundary("register", List(("password", "secret"))) {
        pStack.asString should be (
          """|scope : "params:register" {
             |  password : "secret"
             |}""".stripMargin
        )
        intercept[UnboundAttributeException] {
          pStack.get("<username>")  should be ("gwen")
        }
        pStack.get("<password>")   should be ("secret")
      }
      pStack.get("<username>")  should be ("gwen")
      pStack.get("<password>")  should be ("pwd")
    }

  }

  "get on non-parameter stack" should "get visible data" in {
    
    val sStack = ScopedDataStack.scenarioStack
    sStack.boundary("login", List(("username", "gwen"), ("password", "pwd"))) {
      sStack.asString should be (
        s"""|scope : "scenario:login" {
           |  gwen.scenario.name : "login"
           |  gwen.scenario.eval.start.msecs : "${sStack.get("gwen.scenario.eval.start.msecs")}"
           |  gwen.scenario.eval.started : "${sStack.get("gwen.scenario.eval.started")}"
           |  gwen.scenario.eval.status.keyword : "Passed"
           |  gwen.scenario.eval.status.message : ""
           |  username : "gwen"
           |  password : "pwd"
           |}""".stripMargin
        )
      sStack.boundary("register", List(("password", "secret"))) {
        sStack.asString should be (
          s"""|scope : "scenario:register" {
            |  gwen.scenario.name : "register"
            |  gwen.scenario.eval.start.msecs : "${sStack.get("gwen.scenario.eval.start.msecs")}"
            |  gwen.scenario.eval.started : "${sStack.get("gwen.scenario.eval.started")}"
            |  gwen.scenario.eval.status.keyword : "Passed"
            |  gwen.scenario.eval.status.message : ""
            |  password : "secret"
            |}""".stripMargin
        )
        intercept[UnboundAttributeException] {
          sStack.get("username")  should be ("gwen")
        }
        sStack.get("password")   should be ("secret")
      }
      sStack.get("username")  should be ("gwen")
      sStack.get("password")   should be ("pwd")
    }
    
  }
  
  "parameter stack" should "behave correctly with combinations of empty and non-empty data" in {
    
    val pStack = ScopedDataStack.paramsStack
    
    pStack.boundary("stepdef1", Nil) {
      pStack.asString should be ("""scope : "params" { }""")
      intercept[UnboundAttributeException] {
        pStack.get("<username>")  should be ("gwen")
      }
      pStack.boundary("stepdef2", List(("username", "gwen"))) {
        pStack.asString should be (
          """|scope : "params:stepdef2" {
             |  username : "gwen"
             |}""".stripMargin
        )
        pStack.get("<username>")  should be ("gwen")
        pStack.boundary("stepdef3", Nil) {
          pStack.asString should be ("""scope : "params" { }""")
          intercept[UnboundAttributeException] {
            pStack.get("<username>")  should be ("gwen")
          }
        }
        pStack.asString should be (
          """|scope : "params:stepdef2" {
             |  username : "gwen"
             |}""".stripMargin
        )
        pStack.get("<username>")  should be ("gwen")
      }
      pStack.asString should be  ("""scope : "params" { }""")
      intercept[UnboundAttributeException] {
        pStack.get("<username>")  should be ("gwen")
      }
    }
    pStack.asString should be  ("""scope : "params" { }""")
    intercept[UnboundAttributeException] {
      pStack.get("<username>")  should be ("gwen")
    }
    pStack.asString should be  ("""scope : "params" { }""")
    
  }

  "non-parameter stack" should "behave correctly with combinations of empty and non-empty data" in {
    
    val sdStack = ScopedDataStack.stepDefStack
    
    sdStack.boundary("stepdef1", Nil) {
      sdStack.asString should be (
        s"""|scope : "stepDef:stepdef1" {
           |  gwen.stepDef.name : "stepdef1"
           |  gwen.stepDef.eval.start.msecs : "${sdStack.get("gwen.stepDef.eval.start.msecs")}"
           |  gwen.stepDef.eval.started : "${sdStack.get("gwen.stepDef.eval.started")}"
           |  gwen.stepDef.eval.status.keyword : "Passed"
           |  gwen.stepDef.eval.status.message : ""
           |}""".stripMargin
        )
      intercept[UnboundAttributeException] {
        sdStack.get("username")  should be ("gwen")
      }
      sdStack.boundary("stepdef2", List(("username", "gwen"))) {
        sdStack.asString should be (
          s"""|scope : "stepDef:stepdef2" {
            |  gwen.stepDef.name : "stepdef2"
            |  gwen.stepDef.eval.start.msecs : "${sdStack.get("gwen.stepDef.eval.start.msecs")}"
            |  gwen.stepDef.eval.started : "${sdStack.get("gwen.stepDef.eval.started")}"
            |  gwen.stepDef.eval.status.keyword : "Passed"
            |  gwen.stepDef.eval.status.message : ""
            |  username : "gwen"
            |}""".stripMargin
          )
        sdStack.get("username")  should be ("gwen")
        sdStack.boundary("stepdef3", Nil) {
          sdStack.asString should be (
            s"""|scope : "stepDef:stepdef3" {
              |  gwen.stepDef.name : "stepdef3"
              |  gwen.stepDef.eval.start.msecs : "${sdStack.get("gwen.stepDef.eval.start.msecs")}"
              |  gwen.stepDef.eval.started : "${sdStack.get("gwen.stepDef.eval.started")}"
              |  gwen.stepDef.eval.status.keyword : "Passed"
              |  gwen.stepDef.eval.status.message : ""
              |}""".stripMargin
            )
          intercept[UnboundAttributeException] {
            sdStack.get("username")  should be ("gwen")
          }
        }
        sdStack.asString should be (
          s"""|scope : "stepDef:stepdef2" {
            |  gwen.stepDef.name : "stepdef2"
            |  gwen.stepDef.eval.start.msecs : "${sdStack.get("gwen.stepDef.eval.start.msecs")}"
            |  gwen.stepDef.eval.started : "${sdStack.get("gwen.stepDef.eval.started")}"
            |  gwen.stepDef.eval.status.keyword : "Passed"
            |  gwen.stepDef.eval.status.message : ""
            |  username : "gwen"
            |}""".stripMargin
          )
        sdStack.get("username")  should be ("gwen")
      }
      sdStack.asString should be (
          s"""|scope : "stepDef:stepdef1" {
            |  gwen.stepDef.name : "stepdef1"
            |  gwen.stepDef.eval.start.msecs : "${sdStack.get("gwen.stepDef.eval.start.msecs")}"
            |  gwen.stepDef.eval.started : "${sdStack.get("gwen.stepDef.eval.started")}"
            |  gwen.stepDef.eval.status.keyword : "Passed"
            |  gwen.stepDef.eval.status.message : ""
            |}""".stripMargin
          )
      intercept[UnboundAttributeException] {
        sdStack.get("username")  should be ("gwen")
      }
    }
    sdStack.asString should be ("""scope : "stepDef" { }""")
    intercept[UnboundAttributeException] {
      sdStack.get("username")  should be ("gwen")
    }
    sdStack.asString should be  ("""scope : "stepDef" { }""")
    
  }

}
