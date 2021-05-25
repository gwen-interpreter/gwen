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

package gwen.core.model.state

import gwen.core.Errors.UnboundAttributeException

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class ParameterStackTest extends FlatSpec with Matchers {

  "get" should "throw error when there is not data" in {
    
    val params = new ParameterStack()
    params.toString should be ("params : { }")
    
    intercept[UnboundAttributeException] { params.get("username") }
    intercept[UnboundAttributeException] { params.get("password") }
    intercept[UnboundAttributeException] { params.get("firstName") }
    intercept[UnboundAttributeException] { params.get("lastName") }
    
  }
  
  "get" should "get visible parameters" in {
    
    val params = new ParameterStack()
    
    params.push("login", List(("username", "gwen"), ("password", "pwd")))
    params.toString should be ("params : { scope: login, entries : [ { username: gwen }, { password: pwd } ] }")

    params.push("register", List(("password", "secret")))
    params.toString should be ("params : { scope: register, entries : [ { password: secret } ] }")
    
    intercept[UnboundAttributeException] {
      params.get("username")  should be ("gwen")
    }
    
    params.get("password")   should be ("secret")

    params.pop()
    println("params : { }")

    params.get("username")  should be ("gwen")
    params.get("password")   should be ("pwd")
    
  }
  
  "stack" should "behave correctly with combinations of empty and non-empty parameters" in {
    
    val params = new ParameterStack()
    
    params.push("stepdef1", Nil)
    params.toString should be ("params : { }")
    intercept[UnboundAttributeException] {
      params.get("username")  should be ("gwen")
    }
    
    params.push("stepdef2", List(("username", "gwen")))
    params.toString should be ("params : { scope: stepdef2, entries : [ { username: gwen } ] }")

    params.get("username")  should be ("gwen")
    
    params.push("stepdef3", Nil)
    params.toString should be ("params : { }")
    intercept[UnboundAttributeException] {
      params.get("username")  should be ("gwen")
    }

    params.pop()
    params.toString should be ("params : { scope: stepdef2, entries : [ { username: gwen } ] }")
    params.get("username")  should be ("gwen")
    
    params.pop()
    params.toString should be ("params : { }")
    intercept[UnboundAttributeException] {
      params.get("username")  should be ("gwen")
    }
    
    params.pop()
    params.toString should be ("params : { }")
    intercept[UnboundAttributeException] {
      params.get("username")  should be ("gwen")
    }

    intercept[NoSuchElementException] {
      params.pop()
    }
    params.toString should be ("params : { }")
    
  }
}
