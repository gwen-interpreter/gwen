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

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import gwen.errors.UnboundAttributeException

class LocalDataStackTest extends FlatSpec with Matchers {

  "get" should "throw error when there is not data" in {
    
    val localData = new LocalDataStack()
    
    intercept[UnboundAttributeException] { localData.get("username") }
    intercept[UnboundAttributeException] { localData.get("password") }
    intercept[UnboundAttributeException] { localData.get("firstName") }
    intercept[UnboundAttributeException] { localData.get("lastName") }
  }
  
  "get" should "get visible local data" in {
    
    val localData = new LocalDataStack()
    
    localData.push("login", List(("username", "gwen"), ("password", "pwd")))
    localData.push("register", List(("password", "secret")))
    
    intercept[UnboundAttributeException] {
      localData.get("username")  should be ("gwen")
    }
    
    localData.get("password")   should be ("secret")
    
    localData.pop
    
    localData.get("username")  should be ("gwen")
    localData.get("password")   should be ("pwd")
    
  }
  
  "stack" should "behave correctly with combinations of empty and non-empty parameters" in {
    
    val localData = new LocalDataStack()
    
    localData.push("stepdef1", Nil);
    intercept[UnboundAttributeException] {
      localData.get("username")  should be ("gwen")
    }
    
    localData.push("stepdef2", List(("username", "gwen")));
    localData.get("username")  should be ("gwen")
    
    localData.push("stepdef3", Nil);
    intercept[UnboundAttributeException] {
      localData.get("username")  should be ("gwen")
    }
    
    localData.pop
    localData.get("username")  should be ("gwen")
    
    localData.pop
    intercept[UnboundAttributeException] {
      localData.get("username")  should be ("gwen")
    }
    
    localData.pop
    intercept[UnboundAttributeException] {
      localData.get("username")  should be ("gwen")
    }
    
    intercept[NoSuchElementException] {
      localData.pop
    }
    
  }
}