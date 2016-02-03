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
 
class ScopedDataTest extends FlatSpec with Matchers {

  "new scope" should "not contain any attributes" in {
    val scope = ScopedData("login")
    scope.json.toString    should be ("""{"scope":"login","atts":[]}""")
    scope.getOpt("userId") should be (None)
  }
  
  "scope with a null attribute" should "yield None for getOpt call" in {
    val scope = ScopedData("login").set("userId", null)
    scope.json.toString    should be ("""{"scope":"login","atts":[{"userId":null}]}""")
    scope.getOpt("userId") should be (None)
  }
  
  "scope with a null attribute" should "throw error for get call" in {
    val scope = ScopedData("login").set("userId", null)
    scope.json.toString    should be ("""{"scope":"login","atts":[{"userId":null}]}""")
    intercept[UnboundAttributeException] {
      scope.get("userId")
    }
  }
  
  "scope with one attribute" should "contain only that attribute" in {
    val scope = ScopedData("login").set("userId", "gwen")
    scope.json.toString    should be ("""{"scope":"login","atts":[{"userId":"gwen"}]}""")
    scope.getOpt("userId") should be (Some("gwen"))
    scope.getOpt("UserId") should be (None)
  }
  
  "scope with two attributes" should "contain those two attributes" in {
    val scope = ScopedData("login").set("userId", "gwen").set("password", "pwd")
    scope.json.toString      should be ("""{"scope":"login","atts":[{"userId":"gwen"},{"password":"pwd"}]}""")
    scope.getOpt("userId")   should be (Some("gwen"))
    scope.getOpt("password") should be (Some("pwd"))
    scope.getOpt("UserId")   should be (None)
    scope.getOpt("Password") should be (None)
  }
  
  "get lookup on scope with two same named attributes" should "return the most recently added one" in {
    val scope = ScopedData("register").set("name", "todd").set("name", "gwen")
    scope.json.toString  should be ("""{"scope":"register","atts":[{"name":"todd"},{"name":"gwen"}]}""")
    scope.getOpt("name") should be (Some("gwen"))
  }
  
  "binding the same name and value" should "should not recreate the binding" in {
    val scope = ScopedData("register").set("name", "gwen").set("name", "gwen")
    scope.json.toString  should be ("""{"scope":"register","atts":[{"name":"gwen"}]}""")
    scope.getOpt("name") should be (Some("gwen"))
  }
  
  "getAll lookup on scope with two same named attributes" should "return both" in {
    val scope = ScopedData("register").set("name", "todd").set("name", "gwen")
    scope.json.toString     should be ("""{"scope":"register","atts":[{"name":"todd"},{"name":"gwen"}]}""")
    scope.getAll("name") should be (Seq("todd", "gwen"))
  }
  
}