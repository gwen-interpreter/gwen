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
import org.scalatest.matchers.ShouldMatchers
 
class ScopedDataTest extends FlatSpec with ShouldMatchers {

  "new scope" should "not contain any attributes" in {
    val scope = ScopedData("page", "login")
    scope.toJson.toString    should be ("""{"scope":"login","atts":[]}""")
    scope.get("userId") should be (None)
  }
  
  "scope with one attribute" should "contain only that attribute" in {
    val scope = ScopedData("page", "login").set("userId", "gwen")
    scope.toJson.toString    should be ("""{"scope":"login","atts":[{"userId":"gwen"}]}""")
    scope.get("userId") should be (Some("gwen"))
    scope.get("UserId") should be (None)
  }
  
  "scope with two attributes" should "contain those two attributes" in {
    val scope = ScopedData("page", "login").set("userId", "gwen").set("password", "pwd")
    scope.toJson.toString      should be ("""{"scope":"login","atts":[{"userId":"gwen"},{"password":"pwd"}]}""")
    scope.get("userId")   should be (Some("gwen"))
    scope.get("password") should be (Some("pwd"))
    scope.get("UserId")   should be (None)
    scope.get("Password") should be (None)
  }
  
  "get lookup on scope with two same named attributes" should "return the most recently added one" in {
    val scope = ScopedData("page", "register").set("name", "todd").set("name", "gwen")
    scope.toJson.toString  should be ("""{"scope":"register","atts":[{"name":"todd"},{"name":"gwen"}]}""")
    scope.get("name") should be (Some("gwen"))
  }
  
  "getAll lookup on scope with two same named attributes" should "return both" in {
    val scope = ScopedData("page", "register").set("name", "todd").set("name", "gwen")
    scope.toJson.toString     should be ("""{"scope":"register","atts":[{"name":"todd"},{"name":"gwen"}]}""")
    scope.getAll("name") should be (Seq("todd", "gwen"))
  }
  
}