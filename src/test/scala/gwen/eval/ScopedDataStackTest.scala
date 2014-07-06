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

class ScopedDataStackTest extends FlatSpec with Matchers {

  "get" should "throw error when there are no scopes" in {
    
    val scopes = new ScopedDataStack("page")
    
    intercept[AttrNotFoundException] { scopes.get("username") }
    intercept[AttrNotFoundException] { scopes.get("password") }
    intercept[AttrNotFoundException] { scopes.get("firstName") }
    intercept[AttrNotFoundException] { scopes.get("lastName") }
  }
  
  "getOpt" should "return None when there are no scopes" in {
    
    val scopes = new ScopedDataStack("page")
    
    scopes.getOpt("username")  should be (None)
    scopes.getOpt("password")  should be (None)
    scopes.getOpt("firstName") should be (None)
    scopes.getOpt("lastName")  should be (None)
  }
  
  "getAll" should "not get any attributes when there are no scopes" in {
    
    val scopes = new ScopedDataStack("page")
    
    scopes.getAll("username")  should be (Nil)
    scopes.getAll("password")  should be (Nil)
    scopes.getAll("firstName") should be (Nil)
    scopes.getAll("lastName")  should be (Nil)
  }
  
  "getOpt" should "get all current page scope attributes" in {
    
    val scopes = new ScopedDataStack("page")
    val loginScope = scopes.addScope("login").set("username", "gwen").set("password", "pwd")
    
    scopes.getOpt("username") should be (Some("gwen"))
    scopes.getOpt("password") should be (Some("pwd"))
    scopes.getOpt("token")    should be (None)
    
  }
  
  "get" should "get all current page scope attributes" in {
    
    val scopes = new ScopedDataStack("page")
    val loginScope = scopes.addScope("login").set("username", "gwen").set("password", "pwd")
    
    scopes.get("username") should be ("gwen")
    scopes.get("password") should be ("pwd")
    intercept[AttrNotFoundException] { scopes.get("token") }
  }
  
  "getAll" should "get all current page scope attributes" in {
    
    val scopes = new ScopedDataStack("page")
    val loginScope = scopes.addScope("login").set("username", "gwen").set("password", "pwd")
    
    scopes.getAll("username") should be (Seq("gwen"))
    scopes.getAll("password") should be (Seq("pwd"))
    scopes.getAll("token")    should be (Nil)
  }
  
  "getOpt" should "ignore inactive page scopes" in {
    
    val scopes = new ScopedDataStack("page")
    val loginScope = scopes.addScope("login").set("username", "gwen").set("password", "pwd")
    val registerScope = scopes.addScope("register").set("firstName", "gwen").set("lastName", "tester")
    
    scopes.getOpt("username")   should be (None)
    scopes.getOpt("password")   should be (None)
    scopes.getOpt("firstName")  should be (Some("gwen"))
    scopes.getOpt("lastName")   should be (Some("tester"))
    scopes.getOpt("middleName") should be (None)
    
  }
  
  "get" should "ignore inactive page scopes" in {
    
    val scopes = new ScopedDataStack("page")
    val loginScope = scopes.addScope("login").set("username", "gwen").set("password", "pwd")
    val registerScope = scopes.addScope("register").set("firstName", "gwen").set("lastName", "tester")
    
    intercept[AttrNotFoundException] { scopes.get("username") }
    intercept[AttrNotFoundException] { scopes.get("password") }
    scopes.get("firstName")  should be ("gwen")
    scopes.get("lastName")   should be ("tester")
    intercept[AttrNotFoundException] { scopes.get("middleName") }
    
  }
  
  "getAll" should "ignore inactive page scopes" in {
    
    val scopes = new ScopedDataStack("page")
    val loginScope = scopes.addScope("login").set("username", "gwen").set("password", "pwd")
    val registerScope = scopes.addScope("register").set("firstName", "gwen").set("lastName", "tester")
    
    scopes.getAll("username")   should be (Nil)
    scopes.getAll("password")   should be (Nil)
    scopes.getAll("firstName")  should be (Seq("gwen"))
    scopes.getAll("lastName")   should be (Seq("tester"))
    scopes.getAll("middleName") should be (Nil)
    
  }
  
  "getOpt" should "get attributes in active page scopes only" in {
    
    val scopes = new ScopedDataStack("page")
    val loginScope = scopes.addScope("login").set("username", "gwen").set("password", "pwd")
    val registerScope1 = scopes.addScope("register").set("firstName", "gwen").set("lastName", "tester")
    val registerScope2 = scopes.addScope("register").set("firstName", "gwen1").set("lastName", "tester")
    
    scopes.getOpt("username")   should be (None)
    scopes.getOpt("password")   should be (None)
    scopes.getOpt("firstName")  should be (Some("gwen1"))
    scopes.getOpt("lastName")   should be (Some("tester"))
    scopes.getOpt("middleName") should be (None)
    
  }
  
  "get" should "get attributes in active page scopes only" in {
    
    val scopes = new ScopedDataStack("page")
    scopes.set("middleName", "chaos")
    val loginScope = scopes.addScope("login").set("username", "gwen").set("password", "pwd")
    val registerScope1 = scopes.addScope("register").set("firstName", "gwen").set("lastName", "tester")
    val registerScope2 = scopes.addScope("register").set("firstName", "gwen1").set("lastName", "tester")
    
    intercept[AttrNotFoundException] { scopes.get("username") }
    intercept[AttrNotFoundException] { scopes.get("password") }
    scopes.get("firstName")  should be ("gwen1")
    scopes.get("lastName")   should be ("tester")
    scopes.current.get.name  should be ("register")
    scopes.get("middleName") should be ("chaos")
    intercept[AttrNotFoundException] { scopes.get("maidenName") }
    
  }
  
  "getAll" should "get all attributes in all same named page scopes" in {
    
    val scopes = new ScopedDataStack("page")
    scopes.set("middleName", "chaos")
    val loginScope = scopes.addScope("login").set("username", "gwen").set("password", "pwd")
    val registerScope1 = scopes.addScope("register").set("firstName", "gwen").set("lastName", "tester")
    val registerScope2 = scopes.addScope("register").set("firstName", "gwen1").set("lastName", "tester")
    
    scopes.getAll("username")   should be (Nil)
    scopes.getAll("password")   should be (Nil)
    scopes.getAll("firstName")  should be (Seq("gwen1", "gwen"))
    scopes.getAll("lastName")   should be (Seq("tester", "tester"))
    scopes.getAll("middleName") should be (Seq("chaos"))
    scopes.getAll("maidenName") should be (Nil)
    
  }
  
  "getOpt" should "ignore same name attributes in non active scopes" in {
    
    val scopes = new ScopedDataStack("page")
    scopes.set("middleName", "chaos")
    val registerScope = scopes.addScope("register").set("firstName", "gwen").set("lastName", "register")
    val personScope = scopes.addScope("person").set("firstName", "gwen").set("lastName", "person").set("middleName", "chaos2")
    
    scopes.getOpt("firstName")  should be (Some("gwen"))
    scopes.getOpt("lastName")   should be (Some("person"))
    scopes.getOpt("middleName")   should be (Some("chaos2"))
    scopes.getOpt("maidenName") should be (None)
    
  }
  
  "get" should "ignore same name attributes in non active scopes" in {
    
    val scopes = new ScopedDataStack("page")
    val registerScope = scopes.addScope("register").set("firstName", "gwen").set("lastName", "register")
    val personScope = scopes.addScope("person").set("firstName", "gwen").set("lastName", "person")
    
    scopes.get("firstName")  should be ("gwen")
    scopes.get("lastName")   should be ("person")
    intercept[AttrNotFoundException] { scopes.get("middleName") }
    
  }
  
  "getAll" should "get same name attributes in currently active scope" in {
    
    val scopes = new ScopedDataStack("page")
    val registerScope = scopes.addScope("register").set("firstName", "gwen").set("lastName", "register")
    val personScope = scopes.addScope("person").set("firstName", "gwen").set("lastName", "person")
    
    scopes.getAll("firstName")  should be (Seq("gwen"))
    scopes.getAll("lastName")   should be (Seq("person"))
    scopes.getAll("middleName") should be (Nil)
  }
  
  "getAll" should "ignore same name attributes in non active scopes" in {
    
    val scopes = new ScopedDataStack("page")
    val registerScope = scopes.addScope("register").set("firstName", "gwen").set("lastName", "register")
    val personScope = scopes.addScope("person").set("firstName", "gwen").set("lastName", "person")
    
    scopes.getAll("firstName")  should be (Seq("gwen"))
    scopes.getAll("lastName")   should be (Seq("person"))
    scopes.getAll("middleName") should be (Nil)
    
    scopes.getAll("firstName")  should be (Seq("gwen"))
    scopes.getAll("lastName")   should be (Seq("person"))
    scopes.getAll("middleName") should be (Nil)
    
  }

  "getInOpt" should "get any attribute in any page scope for nominated page " in {
    
    val scopes = new ScopedDataStack("page")
    val registerScope = scopes.addScope("register").set("firstName", "gwen").set("lastName", "register")
    val personScope = scopes.addScope("person").set("firstName", "gwen").set("lastName", "person")
    
    scopes.getInOpt("register", "firstName")  should be (Some("gwen"))
    scopes.getInOpt("register", "lastName")   should be (Some("register"))
    scopes.getInOpt("person", "firstName")    should be (Some("gwen"))
    scopes.getInOpt("person", "lastName")     should be (Some("person"))
    scopes.getInOpt("register", "middleName") should be (None)
    scopes.getInOpt("person", "middleName")   should be (None)
    
  }
  
  "getIn" should "get any attribute in any page scope for nominated page " in {
    
    val scopes = new ScopedDataStack("page")
    val registerScope = scopes.addScope("register").set("firstName", "gwen").set("lastName", "register")
    val personScope = scopes.addScope("person").set("firstName", "gwen").set("lastName", "person")
    
    scopes.getIn("register", "firstName")  should be ("gwen")
    scopes.getIn("register", "lastName")   should be ("register")
    scopes.getIn("person", "firstName")    should be ("gwen")
    scopes.getIn("person", "lastName")     should be ("person")
    intercept[AttrNotFoundException] { scopes.getIn("register", "middleName") }
    intercept[AttrNotFoundException] { scopes.getIn("person", "middleName") }
  }
  
  "getAllIn" should "get any attribute in any page scope for nominated page " in {
    
    val scopes = new ScopedDataStack("page")
    val registerScope = scopes.addScope("register").set("firstName", "gwen").set("lastName", "register")
    val personScope = scopes.addScope("person").set("firstName", "gwen").set("lastName", "person")
    
    scopes.getAllIn("register", "firstName")  should be (List("gwen"))
    scopes.getAllIn("register", "lastName")   should be (List("register"))
    scopes.getAllIn("person", "firstName")    should be (List("gwen"))
    scopes.getAllIn("person", "lastName")     should be (List("person"))
    scopes.getAllIn("register", "middleName") should be (Nil)
    scopes.getAllIn("person", "middleName")   should be (Nil)
  }
  
}