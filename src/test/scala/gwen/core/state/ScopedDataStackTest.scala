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

import scala.collection.mutable

import org.scalatest.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks.forAll

class ScopedDataStackTest extends BaseTest with Matchers {

  "get" should "throw error when there are no scopes" in {
    
    val scopes = new ScopedDataStack()
    
    intercept[UnboundAttributeException] { scopes.get("username") }
    intercept[UnboundAttributeException] { scopes.get("password") }
    intercept[UnboundAttributeException] { scopes.get("firstName") }
    intercept[UnboundAttributeException] { scopes.get("lastName") }
  }
  
  "getOpt" should "return None when there are no scopes" in {
    
    val scopes = new ScopedDataStack()
    
    scopes.getOpt("username")  should be (None)
    scopes.getOpt("password")  should be (None)
    scopes.getOpt("firstName") should be (None)
    scopes.getOpt("lastName")  should be (None)
  }
  
  "getAll" should "not get any attributes when there are no scopes" in {
    
    val scopes = new ScopedDataStack()
    
    scopes.getAll("username")  should be (Nil)
    scopes.getAll("password")  should be (Nil)
    scopes.getAll("firstName") should be (Nil)
    scopes.getAll("lastName")  should be (Nil)
  }
  
  "getOpt" should "get all current page scope attributes" in {
    
    val scopes = new ScopedDataStack()
    scopes.addScope("login")
    scopes.set("username", "gwen")
    scopes.set("password", "pwd")
    
    scopes.getOpt("username") should be (Some("gwen"))
    scopes.getOpt("password") should be (Some("pwd"))
    scopes.getOpt("token")    should be (None)
    
  }
  
  "get" should "get all current page scope attributes" in {
    
    val scopes = new ScopedDataStack()
    scopes.addScope("login")
    scopes.set("username", "gwen")
    scopes.set("password", "pwd")
    
    scopes.get("username") should be ("gwen")
    scopes.get("password") should be ("pwd")
    intercept[UnboundAttributeException] { scopes.get("token") }
  }
  
  "getAll" should "get all current page scope attributes" in {
    
    val scopes = new ScopedDataStack()
    scopes.addScope("login")
    scopes.set("username", "gwen")
    scopes.set("password", "pwd")
    
    scopes.getAll("username") should be (Seq("gwen"))
    scopes.getAll("password") should be (Seq("pwd"))
    scopes.getAll("token")    should be (Nil)
  }
  
  "getOpt" should "ignore inactive page scopes" in {
    
    val scopes = new ScopedDataStack()
    scopes.addScope("login")
    scopes.set("username", "gwen")
    scopes.set("password", "pwd")
    scopes.addScope("register")
    scopes.set("firstName", "gwen")
    scopes.set("lastName", "tester")
    
    scopes.getOpt("username")   should be (None)
    scopes.getOpt("password")   should be (None)
    scopes.getOpt("firstName")  should be (Some("gwen"))
    scopes.getOpt("lastName")   should be (Some("tester"))
    scopes.getOpt("middleName") should be (None)
    
  }
  
  "get" should "ignore inactive page scopes" in {
    
    val scopes = new ScopedDataStack()
    scopes.addScope("login")
    scopes.set("username", "gwen")
    scopes.set("password", "pwd")
    scopes.addScope("register")
    scopes.set("firstName", "gwen")
    scopes.set("lastName", "tester")
    
    intercept[UnboundAttributeException] { scopes.get("username") }
    intercept[UnboundAttributeException] { scopes.get("password") }
    scopes.get("firstName")  should be ("gwen")
    scopes.get("lastName")   should be ("tester")
    intercept[UnboundAttributeException] { scopes.get("middleName") }
    
  }
  
  "getAll" should "ignore inactive page scopes" in {
    
    val scopes = new ScopedDataStack()
    scopes.addScope("login")
    scopes.set("username", "gwen")
    scopes.set("password", "pwd")
    scopes.addScope("register")
    scopes.set("firstName", "gwen")
    scopes.set("lastName", "tester")
    
    scopes.getAll("username")   should be (Nil)
    scopes.getAll("password")   should be (Nil)
    scopes.getAll("firstName")  should be (Seq("gwen"))
    scopes.getAll("lastName")   should be (Seq("tester"))
    scopes.getAll("middleName") should be (Nil)
    
  }
  
  "getOpt" should "get latest attributes in active page scopes only" in {
    
    val scopes = new ScopedDataStack()
    scopes.addScope("login")
    scopes.set("username", "gwen")
    scopes.set("password", "pwd")
    scopes.addScope("register")
    scopes.set("firstName", "gwen")
    scopes.set("lastName", "tester")
    scopes.addScope("register")
    scopes.set("firstName", "gwen1")
    scopes.set("lastName", "tester")
    
    scopes.getOpt("username")   should be (None)
    scopes.getOpt("password")   should be (None)
    scopes.getOpt("firstName")  should be (Some("gwen1"))
    scopes.getOpt("lastName")   should be (Some("tester"))
    scopes.getOpt("middleName") should be (None)
    
  }
  
  "get" should "get latest attributes in active page scopes only" in {
    
    val scopes = new ScopedDataStack()
    scopes.set("middleName", "chaos")
    scopes.addScope("login")
    scopes.set("username", "gwen")
    scopes.set("password", "pwd")
    scopes.addScope("register")
    scopes.set("firstName", "gwen")
    scopes.set("lastName", "tester")
    scopes.addScope("register")
    scopes.set("firstName", "gwen1")
    scopes.set("lastName", "tester")
    
    intercept[UnboundAttributeException] { scopes.get("username") }
    intercept[UnboundAttributeException] { scopes.get("password") }
    scopes.get("firstName")  should be ("gwen1")
    scopes.get("lastName")   should be ("tester")
    scopes.current.scope  should be ("register")
    scopes.get("middleName") should be ("chaos")
    intercept[UnboundAttributeException] { scopes.get("maidenName") }
    
  }
  
  "getAll" should "get all attributes in all same named page scopes" in {
    
    val scopes = new ScopedDataStack()
    scopes.set("middleName", "chaos")
    scopes.addScope("login")
    scopes.set("username", "gwen")
    scopes.set("password", "pwd")
    scopes.addScope("register")
    scopes.set("firstName", "gwen")
    scopes.set("lastName", "tester")
    scopes.addScope("register")
    scopes.set("firstName", "gwen1")
    scopes.set("lastName", "tester")
    
    scopes.getAll("username")   should be (Nil)
    scopes.getAll("password")   should be (Nil)
    scopes.getAll("firstName")  should be (Seq("gwen", "gwen1"))
    scopes.getAll("lastName")   should be (Seq("tester"))
    scopes.getAll("middleName") should be (Seq("chaos"))
    scopes.getAll("maidenName") should be (Nil)
    
  }
  
  "getOpt" should "ignore same name attributes in non active scopes" in {
    
    val scopes = new ScopedDataStack()
    scopes.set("middleName", "chaos")
    scopes.addScope("register")
    scopes.set("firstName", "gwen")
    scopes.set("lastName", "register")
    scopes.addScope("person")
    scopes.set("firstName", "gwen")
    scopes.set("lastName", "person")
    scopes.set("middleName", "chaos2")
    
    scopes.getOpt("firstName")  should be (Some("gwen"))
    scopes.getOpt("lastName")   should be (Some("person"))
    scopes.getOpt("middleName")   should be (Some("chaos2"))
    scopes.getOpt("maidenName") should be (None)
    
  }
  
  "get" should "ignore same name attributes in non active scopes" in {
    
    val scopes = new ScopedDataStack()
    scopes.addScope("register")
    scopes.set("firstName", "gwen")
    scopes.set("lastName", "register")
    scopes.addScope("person")
    scopes.set("firstName", "gwen")
    scopes.set("lastName", "person")
    
    scopes.get("firstName")  should be ("gwen")
    scopes.get("lastName")   should be ("person")
    intercept[UnboundAttributeException] { scopes.get("middleName") }
    
  }
  
  "getAll" should "get same name attributes in currently active scope" in {
    
    val scopes = new ScopedDataStack()
    scopes.addScope("register")
    scopes.set("firstName", "gwen")
    scopes.set("lastName", "register")
    scopes.addScope("person")
    scopes.set("firstName", "gwen")
    scopes.set("lastName", "person")
    
    scopes.getAll("firstName")  should be (Seq("gwen"))
    scopes.getAll("lastName")   should be (Seq("person"))
    scopes.getAll("middleName") should be (Nil)
  }
  
  "getAll" should "ignore same name attributes in non active scopes" in {
    
    val scopes = new ScopedDataStack()
    scopes.addScope("register")
    scopes.set("firstName", "gwen")
    scopes.set("lastName", "register")
    scopes.addScope("person")
    scopes.set("firstName", "gwen")
    scopes.set("lastName", "person")
    
    scopes.getAll("firstName")  should be (Seq("gwen"))
    scopes.getAll("lastName")   should be (Seq("person"))
    scopes.getAll("middleName") should be (Nil)
    
    scopes.getAll("firstName")  should be (Seq("gwen"))
    scopes.getAll("lastName")   should be (Seq("person"))
    scopes.getAll("middleName") should be (Nil)
    
  }

  "getInOpt" should "get any attribute in any page scope for nominated page " in {
    
    val scopes = new ScopedDataStack()
    scopes.addScope("register")
    scopes.set("firstName", "gwen")
    scopes.set("lastName", "register")
    scopes.addScope("person")
    scopes.set("firstName", "gwen")
    scopes.set("lastName", "person")
    
    scopes.getInOpt("register", "firstName")  should be (Some("gwen"))
    scopes.getInOpt("register", "lastName")   should be (Some("register"))
    scopes.getInOpt("person", "firstName")    should be (Some("gwen"))
    scopes.getInOpt("person", "lastName")     should be (Some("person"))
    scopes.getInOpt("register", "middleName") should be (None)
    scopes.getInOpt("person", "middleName")   should be (None)
    
  }
  
  "getIn" should "get any attribute in any page scope for nominated page " in {
    
    val scopes = new ScopedDataStack()
    scopes.addScope("register")
    scopes.set("firstName", "gwen")
    scopes.set("lastName", "register")
    scopes.addScope("person")
    scopes.set("firstName", "gwen")
    scopes.set("lastName", "person")
    
    scopes.getIn("register", "firstName")  should be ("gwen")
    scopes.getIn("register", "lastName")   should be ("register")
    scopes.getIn("person", "firstName")    should be ("gwen")
    scopes.getIn("person", "lastName")     should be ("person")
    intercept[UnboundAttributeException] { scopes.getIn("register", "middleName") }
    intercept[UnboundAttributeException] { scopes.getIn("person", "middleName") }
  }
  
  "getAllIn" should "get any attribute in any page scope for nominated page " in {
    
    val scopes = new ScopedDataStack()
    scopes.addScope("register")
    scopes.set("firstName", "gwen")
    scopes.set("lastName", "register")
    scopes.addScope("person")
    scopes.set("firstName", "gwen")
    scopes.set("lastName", "person")
    
    scopes.getAllIn("register", "firstName")  should be (List("gwen"))
    scopes.getAllIn("register", "lastName")   should be (List("register"))
    scopes.getAllIn("person", "firstName")    should be (List("gwen"))
    scopes.getAllIn("person", "lastName")     should be (List("person"))
    scopes.getAllIn("register", "middleName") should be (Nil)
    scopes.getAllIn("person", "middleName")   should be (Nil)
  }
  
  "addScope" should "should keep currently active scope of same name" in {
    val scopes = new ScopedDataStack()
    val scope1 = scopes.addScope("home")
    val scope2 = scopes.addScope("home")
    scope1 should be (scope2)
  }
  
  "addScope" should "should not keep currently active scope of different name" in {
    val scopes = new ScopedDataStack()
    val scope1 = scopes.addScope("home1")
    val scope2 = scopes.addScope("home2")
    scope1 should not be scope2
  }
  
  forAll (levels) { level =>
    "addScope" should s"should maintain only one $level scope" in {
      withSetting("gwen.state.level", level) {
        val scopes = new ScopedDataStack()
        scopes.current should be (scopes.topScope)
        val home = scopes.addScope("home")
        home should be (scopes.current)
        val top = scopes.addScope(level)
        top should be (scopes.topScope)
      }
    }
  }
  
  "set" should "should not replicate already visible attributes with same name and value" in {
    
    val scopes = new ScopedDataStack()
    scopes.addScope("register")
    scopes.set("firstName", "gwen")
    scopes.set("lastName", "register")
    scopes.addScope("person")
    scopes.set("firstName", "gwen")
    scopes.set("lastName", "person")
    scopes.addScope("register")
    scopes.set("firstName", "gwen")
    scopes.set("lastName", "web")
    
    scopes.get("firstName")  should be ("gwen")
    scopes.get("lastName")   should be ("web")
    
    scopes.asString should be (
      """{
        |  scopes {
        |    scope : "register" {
        |      firstName : "gwen"
        |      lastName : "register"
        |    }
        |    scope : "person" {
        |      firstName : "gwen"
        |      lastName : "person"
        |    }
        |    scope : "register" {
        |      lastName : "web"
        |    }
        |  }
        |}""".stripMargin)
                                                                        
  }
  
  forAll (levels) { level =>
  
    "filtering by findEntry" should s"produce expected results in $level scope" in {
    
      withSetting("gwen.state.level", level) {
      
        val scopes = new ScopedDataStack()
      
        scopes.set("middleName", "interpreter")
        scopes.addScope("register")
        scopes.set("firstName", "gwen")
        scopes.set("lastName", "register")
        scopes.addScope("person")
        scopes.set("firstName", "gwen")
        scopes.set("lastName", "person")
        scopes.addScope("register")
        scopes.set("firstName", "gwen")
        scopes.set("lastName", "web")
        
        scopes.getOpt("middleName") should be (Some("interpreter"))
        
        scopes.asString should be (
          s"""{
            |  scopes {
            |    scope : "$level" {
            |      middleName : "interpreter"
            |    }
            |    scope : "register" {
            |      firstName : "gwen"
            |      lastName : "register"
            |    }
            |    scope : "person" {
            |      firstName : "gwen"
            |      lastName : "person"
            |    }
            |    scope : "register" {
            |      lastName : "web"
            |    }
            |  }
            |}""".stripMargin)
        
        val filtered1 = scopes.visible.filterAtts { case (n, _) => n == "firstName" }
        filtered1.getOpt("firstName") should be (Some("gwen"))
        filtered1.getOpt("middleName") should be (None)
        filtered1.findEntry { case (n, _) => n == "firstName" } should be (Some(("firstName", "gwen")))
        filtered1.findEntry { case (n, _) => n == "lastName" } should be (None)
        filtered1.findEntry { case (n, _) => n == "middleName" } should be (None)
        filtered1.asString should be (
          """{
            |  scopes {
            |    scope : "register" {
            |      firstName : "gwen"
            |    }
            |  }
            |}""".stripMargin)
        
        val filtered2 = scopes.visible.filterAtts { case (n, _) => n == "firstName" || n == "middleName" }
        filtered2.getOpt("firstName") should be (Some("gwen"))
        filtered2.getOpt("middleName") should be (Some("interpreter"))
        filtered2.findEntry { case (n, _) => n == "firstName" } should be (Some(("firstName", "gwen")))
        filtered2.findEntry { case (n, _) => n == "lastName" } should be (None)
        filtered2.findEntry { case (n, _) => n == "middleName" } should be (Some(("middleName", "interpreter")))
        filtered2.asString should be (
          s"""{
            |  scopes {
            |    scope : "$level" {
            |      middleName : "interpreter"
            |    }
            |    scope : "register" {
            |      firstName : "gwen"
            |    }
            |  }
            |}""".stripMargin)
                                                                            
      }
    }
  }

  forAll (levels) { level =>

    "filtering by findEntries" should s"produce expected results in $level scope" in {

      withSetting("gwen.state.level", level) {

        val scopes = new ScopedDataStack()
        scopes.set("middleName", "interpreter")
        scopes.addScope("register")
        scopes.set("firstName", "gwen")
        scopes.set("lastName", "register")
        scopes.addScope("person")
        scopes.set("firstName", "gwen")
        scopes.set("lastName", "person")
        scopes.addScope("register")
        scopes.set("firstName", "gwen")
        scopes.set("lastName", "web")

        scopes.getOpt("middleName") should be (Some("interpreter"))

        scopes.asString should be (
          s"""{
            |  scopes {
            |    scope : "$level" {
            |      middleName : "interpreter"
            |    }
            |    scope : "register" {
            |      firstName : "gwen"
            |      lastName : "register"
            |    }
            |    scope : "person" {
            |      firstName : "gwen"
            |      lastName : "person"
            |    }
            |    scope : "register" {
            |      lastName : "web"
            |    }
            |  }
            |}""".stripMargin)

        val filtered1 = scopes.visible.filterAtts { case (n, _) => n.endsWith("Name")}
        filtered1.getOpt("firstName") should be (Some("gwen"))
        filtered1.getOpt("middleName") should be (Some("interpreter"))
        filtered1.getOpt("lastName") should be (Some("web"))
        filtered1.findEntries { case (n, _) => n == "firstName" } should be (List(("firstName", "gwen")))
        filtered1.findEntries { case (n, _) => n == "missing" } should be (Nil)
        filtered1.asString should be (
          s"""{
            |  scopes {
            |    scope : "$level" {
            |      middleName : "interpreter"
            |    }
            |    scope : "register" {
            |      firstName : "gwen"
            |      lastName : "register"
            |    }
            |    scope : "register" {
            |      lastName : "web"
            |    }
            |  }
            |}""".stripMargin)

        val filtered2 = scopes.visible.filterAtts { case (n, _) => n == "firstName" || n == "middleName" }
        filtered2.getOpt("firstName") should be (Some("gwen"))
        filtered2.getOpt("middleName") should be (Some("interpreter"))
        filtered2.findEntries { case (n, _) => n == "firstName" } should be (List(("firstName", "gwen")))
        filtered2.findEntries { case (n, _) => n == "lastName" } should be (Nil)
        filtered2.asString should be (
          s"""{
            |  scopes {
            |    scope : "$level" {
            |      middleName : "interpreter"
            |    }
            |    scope : "register" {
            |      firstName : "gwen"
            |    }
            |  }
            |}""".stripMargin)

      }
    }
  }

  "filtering by allEntries" should "produce expected results" in {

    val scopes = new ScopedDataStack()
    scopes.set("middleName", "interpreter")
    scopes.allEntries should be (List(("middleName", "interpreter")))

    scopes.addScope("register")
    scopes.set("firstName", "gwen")
    scopes.set("lastName", "register")
    scopes.allEntries should be (List(("firstName", "gwen"), ("lastName", "register"), ("middleName", "interpreter")))

    scopes.addScope("person")
    scopes.set("firstName", "gwen")
    scopes.set("lastName", "person")
    scopes.allEntries should be (List(("firstName", "gwen"), ("lastName", "person"), ("middleName", "interpreter")))

    scopes.addScope("register")
    scopes.set("firstName", "gwen")
    scopes.set("lastName", "web")
    scopes.allEntries should be (List(("lastName", "web"), ("firstName", "gwen"), ("middleName", "interpreter")))

  }
  
  forAll (levels) { level =>
  
    "new stack with no data" should s"return empty current and $level scopes" in {
    
      withSetting("gwen.state.level", level) {
        
        val scopes = ScopedDataStack(new mutable.ArrayStack[ScopedData]())
        
        scopes.current.scope should be (level)
        scopes.topScope.scope should be (level)
        
        intercept[UnboundAttributeException] { scopes.get("username") }
        intercept[UnboundAttributeException] { scopes.get("password") }
        intercept[UnboundAttributeException] { scopes.get("firstName") }
        intercept[UnboundAttributeException] { scopes.get("lastName") }
        
        scopes.getOpt("username")  should be (None)
        scopes.getOpt("password")  should be (None)
        scopes.getOpt("firstName") should be (None)
        scopes.getOpt("lastName")  should be (None)
        
      }
    }
  }
  
  "get entry Opt" should "return expected results" in {
    
    val scopes = new ScopedDataStack()
    
    scopes.set("middleName", "interpreter")
    scopes.addScope("register")
    scopes.set("firstName", "gwen")
    scopes.set("lastName", "register")
    scopes.addScope("person")
    scopes.set("firstName", "gwen")
    scopes.set("lastName", "person")
    scopes.addScope("register")
    scopes.set("firstName", "gwen")
    scopes.set("lastName", "web")
    
    scopes.findEntry { case (n, _) => n == "firstName" } should be (Some(("firstName", "gwen")))
    scopes.findEntry { case (n, _) => n == "lastName" }  should be (Some(("lastName", "web")))
    scopes.findEntry { case (n, _) => n == "middleName" }  should be (Some(("middleName", "interpreter")))
    scopes.findEntry { case (n, v) => n == "middleName" && v == "interpreter" }  should be (Some(("middleName", "interpreter")))
    scopes.findEntry { case (n, v) => n == "middleName" && v == "gwen" }  should be (None)
    scopes.findEntry { case (n, v) => v == "person" }  should be (None)
    scopes.findEntry { case (n, _) => n == "surname" }  should be (None)
    
    scopes.addScope("person")
    
    scopes.findEntry { case (n, _) => n == "firstName" }  should be (Some(("firstName", "gwen")))
    scopes.findEntry { case (n, _) => n == "lastName" }  should be (Some(("lastName", "person")))
    scopes.findEntry { case (n, _) => n == "middleName" }  should be (Some(("middleName", "interpreter")))
    scopes.findEntry { case (n, _) => n == "middleName" }  should be (Some(("middleName", "interpreter")))
    scopes.findEntry { case (n, v) => n == "middleName" && v == "gwen" }  should be (None)
    scopes.findEntry { case (n, v) => v == "person" }  should be (Some(("lastName", "person")))
    scopes.findEntry { case (n, _) => n == "surname" }  should be (None)
    
  }

  "scope with a null attribute" should """yield None for getOpt call""" in {
    val scopes = new ScopedDataStack()
    scopes.set("x", null)
    scopes.getOpt("x") should be (None)
  }

  "scope with a null attribute overriding non null attribute" should """yield None for getOpt call""" in {
    val scopes = new ScopedDataStack()
    scopes.set("x", "1")
    scopes.getOpt("x") should be (Some("1"))
    scopes.set("x", null)
    scopes.getOpt("x") should be (None)
  }
  
}