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
 
class ScopedDataTest extends BaseTest with Matchers {

  "new scope" should "not contain any attributes" in {
    val scope = ScopedData("login")
    scope.asString(env = false)    should be (
      """scope : "login" { }"""
    )
    scope.asString(env = true)    should be (
      """|env {
         |  scope : "login" { }
         |}""".stripMargin
    )
    scope.getOpt("userId") should be (None)
  }
  
  "scope with a null attribute" should "yield None for getOpt call" in {
    val scope = ScopedData("login").set("userId", null)
    scope.asString(env = false)    should be (
      """|scope : "login" {
         |  userId : null
         |}""".stripMargin)
    scope.asString(env = true)    should be (
      """|env {
         |  scope : "login" {
         |    userId : null
         |  }
         |}""".stripMargin)
    scope.getOpt("userId") should be (None)
  }

  "scope with clear on attribute" should "yield None for getOpt call" in {
    val scope = ScopedData("login").clear("userId")
    scope.asString(env = false)       should be ("""scope : "login" { }""")
    scope.asString(env = true)        should be (
      """|env {
         |  scope : "login" { }
         |}""".stripMargin
    )
    scope.getOpt("userId") should be (None)
  }
  
  "scope with a null attribute" should "throw error for get call" in {
    val scope = ScopedData("login").set("userId", null)
    scope.asString(env = false) should be (
      """|scope : "login" {
         |  userId : null
         |}""".stripMargin)
    scope.asString(env = true) should be (
      """|env {
         |  scope : "login" {
         |    userId : null
         |  }
         |}""".stripMargin)
    intercept[UnboundAttributeException] {
      scope.get("userId")
    }
  }

  "scope after clear on attribute" should "throw error for get call" in {
    val scope = ScopedData("login").clear("userId")
    scope.asString(env = false)    should be ("""scope : "login" { }""")
    scope.asString(env = true)    should be (
      """|env {
         |  scope : "login" { }
         |}""".stripMargin
    )
    intercept[UnboundAttributeException] {
      scope.get("userId")
    }
  }
  
  "scope with one attribute" should "contain only that attribute" in {
    val scope = ScopedData("login").set("userId", "gwen")
    scope.asString(env = false) should be (
      """|scope : "login" {
         |  userId : "gwen"
         |}""".stripMargin)
    scope.asString(env = true) should be (
      """|env {
         |  scope : "login" {
         |    userId : "gwen"
         |  }
         |}""".stripMargin)
    scope.getOpt("userId") should be (Some("gwen"))
    scope.getOpt("UserId") should be (None)
  }
  
  "scope with two attributes" should "contain those two attributes" in {
    val scope = ScopedData("login").set("userId", "gwen").set("password", "pwd")
    scope.asString(env = false) should be (
      """|scope : "login" {
         |  userId : "gwen"
         |  password : "pwd"
         |}""".stripMargin)
    scope.asString(env = true) should be (
      """|env {
         |  scope : "login" {
         |    userId : "gwen"
         |    password : "pwd"
         |  }
         |}""".stripMargin)
    scope.getOpt("userId")   should be (Some("gwen"))
    scope.getOpt("password") should be (Some("pwd"))
    scope.getOpt("UserId")   should be (None)
    scope.getOpt("Password") should be (None)
  }
  
  "get lookup on scope with two same named attributes" should "return the most recently added one" in {
    val scope = ScopedData("register").set("name", "todd").set("name", "gwen")
    scope.asString(env = false) should be (
      """|scope : "register" {
         |  name : "todd"
         |  name : "gwen"
         |}""".stripMargin)
    scope.asString(env = true) should be (
      """|env {
         |  scope : "register" {
         |    name : "todd"
         |    name : "gwen"
         |  }
         |}""".stripMargin)
    scope.getOpt("name") should be (Some("gwen"))
  }
  
  "binding the same name and value" should "should not recreate the binding" in {
    val scope = ScopedData("register").set("name", "gwen").set("name", "gwen")
    scope.asString(env = false)  should be (
      """|scope : "register" {
         |  name : "gwen"
         |}""".stripMargin)
    scope.asString(env = true)  should be (
      """|env {
         |  scope : "register" {
         |    name : "gwen"
         |  }
         |}""".stripMargin)
    scope.getOpt("name") should be (Some("gwen"))
  }
  
  "getAll lookup on scope with two same named attributes" should "return both" in {
    val scope = ScopedData("register").set("name", "todd").set("name", "gwen")
    scope.asString(env = false) should be (
      """|scope : "register" {
         |  name : "todd"
         |  name : "gwen"
         |}""".stripMargin)
    scope.asString(env = true) should be (
      """|env {
         |  scope : "register" {
         |    name : "todd"
         |    name : "gwen"
         |  }
         |}""".stripMargin)
    scope.getAll("name") should be (Seq("todd", "gwen"))
  }

  "scope with a blank attribute" should """yield Some("") for getOpt call""" in {
    val scope = ScopedData("login").set("userId", "")
    scope.asString(env = false) should be (
      """|scope : "login" {
         |  userId : ""
         |}""".stripMargin)
    scope.asString(env = true) should be (
      """|env {
         |  scope : "login" {
         |    userId : ""
         |  }
         |}""".stripMargin)
    scope.getOpt("userId") should be (Some(""))
  }

  "scope with a non blank attribute overridden to blank" should """yield Some("") for getOpt call""" in {
    val scope = ScopedData("login").set("userId", "").set("userId", "gwen").set("userId", "")
    scope.asString(env = false) should be (
      """|scope : "login" {
         |  userId : ""
         |  userId : "gwen"
         |  userId : ""
         |}""".stripMargin)
    scope.asString(env = true) should be (
      """|env {
         |  scope : "login" {
         |    userId : ""
         |    userId : "gwen"
         |    userId : ""
         |  }
         |}""".stripMargin)
    scope.getOpt("userId") should be (Some(""))
  }

  "get" should "throw error when there are no scopes" in {
    
    val scope = new ScopedData("test")
    
    intercept[UnboundAttributeException] { scope.get("username") }
    intercept[UnboundAttributeException] { scope.get("password") }
    intercept[UnboundAttributeException] { scope.get("firstName") }
    intercept[UnboundAttributeException] { scope.get("lastName") }
  }
  
  "getOpt" should "return None when there are no scopes" in {
    
    val scope = new ScopedData("test")
    
    scope.getOpt("username")  should be (None)
    scope.getOpt("password")  should be (None)
    scope.getOpt("firstName") should be (None)
    scope.getOpt("lastName")  should be (None)
  }
  
  "getAll" should "not get any attributes when there are no scopes" in {
    
    val scope = new ScopedData("test")
    
    scope.getAll("username")  should be (Nil)
    scope.getAll("password")  should be (Nil)
    scope.getAll("firstName") should be (Nil)
    scope.getAll("lastName")  should be (Nil)
  }
 
  "scope after clear on attribute" should """yield None for getOpt call""" in {
    val scope = new ScopedData("test")
    scope.clear("x")
    scope.getOpt("x") should be (None)
  }

  "scope after clear on attribute overriding non null attribute" should """yield None for getOpt call""" in {
    val scope = new ScopedData("test")
    scope.set("x", "1")
    scope.getOpt("x") should be (Some("1"))
    scope.clear("x")
    scope.getOpt("x") should be (None)
  }
  
}