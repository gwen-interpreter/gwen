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

import org.scalatest.prop.TableDrivenPropertyChecks.forAll
import org.scalatest.matchers.should.Matchers

class TopScopeTest extends BaseTest with Matchers {

  forAll (levels) { level =>
    s"new $level scope" should "not contain any attributes" in {
      withSetting("gwen.state.level", level) {
        val scope = new TopScope()
        scope.asString()    should be (s"""scope : "$level" { }""")
        scope.getOpt("userId") should be (None)
      }
    }
  }
  
  forAll (levels) { level =>
    s"$level scope with a null attribute" should "yield None for getOpt call" in {
      withSetting("gwen.state.level", level) {
        val scope = new TopScope().set("userId", null)
        scope.asString()    should be (
          s"""scope : "$level" {
            |  userId : null
            |}""".stripMargin)
        scope.getOpt("userId") should be (None)
      }
    }
  }
  
  forAll (levels) { level =>
    s"$level scope with a null attribute" should "throw error for get call" in {
      withSetting("gwen.state.level", level) {
        val scope = new TopScope().set("userId", null)
        scope.asString()    should be (
          s"""scope : "$level" {
            |  userId : null
            |}""".stripMargin)
        intercept[UnboundAttributeException] {
          scope.get("userId")
        }
      }
    }
  }
  
  forAll (levels) { level =>
    s"$level scope with one attribute" should "contain only that attribute" in {
      withSetting("gwen.state.level", level) {
        val scope = new TopScope().set("userId", "gwen")
        scope.asString()    should be (
          s"""scope : "$level" {
            |  userId : "gwen"
            |}""".stripMargin)
        scope.getOpt("userId") should be (Some("gwen"))
        scope.getOpt("UserId") should be (None)
      }
    }
  }
  
  forAll (levels) { level =>
    s"$level scope with two attributes" should "contain those two attributes" in {
      withSetting("gwen.state.level", level) {
        val scope = new TopScope().set("userId", "gwen").set("password", "pwd")
        scope.asString()      should be (
          s"""scope : "$level" {
            |  userId : "gwen"
            |  password : "pwd"
            |}""".stripMargin)
        scope.getOpt("userId")   should be (Some("gwen"))
        scope.getOpt("password") should be (Some("pwd"))
        scope.getOpt("UserId")   should be (None)
        scope.getOpt("Password") should be (None)
      }
    }
  }
  
  forAll (levels) { level =>
    s"get lookup on $level scope with two same named attributes" should "return the most recently added one" in {
      withSetting("gwen.state.level", level) {
        val scope = new TopScope().set("name", "todd").set("name", "gwen")
        scope.asString()  should be (
          s"""scope : "$level" {
            |  name : "todd"
            |  name : "gwen"
            |}""".stripMargin)
        scope.getOpt("name") should be (Some("gwen"))
      }
    }
  }
  
  forAll (levels) { level =>
    s"binding the same name and value in $level scope" should "should not recreate the binding" in {
      withSetting("gwen.state.level", level) {
        val scope = new TopScope().set("name", "gwen").set("name", "gwen")
        scope.asString()  should be (
          s"""scope : "$level" {
            |  name : "gwen"
            |}""".stripMargin)
        scope.getOpt("name") should be (Some("gwen"))
      }
    }
  }
  
  forAll (levels) { level =>
    s"getAll lookup on $level scope with two same named attributes" should "return both" in {
      withSetting("gwen.state.level", level) {
        val scope = new TopScope().set("name", "todd").set("name", "gwen")
        scope.asString()     should be (
          s"""scope : "$level" {
            |  name : "todd"
            |  name : "gwen"
            |}""".stripMargin)
        scope.getAll("name") should be (Seq("todd", "gwen"))
      }
    }
  }

  forAll (levels) { level =>
    s"$level scope with one function attribute" should "contain only that attribute" in {
      withSetting("gwen.state.level", level) {
        val scope = new TopScope().set("userId") { () => "gwen" }
        scope.asString()    should be (
          s"""scope : "$level" {
            |  userId : "gwen"
            |}""".stripMargin)
        scope.getOpt("userId") should be (Some("gwen"))
        scope.getOpt("UserId") should be (None)
      }
    }
  }

  forAll (levels) { level =>
    s"$level scope with one attribute and one function value" should "contain those two attributes" in {
      withSetting("gwen.state.level", level) {
        val scope = new TopScope()
        scope.set("userId", "gwen")
        scope.set("password") { () => "pwd" }
        scope.asString()      should be (
          s"""scope : "$level" {
            |  userId : "gwen"
            |  password : "pwd"
            |}""".stripMargin)
        scope.getOpt("userId")   should be (Some("gwen"))
        scope.getOpt("password") should be (Some("pwd"))
        scope.getOpt("UserId")   should be (None)
        scope.getOpt("Password") should be (None)
      }
    }
  }

  forAll (levels) { level =>
    s"$level scope with a blank attribute" should """yield Some("") for getOpt call""" in {
      withSetting("gwen.state.level", level) {
        val scope = new TopScope().set("userId", "")
        scope.asString()    should be (
          s"""scope : "$level" {
            |  userId : ""
            |}""".stripMargin)
        scope.getOpt("userId") should be (Some(""))
      }
    }
  }

  forAll (levels) { level =>
    s"$level scope with a non blank attribute overridden to blank" should """yield Some("") for getOpt call""" in {
      withSetting("gwen.state.level", level) {
        val scope = new TopScope().set("userId", "").set("userId", "gwen").set("userId", "")
        scope.asString()    should be (
          s"""scope : "$level" {
            |  userId : ""
            |  userId : "gwen"
            |  userId : ""
            |}""".stripMargin)
        scope.getOpt("userId") should be (Some(""))
      }
    }
  }

}