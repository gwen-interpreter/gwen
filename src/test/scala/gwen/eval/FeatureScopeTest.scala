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

import gwen.errors.UnboundAttributeException
import org.scalatest.{FlatSpec, Matchers}

class FeatureScopeTest extends FlatSpec with Matchers {

  "new scope" should "not contain any attributes" in {
    val scope = new FeatureScope()
    scope.asString()    should be ("""scope : "feature" { }""")
    scope.getOpt("userId") should be (None)
  }
  
  "scope with a null attribute" should "yield None for getOpt call" in {
    val scope = new FeatureScope().set("userId", null)
    scope.asString()    should be (
      """scope : "feature" {
        |  userId : null
        |}""".stripMargin)
    scope.getOpt("userId") should be (None)
  }
  
  "scope with a null attribute" should "throw error for get call" in {
    val scope = new FeatureScope().set("userId", null)
    scope.asString()    should be (
      """scope : "feature" {
        |  userId : null
        |}""".stripMargin)
    intercept[UnboundAttributeException] {
      scope.get("userId")
    }
  }
  
  "scope with one attribute" should "contain only that attribute" in {
    val scope = new FeatureScope().set("userId", "gwen")
    scope.asString()    should be (
      """scope : "feature" {
        |  userId : "gwen"
        |}""".stripMargin)
    scope.getOpt("userId") should be (Some("gwen"))
    scope.getOpt("UserId") should be (None)
  }
  
  "scope with two attributes" should "contain those two attributes" in {
    val scope = new FeatureScope().set("userId", "gwen").set("password", "pwd")
    scope.asString()      should be (
      """scope : "feature" {
        |  userId : "gwen"
        |  password : "pwd"
        |}""".stripMargin)
    scope.getOpt("userId")   should be (Some("gwen"))
    scope.getOpt("password") should be (Some("pwd"))
    scope.getOpt("UserId")   should be (None)
    scope.getOpt("Password") should be (None)
  }
  
  "get lookup on scope with two same named attributes" should "return the most recently added one" in {
    val scope = new FeatureScope().set("name", "todd").set("name", "gwen")
    scope.asString()  should be (
      """scope : "feature" {
        |  name : "todd"
        |  name : "gwen"
        |}""".stripMargin)
    scope.getOpt("name") should be (Some("gwen"))
  }
  
  "binding the same name and value" should "should not recreate the binding" in {
    val scope = new FeatureScope().set("name", "gwen").set("name", "gwen")
    scope.asString()  should be (
      """scope : "feature" {
        |  name : "gwen"
        |}""".stripMargin)
    scope.getOpt("name") should be (Some("gwen"))
  }
  
  "getAll lookup on scope with two same named attributes" should "return both" in {
    val scope = new FeatureScope().set("name", "todd").set("name", "gwen")
    scope.asString()     should be (
      """scope : "feature" {
        |  name : "todd"
        |  name : "gwen"
        |}""".stripMargin)
    scope.getAll("name") should be (Seq("todd", "gwen"))
  }

  "scope with one function attribute" should "contain only that attribute" in {
    val scope = new FeatureScope().set("userId") { () => "gwen" }
    scope.asString()    should be (
      """scope : "feature" {
        |  userId : "gwen"
        |}""".stripMargin)
    scope.getOpt("userId") should be (Some("gwen"))
    scope.getOpt("UserId") should be (None)
  }

  "scope with one attribute and one function value" should "contain those two attributes" in {
    val scope = new FeatureScope()
    scope.set("userId", "gwen")
    scope.set("password") { () => "pwd" }
    scope.asString()      should be (
      """scope : "feature" {
        |  userId : "gwen"
        |  password : "pwd"
        |}""".stripMargin)
    scope.getOpt("userId")   should be (Some("gwen"))
    scope.getOpt("password") should be (Some("pwd"))
    scope.getOpt("UserId")   should be (None)
    scope.getOpt("Password") should be (None)
  }

  "scope with a blank attribute" should """yield Some("") for getOpt call""" in {
    val scope = new FeatureScope().set("userId", "")
    scope.asString()    should be (
      """scope : "feature" {
        |  userId : ""
        |}""".stripMargin)
    scope.getOpt("userId") should be (Some(""))
  }

  "scope with a non blank attribute overridden to blank" should """yield Some("") for getOpt call""" in {
    val scope = new FeatureScope().set("userId", "").set("userId", "gwen").set("userId", "")
    scope.asString()    should be (
      """scope : "feature" {
        |  userId : ""
        |  userId : "gwen"
        |  userId : ""
        |}""".stripMargin)
    scope.getOpt("userId") should be (Some(""))
  }

}