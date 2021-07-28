/*
 * Copyright 2014-2021 Branko Juric, Brady Wood
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

package gwen.core.node.gherkin

import gwen.core.BaseTest

import org.scalatest.matchers.should.Matchers

import scala.util.Success

class StepKeywordParserTest extends BaseTest with Matchers with GherkinParser {

  private val parse = parseStep(_: String).map(_.keyword)
  
  "Valid step keywords" should "parse" in {
    StepKeyword.values.map(_.toString) foreach { keyword =>
      parse(s"$keyword I am step 1").get      should be (keyword)
      parse(s"\t$keyword I am step 2").get    should be (keyword)
      parse(s" $keyword I am step 3").get     should be (keyword)
      parse(s"$keyword I am step 4").get     should be (keyword)
    }
  }
  
  "Invalid step keywords" should "not parse" in {
    
    assertFail("NonStepKeyword")
    
    StepKeyword.values.map(_.toString) foreach { keyword =>
      assertFail(keyword.toString.toLowerCase())
      assertFail(keyword.toString.toUpperCase())
      assertFail(keyword + "x")
      assertFail("x" + keyword)
      assertFail(s"$keyword\tI am step x1")
      assertFail(s"$keyword\nI am step x2")
      
    }
    
  }
  
  private def assertFail(input: String): Unit = {
    parse(input) match {
      case Success(_) => fail("failure expected")
      case _ => 
    }
  }
  
}