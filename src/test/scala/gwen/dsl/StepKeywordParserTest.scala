/*
 * Copyright 2014-2015 Branko Juric, Brady Wood
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

package gwen.dsl

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class StepKeywordParserTest extends FlatSpec with Matchers with SpecParser {

  private val parse = parseAll(keyword, _: String);
  
  "Valid step keywords" should "parse" in {
    StepKeyword.values foreach { keyword =>
      parse(keyword.toString).get should be (keyword)
      parse(s"\t$keyword").get    should be (keyword)
      parse(s" $keyword").get     should be (keyword)
      parse(s"$keyword ").get     should be (keyword)
      parse(s"$keyword\t").get    should be (keyword)
      parse(s"$keyword\n").get    should be (keyword)
    }
  }
  
  "Invalid step keywords" should "not parse" in {
    
    assertFail("NonStepKeyword")
    
    StepKeyword.values foreach { keyword =>
      assertFail(keyword.toString.toLowerCase())
      assertFail(keyword.toString.toUpperCase())
      assertFail(keyword + "x")
      assertFail("x" + keyword)
      
    }
    
  }
  
  private def assertFail(input: String) {
    parse(input) match {
      case f: Failure => f.msg should be (s"'Given|When|Then|And|But' expected")
      case _ => fail("failure expected")
    }
  }
  
}