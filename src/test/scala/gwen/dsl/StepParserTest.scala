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

package gwen.dsl

import org.scalatest.FunSuite
import org.scalatest.Matchers
import org.scalatest.FlatSpec

class StepParserTest extends FlatSpec with Matchers with SpecParser {

  private val parse = parseAll(step, _: String);
  
  "Valid steps" should "parse" in {
    
    StepKeyword.values foreach { keyword =>
      parse(s"$keyword I am a regular test step").get                               should be (Step(keyword, "I am a regular test step"))
      parse(s"""$keyword I contain a double qutoed "literal"""").get                should be (Step(keyword, """I contain a double qutoed "literal""""))
      parse(s"$keyword I contain a single quoted 'literal'").get                    should be (Step(keyword, "I contain a single quoted 'literal'"))
      parse(s" $keyword I contain a leading space").get                             should be (Step(keyword, "I contain a leading space"))
      parse(s" \t$keyword I contain a leading space and a tab").get                 should be (Step(keyword, "I contain a leading space and a tab"))
      parse(s"$keyword $keyword").get                                               should be (Step(keyword, s"$keyword"))
      parse(s"$keyword I contain the $keyword clause literal").get                  should be (Step(keyword, s"I contain the $keyword clause literal"))
      parse(s"$keyword I contain an embedded\ttab").get                             should be (Step(keyword, "I contain an embedded\ttab"))
      parse(s"$keyword I contain a trailing tab\t").get                             should be (Step(keyword, "I contain a trailing tab"))
      parse(s"$keyword I contain a trailing space ").get                            should be (Step(keyword, "I contain a trailing space"))
      parse(s"$keyword I contain an embedded double  space").get                    should be (Step(keyword, "I contain an embedded double  space"))
      parse(s"$keyword I contain an embedded double  space and triple   space").get should be (Step(keyword, "I contain an embedded double  space and triple   space"))
    }
    
  }
  
  "Invalid steps" should "not parse" in {
    
    StepKeyword.values foreach { keyword =>
      
      assertFail(s"$keyword",   "incomplete step expression")
      assertFail(s"$keyword ",  "incomplete step expression")
      assertFail(s"$keyword\t", "incomplete step expression")
      assertFail(s"$keyword\n", "incomplete step expression")
      
      assertFail(s"I do not start with the $keyword clause", "'Given|When|Then|And|But' expected")
    }
    
  }
  
  "invalid keywords" should "not parse" in {
    
    assertFail("?",        "'Given|When|Then|And|But' expected")
    assertFail("^C",       "'Given|When|Then|And|But' expected")
    assertFail("^Q",       "'Given|When|Then|And|But' expected")
    assertFail("{)()ASD}", "'Given|When|Then|And|But' expected")
    assertFail(";",        "'Given|When|Then|And|But' expected")
    assertFail("\\s",      "'Given|When|Then|And|But' expected")
    assertFail("/n",       "'Given|When|Then|And|But' expected")
    assertFail("(?:.+)?",  "'Given|When|Then|And|But' expected")
    assertFail("''",       "'Given|When|Then|And|But' expected")
  }
  
  private def assertFail(input: String, expected: String) {
    parse(input) match {
      case f: Failure => f.msg should be (expected)
      case _ => fail("failure expected")
    }
  }
  
}