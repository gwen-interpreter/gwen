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

package gwen.core.node.gherkin

import gwen.core.BaseTest
import gwen.core.TestModel

import scala.util.Failure

import org.scalatest.matchers.should.Matchers

class StepTest extends BaseTest with Matchers with GherkinParser with TestModel {

  private val parse = parseStep(_: String)
  
  "Valid steps" should "parse" in {
    
    StepKeyword.values.map(_.toString) foreach { keyword =>
      parse(s"$keyword I am a regular test step").get                               should be (Step(keyword, "I am a regular test step"))
      parse(s"""$keyword I contain a double quoted "literal"""").get                should be (Step(keyword, """I contain a double quoted "literal""""))
      parse(s"$keyword I contain a single quoted 'literal'").get                    should be (Step(keyword, "I contain a single quoted 'literal'"))
      parse(s" $keyword I contain a leading space").get                             should be (Step(keyword, "I contain a leading space"))
      parse(s"$keyword $keyword").get                                               should be (Step(keyword, s"$keyword"))
      parse(s"$keyword I contain the $keyword clause literal").get                  should be (Step(keyword, s"I contain the $keyword clause literal"))
      parse(s"$keyword I contain an embedded\ttab").get                             should be (Step(keyword, "I contain an embedded\ttab"))
      parse(s"$keyword I contain a trailing tab\t").get                             should be (Step(keyword, "I contain a trailing tab"))
      parse(s"$keyword I contain a trailing space ").get                            should be (Step(keyword, "I contain a trailing space"))
      parse(s"$keyword I contain an embedded double  space").get                    should be (Step(keyword, "I contain an embedded double  space"))
      parse(s"$keyword I contain an embedded double  space and triple   space").get should be (Step(keyword, "I contain an embedded double  space and triple   space"))
      parse(s"""$keyword INFO: "Testing"""").get                                    should be (Step(keyword, """INFO: "Testing""""))
    }
    
  }
  
  "Invalid steps" should "not parse" in {
    
    StepKeyword.values.map(_.toString) foreach { keyword =>
      
      assertFail(s"$keyword",   "Syntax error [at line 1]: 'Given|When|Then|And|But <expression>' expected")
      assertFail(s"$keyword ",  "Syntax error [at line 1]: 'Given|When|Then|And|But <expression>' expected")
      assertFail(s"$keyword\t", "Syntax error [at line 1]: 'Given|When|Then|And|But <expression>' expected")
      assertFail(s"$keyword\n", "Syntax error [at line 1]: 'Given|When|Then|And|But <expression>' expected")
      
      assertFail(s"I do not start with the $keyword clause", "Syntax error [at line 1]: 'Given|When|Then|And|But <expression>' expected")
    }
    
  }
  
  "invalid keywords" should "not parse" in {
    
    assertFail("?",        "Syntax error [at line 1]: 'Given|When|Then|And|But <expression>' expected")
    assertFail("^C",       "Syntax error [at line 1]: 'Given|When|Then|And|But <expression>' expected")
    assertFail("^Q",       "Syntax error [at line 1]: 'Given|When|Then|And|But <expression>' expected")
    assertFail("{)()ASD}", "Syntax error [at line 1]: 'Given|When|Then|And|But <expression>' expected")
    assertFail(";",        "Syntax error [at line 1]: 'Given|When|Then|And|But <expression>' expected")
    assertFail("\\s",      "Syntax error [at line 1]: 'Given|When|Then|And|But <expression>' expected")
    assertFail("/n",       "Syntax error [at line 1]: 'Given|When|Then|And|But <expression>' expected")
    assertFail("(?:.+)?",  "Syntax error [at line 1]: 'Given|When|Then|And|But <expression>' expected")
    assertFail("''",       "Syntax error [at line 1]: 'Given|When|Then|And|But <expression>' expected")
  }

  "Step with valid data table " should "parse" in {

    val stepString =
      """
        |Then the word should match the number
        |       | one   | 1 |
        |       | two   | 2 |
        |       | three | 3 |
      """.stripMargin

    val step = parse(stepString).get

    step.keyword should be (StepKeyword.Then.toString)
    step.name should be ("the word should match the number")
    step.table should be (
      // offset line numbers by implicit feature+scenario add by step string parser
      List(
        (5, List("one", "1")),
        (6, List("two", "2")),
        (7, List("three", "3"))
      )
    )
  }

  "Step with invalid data table " should "should fail" in {

    val stepString =
      """
        |Then the word should match the number
        |       | one   | 1 |
        |       | two   |
        |       | three | 3 |
      """.stripMargin

    assertFail(stepString, "Syntax error [at line 1]: 'Given|When|Then|And|But <expression>' expected: Parser errors:\n(6:8): inconsistent cell count within the table")

  }

  "Step with docString and no content type" should "parse" in {

    val stepString =
      s"""
        |Given the following lines
        |  ${"\"\"\""}
        |  Line 1
        |  Line 2
        |  Line 3
        |  ${"\"\"\""}""".stripMargin

    val step = parse(stepString).get

    step.keyword should be (StepKeyword.Given.toString)
    step.name should be ("the following lines")
    step.table should be (Nil)
    step.docString.nonEmpty should be (true)
    step.docString.foreach { case (line, content, contentType) =>
      line > 0 should be (true)
      content should be (
        """Line 1
          |Line 2
          |Line 3""".stripMargin)
      contentType should be (None)
    }
  }

  "Step with docString and content type" should "parse" in {

    val stepString =
      s"""
        |Given the following lines
        |  ${"\"\"\""}xml
        |  <lines>
        |    <line>Line 1</line>
        |    <line>Line 2</line>
        |    <line>Line 3</line>
        |  </lines>
        |  ${"\"\"\""}""".stripMargin

    val step = parse(stepString).get

    step.keyword should be (StepKeyword.Given.toString)
    step.name should be ("the following lines")
    step.table should be (Nil)
    step.docString.nonEmpty should be (true)
    step.docString.foreach { case (line, content, contentType) =>
      line > 0 should be (true)
      content should be (
        """<lines>
          |  <line>Line 1</line>
          |  <line>Line 2</line>
          |  <line>Line 3</line>
          |</lines>""".stripMargin)
      contentType should be (Some("xml"))
    }
  }
  
  private def assertFail(input: String, expected: String): Unit = {
    parse(input) match {
      case Failure(e) => e.getMessage should be (expected)
      case _ => fail("failure expected")
    }
  }

  "toString on step clone" should "return same value" in {
    val step1 = Step(StepKeyword.Given.toString, "I am a step")
    val step2 = Step(StepKeyword.Given.toString, "I am a step")
    step1.toString should be (step2.toString)
  }

  "uid on step clone" should "return different values" in {
    val step1 = Step(StepKeyword.Given.toString, "I am a step")
    val step2 = Step(StepKeyword.Given.toString, "I am a step")
    step1.uuid should not be (step2.uuid)
  }

  "@Breakpoint on step" should "parse" in {
    val step1 = parse("Given @Breakpoint I am a breakpoint").get
    val step2 = parse("Given @breakpoint I am a breakpoint too").get
    step1.isBreakpoint should be (true)
    step1.isFinally should be (false)
    step1.tags should be (List(Tag("@Breakpoint")))
    step1.name should be ("I am a breakpoint")
    step2.isBreakpoint should be (true)
    step2.isFinally should be (false)
    step2.tags should be (List(Tag("@breakpoint")))
    step2.name should be ("I am a breakpoint too")
  }

  "@Finally on step" should "parse" in {
    val step1 = parse("Given @Finally I am a finally").get
    val step2 = parse("Given @finally I am a finally too").get
    step1.isBreakpoint should be (false)
    step1.isFinally should be (true)
    step1.tags should be (List(Tag("@Finally")))
    step1.name should be ("I am a finally")
    step2.isFinally should be (true)
    step2.isBreakpoint should be (false)
    step2.tags should be (List(Tag("@finally")))
    step2.name should be ("I am a finally too")
  }

  "@Breakpoint and @Finally on step" should "parse" in {
    val step1 = parse("Given @Breakpoint @Finally I am a breakpoint and finally").get
    val step2 = parse("Given @finally  @breakpoint I am a breakpoint and finally too").get
    step1.isBreakpoint should be (true)
    step1.isFinally should be (true)
    step1.tags should be (List(Tag("@Breakpoint"), Tag("@Finally")))
    step1.name should be ("I am a breakpoint and finally")
    step2.isBreakpoint should be (true)
    step2.isFinally should be (true)
    step2.tags should be (List(Tag("@finally"), Tag("@breakpoint")))
    step2.name should be ("I am a breakpoint and finally too")
  }
  
}