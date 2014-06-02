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

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class BackgroundParserTest extends FlatSpec with Matchers with SpecParser {

  private val parse = parseAll(background, _: String);
  
  private val step1 = Step(StepKeyword.Given, "I am step 1")
  private val step2 = Step(StepKeyword.Then, "I am not step 1")
  
  private val comment1 = "# I am single line hash comment"
  private val comment2 = "// I am single line double slash comment"
  private val comment3 = "/* I am a\nmulti line\ncomment */"
  
  "Valid backgrounds" should "parse" in {
      
      parse("Background:").get   should be (Background("", Nil))
      parse("Background:\n").get should be (Background("", Nil))
      
      parse(s"Background :name\n$step1").get      should be (Background("name", List(step1)))
      parse(s"Background\t:name\n$step1").get     should be (Background("name", List(step1)))
      parse(s"\tBackground\t:name\n$step1").get   should be (Background("name", List(step1)))
      parse(s"\tBackground\t:\tname\n$step1").get should be (Background("name", List(step1)))
    
      parse(s"Background: name\n$step1").get should be (Background("name", List(step1)))
      parse(s"Background:name\n$step1").get  should be (Background("name", List(step1)))
      
      parse(s"\tBackground:name\n$step1").get     should be (Background("name", List(step1)))
      parse(s"Background:\tname\n$step1").get     should be (Background("name", List(step1)))
      parse(s"Background:\tname\t\n$step1").get   should be (Background("name", List(step1)))
      parse(s"Background:\tname \n$step1").get    should be (Background("name", List(step1)))
      parse(s"Background:\tname\t \n$step1").get  should be (Background("name", List(step1)))
      
      parse(s"Background: name\n$step1\n$step2").get should be (Background("name", List(step1, step2)))
      
      parse(s"Background: name\n$step1\n$comment1").get            should be (Background("name", List(step1)))
      parse(s"Background: name\n$step1\n$step2\n$comment1").get    should be (Background("name", List(step1, step2)))
      parse(s"Background: name\n$step1\n$comment1\n$step2").get    should be (Background("name", List(step1, step2)))
      parse(s"Background: name\n$comment1\n$step1\n$step2").get    should be (Background("name", List(step1, step2)))
      parse(s"Background: name\n$comment1\n$step1\n$comment2").get should be (Background("name", List(step1)))
      
      parse(s"Background: name\n$step1\n$comment3").get            should be (Background("name", List(step1)))
      parse(s"Background: name\n$step1\n$step2\n$comment3").get    should be (Background("name", List(step1, step2)))
      parse(s"Background: name\n$step1\n$comment3\n$step2").get    should be (Background("name", List(step1, step2)))
      parse(s"Background: name\n$comment3\n$step1\n$step2").get    should be (Background("name", List(step1, step2)))
      parse(s"Background: name\n$comment3\n$step1\n$comment3").get should be (Background("name", List(step1)))
      
      parse(s"Background: name\n$comment1\n$step1\n$comment3").get should be (Background("name", List(step1)))
      parse(s"Background: name\n$comment3\n$step1\n$comment1").get should be (Background("name", List(step1)))
      
      parse(s"Background:\n$step1\n$step2").get    should be (Background(s"$step1", List(step2)))
      parse(s"Background: \n$step1\n$step2").get   should be (Background(s"$step1", List(step2)))
      parse(s"Background:\t\n$step1\n$step2").get  should be (Background(s"$step1", List(step2)))
      parse(s"Background:\t \n$step1\n$step2").get should be (Background(s"$step1", List(step2)))
      parse(s"Background: \t\n$step1\n$step2").get should be (Background(s"$step1", List(step2)))
      
      parse("Background: I dont have any steps").get should be (Background("I dont have any steps", Nil))
      
      parse(s"Background: All my steps are commented out\n$comment1\n$comment2\n$comment3").get should be (Background("All my steps are commented out", Nil))
      
      StepKeyword.values foreach { clause =>
        parse(s"Background: I contain a $clause keyword in name\n$step1").get should be (Background(s"I contain a $clause keyword in name", List(step1)))
      }
  }
  
  "Invalid backgrounds" should "not parse" in {

    assertFail("Background",                                 "`:' expected but end of source found")
    assertFail("I am not a valid background",                "'Background|Scenario' expected")
    assertFail("Background no colon after 'Background:'",    "`:' expected but `n' found")
     
  }
  
  private def assertFail(input: String, expected: String) {
    parse(input) match {
      case f: Failure => f.msg should be (expected)
      case _ => fail("failure expected")
    }
  }
  
}