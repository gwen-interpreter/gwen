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

import gwen.core.TestModel
import gwen.core.model.StepKeyword

import scala.util.Success

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class BackgroundParserTest extends FlatSpec with Matchers with GherkinParser with TestModel {

  private def parse(input: String) = {
    val background = parseSpec(s"Feature: ftest\n$input").filter(_.background.nonEmpty).map(_.background.get)
    background.map { bg => 
      bg.copy(
        withSourceRef = None,
        withSteps = bg.steps.map { s => 
          s.copy(withSourceRef = None) 
        })
    }
  }
  
  private val step1 = Step(StepKeyword.Given.toString, "I am step 1")
  private val step2 = Step(StepKeyword.Then.toString, "I am not step 1")
  
  private val comment1 = "# I am single line hash comment"
  
  "Valid backgrounds" should "parse" in {
      
      parse("Background:").get   should be (Background("", Nil, Nil))
      parse("Background:\n").get should be (Background("", Nil, Nil))
    
      parse(s"Background: name\n$step1").get      should be (Background("name", Nil, List(step1)))
      parse(s"\tBackground:\tname\n$step1").get   should be (Background("name", Nil, List(step1)))
      
      parse(s"Background: name\nI am a background\n$step1").get      should be (Background("name", List("I am a background"), List(step1)))
      parse(s"\tBackground:\tname\nI am a background\nwith multiple\n\nlines\n$step1").get   should be (Background("name", List("I am a background", "with multiple", "", "lines"), List(step1)))
    
      parse(s"Background: name\n$step1").get should be (Background("name", Nil, List(step1)))
      parse(s"Background:name\n$step1").get  should be (Background("name", Nil, List(step1)))
      
      parse(s"\tBackground:name\n$step1").get     should be (Background("name", Nil, List(step1)))
      parse(s"Background:\tname\n$step1").get     should be (Background("name", Nil, List(step1)))
      parse(s"Background:\tname \n$step1").get    should be (Background("name", Nil, List(step1)))
      
      parse(s"Background: name\n$step1\n$step2").get should be (Background("name", Nil, List(step1,step2)))
      
      parse(s"Background: name\n$step1\n$comment1").get            should be (Background("name", Nil, List(step1)))
      parse(s"Background: name\n$step1\n$step2\n$comment1").get    should be (Background("name", Nil, List(step1,step2)))
      parse(s"Background: name\n$step1\n$comment1\n$step2").get    should be (Background("name", Nil, List(step1,step2)))
      parse(s"Background: name\n$comment1\n$step1\n$step2").get    should be (Background("name", Nil, List(step1,step2)))
      
      parse(s"Background:$step1\n$step2").get    should be (Background(s"$step1", Nil, List(step2)))
      parse(s"Background:$step1\n$step2").get    should be (Background(s"$step1", Nil, List(step2)))
      
      parse("Background: I dont have any steps").get should be (Background("I dont have any steps", Nil, Nil))
      
      StepKeyword.values foreach { clause =>
        parse(s"Background: I contain a $clause keyword in name\n$step1").get should be (Background(s"I contain a $clause keyword in name", Nil, List(step1)))
      }
  }
  
  "Invalid backgrounds" should "not parse" in {

    assertFail(s"Background\t:name\n$step1")
    assertFail(s"\tBackground\t:name\n$step1")
    assertFail("Background")
    assertFail("I am not a valid background")
    assertFail("Background no colon after 'Background:'")
     
  }
  
  private def assertFail(input: String): Unit = {
    parse(input) match {
      case Success(_) => fail("failure expected") 
      case _ => 
    }
  }
  
}