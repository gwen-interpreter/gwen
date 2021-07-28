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
import gwen.core.node.gherkin.GherkinParser
import gwen.core.node.gherkin.Tag

import org.scalatest.matchers.should.Matchers

class TagFilterTest extends BaseTest with Matchers with GherkinParser { 

  private val parse = parseSpec(_: String)
  
  private val featureString = """
      
       @wip
       Feature: Work in progress
      
      Scenario: Work unit 1
          Given I do work 1

      @work
      Scenario: Work unit 2
          Given I do work 2
      
      @work @play @rest
      Scenario: Work unit 3
          Given I do work 3
      
      @wip @play
      Scenario: Work unit 4
          Given I do work 4

      @work
      Scenario Outline: Work unit 5
          Given I do work <name>
        @play
        Examples: play examples
          | name |
          | play |
        @rest
        Examples: rest examples
          | name |
          | rest |"""
    
  "No tags" should "return same feature" in {
    val source = parse(featureString).get
    val tagFilter = new TagFilter(Nil)
    tagFilter.filter(source) match {
      case Some(target) => source should be (target)
      case None => fail("same feature expected")
    }
  }
  
  "Include feature level tag" should "return same feature" in {
    val source = parse(featureString).get
    val tagFilter = new TagFilter(List((Tag("@wip"), true)))
    tagFilter.filter(source) match {
      case Some(target) => source should be (target)
      case None => fail("same feature expected")
    }
  }
  
  "Exclude feature level tag" should "return no feature" in {
    val source = parse(featureString).get
    val tagFilter = new TagFilter(List((Tag("@wip"), false)))
    tagFilter.filter(source) match {
      case Some(target) => fail("None expected")
      case None => // success
    }
  }
  
  "Include scenario level tag" should "return scenarios with only those tagged scenarios" in {
    val source = parse(featureString).get
    val tagFilter = new TagFilter(List((Tag("@work"), true)))
    tagFilter.filter(source) match {
      case Some(target) =>
        val scenarios = target.scenarios
        scenarios.size should be (3)
        scenarios(0).name should be ("Work unit 2")
        scenarios(1).name should be ("Work unit 3")
        scenarios(2).name should be ("Work unit 5")
        scenarios(2).examples.size should be (2)
        scenarios(2).examples(0).name should be ("play examples")
        scenarios(2).examples(1).name should be ("rest examples")
        
      case None => fail("feature expected")
    }
  }
  
  "Exclude scenario level tag" should "return scenarios without those tagged scenarios" in {
    val source = parse(featureString).get
    val tagFilter = new TagFilter(List((Tag("@work"), false)))
    tagFilter.filter(source) match {
      case Some(target) =>
        val scenarios = target.scenarios
        scenarios.size should be (2)
        scenarios(0).name should be ("Work unit 1")
        scenarios(1).name should be ("Work unit 4")
        
      case None => fail("feature expected")
    }
  }
  
  "Include and exclude scenario level tags" should "return scenarios with include tag minus exclude tag" in {
    val source = parse(featureString).get
    val tagFilter = new TagFilter(List((Tag("@play"), true), (Tag("@work"), false)))
    tagFilter.filter(source) match {
      case Some(target) =>
        val scenarios = target.scenarios
        scenarios.size should be (1)
        scenarios(0).name should be ("Work unit 4")
        
      case None => fail("feature expected")
    }
  }
  
  "Exclude and include scenario level tags" should "return scenarios without exclude tag plus include tag" in {
    val source = parse(featureString).get
    val tagFilter = new TagFilter(List((Tag("@play"), false), (Tag("@work"), true)))
    tagFilter.filter(source) match {
      case Some(target) =>
        val scenarios = target.scenarios
        scenarios.size should be (2)
        scenarios(0).name should be ("Work unit 2")
        scenarios(1).name should be ("Work unit 5")
        scenarios(1).examples.size should be (1)
        scenarios(1).examples(0).name should be ("rest examples")
        
      case None => fail("feature expected")
    }
  }
  
  "Include and exclude of same scenario level tag" should "return no feature" in {
    val source = parse(featureString).get
    val tagFilter = new TagFilter(List((Tag("@play"), true), (Tag("@play"), false)))
    tagFilter.filter(source) match {
      case Some(_) => fail("None expected")
      case None => // success
    }
  }
  
  "Either of two scenario level include tags" should "return the feature" in {
    val source = parse(featureString).get
    val tagFilter = new TagFilter(List((Tag("@work"), true), (Tag("@play"), true)))
    tagFilter.filter(source) match {
      case Some(target) => 
        val scenarios = target.scenarios
        scenarios.size should be (4)
        scenarios(0).name should be ("Work unit 2")
        scenarios(1).name should be ("Work unit 3")
        scenarios(2).name should be ("Work unit 4")
        scenarios(3).name should be ("Work unit 5")
        scenarios(3).examples.size should be (2)
        scenarios(3).examples(0).name should be ("play examples")
        scenarios(3).examples(1).name should be ("rest examples")
      case None => fail("feature expected")
    }
  }
  
  "Either of two scenario level include tags but not exclude tag" should "return the feature" in {
    val source = parse(featureString).get
    val tagFilter = new TagFilter(List((Tag("@work"), true), (Tag("@rest"), false), (Tag("@play"), true)))
    tagFilter.filter(source) match {
      case Some(target) => 
        val scenarios = target.scenarios
        scenarios.size should be (3)
        scenarios(0).name should be ("Work unit 2")
        scenarios(1).name should be ("Work unit 4")
        scenarios(2).name should be ("Work unit 5")
        scenarios(2).examples.size should be (1)
        scenarios(2).examples(0).name should be ("play examples")
      case None => fail("feature expected")
    }
  }
  
  "Exclude and include of same scenario level tag" should "return no feature" in {
    val source = parse(featureString).get
    val tagFilter = new TagFilter(List((Tag("@play"), false), (Tag("@play"), true)))
    tagFilter.filter(source) match {
      case Some(_) => fail("None expected")
      case None => // success
    }
  }
  
  "@Ignore tag" should "be excluded by default" in {
    val source = parse(featureString + """
      @Ignore
      Scenario: Work unit 6
          Given I do work 6""").get
    val tagFilter = new TagFilter(Nil)
    tagFilter.filter(source) match {
      case Some(target) =>
        val scenarios = target.scenarios
        scenarios.size should be (5)
        scenarios(0).name should be ("Work unit 1")
        scenarios(1).name should be ("Work unit 2")
        scenarios(2).name should be ("Work unit 3")
        scenarios(3).name should be ("Work unit 4")
        scenarios(4).name should be ("Work unit 5")
        scenarios(4).examples.size should be (2)
        scenarios(4).examples(0).name should be ("play examples")
        scenarios(4).examples(1).name should be ("rest examples")
        
      case None => fail("feature expected")
    }
  }

  "Include @play scenario and example level tags" should "return scenarios and examples with those tags" in {
    val source = parse(featureString).get
    val tagFilter = new TagFilter(List((Tag("@play"), true)))
    tagFilter.filter(source) match {
      case Some(target) =>
        val scenarios = target.scenarios
        scenarios.size should be (3)
        scenarios(0).name should be ("Work unit 3")
        scenarios(1).name should be ("Work unit 4")
        scenarios(2).name should be ("Work unit 5")
        scenarios(2).examples.size should be (1)
        scenarios(2).examples(0).name should be ("play examples")

      case None => fail("feature expected")
    }
  }

  "Exclude @rest and @play scenario and example level tags" should "return scenarios and examples without those tags" in {
    val source = parse(featureString).get
    val tagFilter = new TagFilter(List((Tag("@play"), false), (Tag("@rest"), false)))
    tagFilter.filter(source) match {
      case Some(target) =>
        val scenarios = target.scenarios
        scenarios.size should be (2)
        scenarios(0).name should be ("Work unit 1")
        scenarios(1).name should be ("Work unit 2")

      case None => fail("feature expected")
    }
  }

  "Include @work and exclude @rest and @play scenario and example level tags" should "return scenarios and examples that match" in {
    val source = parse(featureString).get
    val tagFilter = new TagFilter(List((Tag("@work"), true), (Tag("@play"), false), (Tag("@rest"), false)))
    tagFilter.filter(source) match {
      case Some(target) =>
        val scenarios = target.scenarios
        scenarios.size should be (1)
        scenarios(0).name should be ("Work unit 2")

      case None => fail("feature expected")
    }
  }

  "Include @work and exclude @rest scenario and example level tags" should "return scenarios and examples that match" in {
    val source = parse(featureString).get
    val tagFilter = new TagFilter(List((Tag("@work"), true), (Tag("@rest"), false)))
    tagFilter.filter(source) match {
      case Some(target) =>
        val scenarios = target.scenarios
        scenarios.size should be (2)
        scenarios(0).name should be ("Work unit 2")
        scenarios(1).name should be ("Work unit 5")
        scenarios(1).examples.size should be (1)
        scenarios(1).examples(0).name should be ("play examples")

      case None => fail("feature expected")
    }
  }

  "Include @work and exclude @play scenario and example level tags" should "return scenarios and examples that match" in {
    val source = parse(featureString).get
    val tagFilter = new TagFilter(List((Tag("@work"), true), (Tag("@play"), false)))
    tagFilter.filter(source) match {
      case Some(target) =>
        val scenarios = target.scenarios
        scenarios.size should be (2)
        scenarios(0).name should be ("Work unit 2")
        scenarios(1).name should be ("Work unit 5")
        scenarios(1).examples.size should be (1)
        scenarios(1).examples(0).name should be ("rest examples")

      case None => fail("feature expected")
    }
  }

  "Include @rest scenario and example level tags" should "return scenarios and examples that match" in {
    val source = parse(featureString).get
    val tagFilter = new TagFilter(List((Tag("@rest"), true)))
    tagFilter.filter(source) match {
      case Some(target) =>
        val scenarios = target.scenarios
        scenarios.size should be (2)
        scenarios(0).name should be ("Work unit 3")
        scenarios(1).name should be ("Work unit 5")
        scenarios(1).examples.size should be (1)
        scenarios(1).examples(0).name should be ("rest examples")

      case None => fail("feature expected")
    }
  }

  "Include @play scenario and example level tags" should "return scenarios and examples that match" in {
    val source = parse(featureString).get
    val tagFilter = new TagFilter(List((Tag("@play"), true)))
    tagFilter.filter(source) match {
      case Some(target) =>
        val scenarios = target.scenarios
        scenarios.size should be (3)
        scenarios(0).name should be ("Work unit 3")
        scenarios(1).name should be ("Work unit 4")
        scenarios(2).name should be ("Work unit 5")
        scenarios(2).examples.size should be (1)
        scenarios(2).examples(0).name should be ("play examples")

      case None => fail("feature expected")
    }
  }
  
}