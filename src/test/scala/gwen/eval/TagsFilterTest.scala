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

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import gwen.dsl.SpecParser
import gwen.dsl.Tag.string2Tag

class TagsFilterTest extends FlatSpec with ShouldMatchers with SpecParser { 

  private val parse = parseAll(spec, _: String);
  
  private val featureString = """
      
       @wip
       Feature: Work in progress
      
      Scenario: Work unit 1
          Given I do work 1
      
      @work
      Scenario: Work unit 2
          Given I do work 2
      
      @work @play
      Scenario: Work unit 3
          Given I do work 3
      
      @wip @play
      Scenario: Work unit 4
          Given I do work 4"""
    
  "No tags" should "return same feature" in {
	  val source = parse(featureString).get
	  TagsFilter.filter(source, Nil) match {
	    case Some(target) => source should be (target)
	    case None => fail("same feature expected")
	  }
  }
  
  "Include feature level tag" should "return same feature" in {
	  val source = parse(featureString).get
	  TagsFilter.filter(source, List(("@wip", true))) match {
	    case Some(target) => source should be (target)
	    case None => fail("same feature expected")
	  }
  }
  
  "Exclude feature level tag" should "return no feature" in {
    val source = parse(featureString).get
	  TagsFilter.filter(source, List(("@wip", false))) match {
	    case Some(target) => fail("None expected")
	    case None => // success
	  }
  }
  
  "Include scenario level tag" should "return scenarios with only those tagged scenarios" in {
    val source = parse(featureString).get
	  TagsFilter.filter(source, List(("@work", true))) match {
	    case Some(target) =>
	      val scenarios = target.scenarios
	      scenarios.size should be (2)
	      scenarios(0).name should be ("Work unit 2")
	      scenarios(1).name should be ("Work unit 3")
	      
	    case None => fail("feature expected")
	  }
  }
  
  "Exclude scenario level tag" should "return scenarios without those tagged scenarios" in {
    val source = parse(featureString).get
	  TagsFilter.filter(source, List(("@work", false))) match {
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
	  TagsFilter.filter(source, List(("@play", true), ("@work", false))) match {
	    case Some(target) =>
	      val scenarios = target.scenarios
	      scenarios.size should be (1)
	      scenarios(0).name should be ("Work unit 4")
	      
	    case None => fail("feature expected")
	  }
  }
  
  "Exclude and include scenario level tags" should "return scenarios without exclude tag plus include tag" in {
    val source = parse(featureString).get
	  TagsFilter.filter(source, List(("@play", false), ("@work", true))) match {
	    case Some(target) =>
	      val scenarios = target.scenarios
	      scenarios.size should be (1)
	      scenarios(0).name should be ("Work unit 2")
	      
	    case None => fail("feature expected")
	  }
  }
  
  "Include and exclude of same scenario level tag" should "return no feature" in {
    val source = parse(featureString).get
	  TagsFilter.filter(source, List(("@play", true), ("@play", false))) match {
	    case Some(target) => fail("None expected")
	    case None => // success
	  }
  }
  
  "Exclude and include of same scenario level tag" should "return no feature" in {
    val source = parse(featureString).get
	  TagsFilter.filter(source, List(("@play", false), ("@play", true))) match {
	    case Some(target) => fail("None expected")
	    case None => // success
	  }
  }
  
}