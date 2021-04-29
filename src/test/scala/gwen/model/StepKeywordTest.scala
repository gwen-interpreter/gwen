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

package gwen.model

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class StepKeywordTest extends FlatSpec with Matchers {

  "names" should "evaluate correctly" in {
    val names = StepKeyword.names
    names.size should be (5)
    names.contains("Given") should be (true)
    names.contains("When") should be (true)
    names.contains("Then") should be (true)
    names.contains("And") should be (true)
    names.contains("But") should be (true)
  }

  "nameOf" should "evaluate correctly" in {
    StepKeyword.nameOf(StepKeyword.Given) should be ("Given")
    StepKeyword.nameOf(StepKeyword.When) should be ("When")
    StepKeyword.nameOf(StepKeyword.Then) should be ("Then")
    StepKeyword.nameOf(StepKeyword.And) should be ("And")
    StepKeyword.nameOf(StepKeyword.But) should be ("But")
  }

  "valueOf" should "evaluate correctly" in {
    StepKeyword.valueOf("Given") should be (StepKeyword.Given)
    StepKeyword.valueOf("When") should be (StepKeyword.When)
    StepKeyword.valueOf("Then") should be (StepKeyword.Then)
    StepKeyword.valueOf("And") should be (StepKeyword.And)
    StepKeyword.valueOf("But") should be (StepKeyword.But)
    StepKeyword.valueOf("*") should be (StepKeyword.And)
  }

  "isGiven" should "evaluate correctly" in {
    StepKeyword.isGiven("Given") should be (true)
    StepKeyword.isGiven("When") should be (false)
    StepKeyword.isGiven("Then") should be (false)
    StepKeyword.isGiven("And") should be (false)
    StepKeyword.isGiven("But") should be (false)
    StepKeyword.isGiven("*") should be (false)
  }

  "isWhen" should "evaluate correctly" in {
    StepKeyword.isWhen("Given") should be (false)
    StepKeyword.isWhen("When") should be (true)
    StepKeyword.isWhen("Then") should be (false)
    StepKeyword.isWhen("And") should be (false)
    StepKeyword.isWhen("But") should be (false)
    StepKeyword.isWhen("*") should be (false)
  }

  "isThen" should "evaluate correctly" in {
    StepKeyword.isThen("Given") should be (false)
    StepKeyword.isThen("When") should be (false)
    StepKeyword.isThen("Then") should be (true)
    StepKeyword.isThen("And") should be (false)
    StepKeyword.isThen("But") should be (false)
    StepKeyword.isThen("*") should be (false)
  }

  "isAnd" should "evaluate correctly" in {
    StepKeyword.isAnd("Given") should be (false)
    StepKeyword.isAnd("When") should be (false)
    StepKeyword.isAnd("Then") should be (false)
    StepKeyword.isAnd("And") should be (true)
    StepKeyword.isAnd("But") should be (false)
    StepKeyword.isAnd("*") should be (true)
  }

  "isBut" should "evaluate correctly" in {
    StepKeyword.isBut("Given") should be (false)
    StepKeyword.isBut("When") should be (false)
    StepKeyword.isBut("Then") should be (false)
    StepKeyword.isBut("And") should be (false)
    StepKeyword.isBut("But") should be (true)
    StepKeyword.isBut("*") should be (false)
  }
  
}