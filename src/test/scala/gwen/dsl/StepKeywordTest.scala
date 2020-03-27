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

class StepKeywordTest extends FlatSpec with Matchers {

   "names" should "evaluate" in {
    val names = StepKeyword.names
    names.size should be (5)
    names.contains("Given") should be (true)
    names.contains("When") should be (true)
    names.contains("Then") should be (true)
    names.contains("And") should be (true)
    names.contains("But") should be (true)
  }

  "valueOf" should "evaluate OK" in {
    StepKeyword.valueOf("Given") should be (StepKeyword.Given)
    StepKeyword.valueOf("When") should be (StepKeyword.When)
    StepKeyword.valueOf("Then") should be (StepKeyword.Then)
    StepKeyword.valueOf("And") should be (StepKeyword.And)
    StepKeyword.valueOf("But") should be (StepKeyword.But)
  }
  
}