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

class FeatureKeywordTest extends FlatSpec with Matchers {

   "names" should "evaluate" in {
    val names = FeatureKeyword.names
    names.size should be (11)
    names.contains("Feature") should be (true)
    names.contains("Business Need") should be (true)
    names.contains("Ability") should be (true)
    names.contains("Background") should be (true)
    names.contains("Rule") should be (true)
    names.contains("Scenario") should be (true)
    names.contains("Scenario Outline") should be (true)
    names.contains("Scenario Template") should be (true)
    names.contains("Example") should be (true)
    names.contains("Examples") should be (true)
    names.contains("Scenarios") should be (true)
  }

  "valueOf" should "evaluate OK" in {
    FeatureKeyword.valueOf("Feature") should be (FeatureKeyword.Feature)
    FeatureKeyword.valueOf("Business Need") should be (FeatureKeyword.`Business Need`)
    FeatureKeyword.valueOf("Ability") should be (FeatureKeyword.Ability)
    FeatureKeyword.valueOf("Background") should be (FeatureKeyword.Background)
    FeatureKeyword.valueOf("Rule") should be (FeatureKeyword.Rule)
    FeatureKeyword.valueOf("Scenario") should be (FeatureKeyword.Scenario)
    FeatureKeyword.valueOf("Scenario Outline") should be (FeatureKeyword.`Scenario Outline`)
    FeatureKeyword.valueOf("Scenario Template") should be (FeatureKeyword.`Scenario Template`)
    FeatureKeyword.valueOf("Example") should be (FeatureKeyword.Example)
    FeatureKeyword.valueOf("Examples") should be (FeatureKeyword.Examples)
    FeatureKeyword.valueOf("Scenarios") should be (FeatureKeyword.Scenarios)
  }
  
}