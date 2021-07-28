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
import org.scalatest.matchers.should.Matchers

class FeatureKeywordTest extends BaseTest with Matchers {

  "names" should "evaluate correctly" in {
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

  "valueOf" should "evaluate correctly" in {
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

  "nameOf" should "evaluate correctly" in {
    FeatureKeyword.nameOf(FeatureKeyword.Feature) should be ("Feature")
    FeatureKeyword.nameOf(FeatureKeyword.`Business Need`) should be ("Business Need")
    FeatureKeyword.nameOf(FeatureKeyword.Ability) should be ("Ability")
    FeatureKeyword.nameOf(FeatureKeyword.Background) should be ("Background")
    FeatureKeyword.nameOf(FeatureKeyword.Rule) should be ("Rule")
    FeatureKeyword.nameOf(FeatureKeyword.`Scenario Outline`) should be ("Scenario Outline")
    FeatureKeyword.nameOf(FeatureKeyword.`Scenario Template`) should be ("Scenario Template")
    FeatureKeyword.nameOf(FeatureKeyword.Example) should be ("Example")
    FeatureKeyword.nameOf(FeatureKeyword.Examples) should be ("Examples")
    FeatureKeyword.nameOf(FeatureKeyword.Scenarios) should be ("Scenarios")
  }

  "isFeature" should "evaluate correctly" in {
    FeatureKeyword.isFeature("Feature") should be (true)
    FeatureKeyword.isFeature("Business Need") should be (true)
    FeatureKeyword.isFeature("Ability") should be (true)
    FeatureKeyword.isFeature("Background") should be (false)
    FeatureKeyword.isFeature("Rule") should be (false)
    FeatureKeyword.isFeature("Scenario") should be (false)
    FeatureKeyword.isFeature("Scenario Outline") should be (false)
    FeatureKeyword.isFeature("Scenario Template") should be (false)
    FeatureKeyword.isFeature("Example") should be (false)
    FeatureKeyword.isFeature("Examples") should be (false)
    FeatureKeyword.isFeature("Scenarios") should be (false)
  }

  "isBackground" should "evaluate correctly" in {
    FeatureKeyword.isBackground("Feature") should be (false)
    FeatureKeyword.isBackground("Business Need") should be (false)
    FeatureKeyword.isBackground("Ability") should be (false)
    FeatureKeyword.isBackground("Background") should be (true)
    FeatureKeyword.isBackground("Rule") should be (false)
    FeatureKeyword.isBackground("Scenario") should be (false)
    FeatureKeyword.isBackground("Scenario Outline") should be (false)
    FeatureKeyword.isBackground("Scenario Template") should be (false)
    FeatureKeyword.isBackground("Example") should be (false)
    FeatureKeyword.isBackground("Examples") should be (false)
    FeatureKeyword.isBackground("Scenarios") should be (false)
  }

  "isRule" should "evaluate correctly" in {
    FeatureKeyword.isRule("Feature") should be (false)
    FeatureKeyword.isRule("Business Need") should be (false)
    FeatureKeyword.isRule("Ability") should be (false)
    FeatureKeyword.isRule("Background") should be (false)
    FeatureKeyword.isRule("Rule") should be (true)
    FeatureKeyword.isRule("Scenario") should be (false)
    FeatureKeyword.isRule("Scenario Outline") should be (false)
    FeatureKeyword.isRule("Scenario Template") should be (false)
    FeatureKeyword.isRule("Example") should be (false)
    FeatureKeyword.isRule("Examples") should be (false)
    FeatureKeyword.isRule("Scenarios") should be (false)
  }

  "isScenario" should "evaluate correctly" in {
    FeatureKeyword.isScenario("Feature") should be (false)
    FeatureKeyword.isScenario("Business Need") should be (false)
    FeatureKeyword.isScenario("Ability") should be (false)
    FeatureKeyword.isScenario("Background") should be (false)
    FeatureKeyword.isScenario("Rule") should be (false)
    FeatureKeyword.isScenario("Scenario") should be (true)
    FeatureKeyword.isScenario("Scenario Outline") should be (false)
    FeatureKeyword.isScenario("Scenario Template") should be (false)
    FeatureKeyword.isScenario("Example") should be (false)
    FeatureKeyword.isScenario("Examples") should be (false)
    FeatureKeyword.isScenario("Scenarios") should be (false)
  }

  "isScenarioOutline" should "evaluate correctly" in {
    FeatureKeyword.isScenarioOutline("Feature") should be (false)
    FeatureKeyword.isScenarioOutline("Business Need") should be (false)
    FeatureKeyword.isScenarioOutline("Ability") should be (false)
    FeatureKeyword.isScenarioOutline("Background") should be (false)
    FeatureKeyword.isScenarioOutline("Rule") should be (false)
    FeatureKeyword.isScenarioOutline("Scenario") should be (false)
    FeatureKeyword.isScenarioOutline("Scenario Outline") should be (true)
    FeatureKeyword.isScenarioOutline("Scenario Template") should be (false)
    FeatureKeyword.isScenarioOutline("Example") should be (false)
    FeatureKeyword.isScenarioOutline("Examples") should be (false)
    FeatureKeyword.isScenarioOutline("Scenarios") should be (false)
  }

  "isScenarioTemplate" should "evaluate correctly" in {
    FeatureKeyword.isScenarioTemplate("Feature") should be (false)
    FeatureKeyword.isScenarioTemplate("Business Need") should be (false)
    FeatureKeyword.isScenarioTemplate("Ability") should be (false)
    FeatureKeyword.isScenarioTemplate("Background") should be (false)
    FeatureKeyword.isScenarioTemplate("Rule") should be (false)
    FeatureKeyword.isScenarioTemplate("Scenario") should be (false)
    FeatureKeyword.isScenarioTemplate("Scenario Outline") should be (false)
    FeatureKeyword.isScenarioTemplate("Scenario Template") should be (true)
    FeatureKeyword.isScenarioTemplate("Example") should be (false)
    FeatureKeyword.isScenarioTemplate("Examples") should be (false)
    FeatureKeyword.isScenarioTemplate("Scenarios") should be (false)
  }

  "isExample" should "evaluate correctly" in {
    FeatureKeyword.isExample("Feature") should be (false)
    FeatureKeyword.isExample("Business Need") should be (false)
    FeatureKeyword.isExample("Ability") should be (false)
    FeatureKeyword.isExample("Background") should be (false)
    FeatureKeyword.isExample("Rule") should be (false)
    FeatureKeyword.isExample("Scenario") should be (false)
    FeatureKeyword.isExample("Scenario Outline") should be (false)
    FeatureKeyword.isExample("Scenario Template") should be (false)
    FeatureKeyword.isExample("Example") should be (true)
    FeatureKeyword.isExample("Examples") should be (false)
    FeatureKeyword.isExample("Scenarios") should be (false)
  }

  "isExamples" should "evaluate correctly" in {
    FeatureKeyword.isExamples("Feature") should be (false)
    FeatureKeyword.isExamples("Business Need") should be (false)
    FeatureKeyword.isExamples("Ability") should be (false)
    FeatureKeyword.isExamples("Background") should be (false)
    FeatureKeyword.isExamples("Rule") should be (false)
    FeatureKeyword.isExamples("Scenario") should be (false)
    FeatureKeyword.isExamples("Scenario Outline") should be (false)
    FeatureKeyword.isExamples("Scenario Template") should be (false)
    FeatureKeyword.isExamples("Example") should be (false)
    FeatureKeyword.isExamples("Examples") should be (true)
    FeatureKeyword.isExamples("Scenarios") should be (true)
  }

}
