/*
 * Copyright 2017-2020 Branko Juric, Brady Wood
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

import gwen.GwenSettings
import scala.language.postfixOps

object FeatureKeyword extends Enumeration {
  type FeatureKeyword = Value
  val Feature, Background, Rule, Scenario, Example, Examples = Value

  /** List of all keyword names. */
  val names: Set[String] = values.map(_.toString)

}

object FeatureSymbol extends Enumeration {
  type FeatureSymbol = Value
  val `#` = Value("#")
  val `@` = Value("@")

  /** List of all keyword names. */
  val names: Set[String] = values.map(_.toString)

}

object BehaviorType extends Enumeration {
  
  type BehaviorType = Value
  val Context, Action, Assertion = Value

  def of(keyword: StepKeyword.Value): BehaviorType.Value = keyword match {
    case StepKeyword.Given => BehaviorType.Context
    case StepKeyword.When => BehaviorType.Action
    case StepKeyword.Then | StepKeyword.But => BehaviorType.Assertion
  }

}

object StepKeyword extends Enumeration {
  
  type StepKeyword = Value
  val Given, When, Then, And, But = Value

  /** List of all keyword names. */
  val names: Set[String] = values.map(_.toString)

}

object ReservedKeyword {
  val literals = FeatureKeyword.names.map(_ + ":") ++ FeatureSymbol.names ++ StepKeyword.names
}

object AssertionMode extends Enumeration {
  type AssertionMode = Value
  val hard, soft, sustained = Value
  def isHard = GwenSettings.`gwen.assertion.mode` == hard
  def isSoft = GwenSettings.`gwen.assertion.mode` == soft
  def isSustained = GwenSettings.`gwen.assertion.mode` == sustained
}

object StateLevel extends Enumeration {
  type StateLevel = Value
  val scenario, feature = Value
  def isScenario = GwenSettings.`gwen.state.level` == scenario
  def isFeature = GwenSettings.`gwen.state.level` == feature
}

object FeatureMode extends Enumeration {
  type FeatureMode = Value
  val declarative, imperative = Value
  def isDeclarative = GwenSettings.`gwen.feature.mode` == declarative
  def isImperative = GwenSettings.`gwen.feature.mode` == imperative
}

object BehaviorRules extends Enumeration {
  type BehaviorRules = Value
  val strict, lenient = Value
  def isStrict = GwenSettings.`gwen.behavior.rules` == strict
  def isLenient = GwenSettings.`gwen.behavior.rules` == lenient
}

