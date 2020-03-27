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
import gwen.errors.keywordDialectError
import scala.language.postfixOps
import io.cucumber.gherkin.GherkinDialectProvider
import scala.util.Try
import scala.collection.JavaConverters._
import java.{util => ju}
import java.awt.Dialog

object FeatureKeyword extends Enumeration {
  type FeatureKeyword = Value
  val Feature, Ability, Background, Rule, Scenario, Example, Examples, Scenarios = Value
  val `Business Need` = Value("Business Need")
  val `Scenario Outline` = Value("Scenario Outline")
  val `Scenario Template` = Value("Scenario Template")

  def nameOf(keyword: FeatureKeyword.Value): String = namesOf(keyword).head

  private def namesOf(keyword: FeatureKeyword.Value): List[String] = {
    val dialect = Dialect.instance
    if (dialect.getLanguage == "en") {
      List(keyword.toString)
    } else {
      (keyword match {
        case Feature | Ability => dialect.getFeatureKeywords
        case `Business Need` => dialect.getFeatureKeywords
        case Background => dialect.getBackgroundKeywords
        case Rule => dialect.getRuleKeywords
        case Scenario | Example if (dialect.getLanguage != "en") => dialect.getScenarioKeywords
        case `Scenario Outline` => dialect.getScenarioOutlineKeywords
        case `Scenario Template` => dialect.getScenarioOutlineKeywords
        case _ => dialect.getExamplesKeywords
      }).asScala.toList.map(_.trim)
    }
  }
  
  def names: List[String] =
    ((Feature.toString::(`Business Need`.toString::(Ability.toString::namesOf(Feature)))) ++
    (Background.toString::namesOf(Background)) ++
    (Rule.toString::namesOf(Rule)) ++
    (Scenario.toString::(Example.toString::namesOf(Scenario))) ++
    (`Scenario Outline`.toString::(`Scenario Template`.toString::namesOf(`Scenario Outline`))) ++ 
    (Examples.toString::(Scenarios.toString::namesOf(Examples)))).distinct

  def isFeature(keyword: String): Boolean = 
    Feature.toString == keyword || `Business Need`.toString == keyword || Ability.toString == keyword || namesOf(Feature).exists(_ == keyword)

  def isBackground(keyword: String): Boolean = 
    Background.toString == keyword || namesOf(Background).exists(_ == keyword)

  def isRule(keyword: String): Boolean = 
    Rule.toString == keyword || namesOf(Rule).exists(_ == keyword)

  def isScenario(keyword: String): Boolean = 
    Scenario.toString == keyword || namesOf(Scenario).exists(_ == keyword)

  def isScenarioOutline(keyword: String): Boolean = 
    `Scenario Outline`.toString == keyword || namesOf(`Scenario Outline`).exists(_ == keyword)

  def isScenarioTemplate(keyword: String): Boolean = 
    `Scenario Template`.toString == keyword || namesOf(`Scenario Template`).exists(_ == keyword)

  def isExample(keyword: String): Boolean = 
    Example.toString == keyword || namesOf(Example).exists(_ == keyword)

  def isExamples(keyword: String): Boolean = 
    Examples.toString == keyword || Scenarios.toString == keyword || namesOf(Examples).exists(_ == keyword)

  def valueOf(keyword: String): FeatureKeyword.Value = 
    Try(FeatureKeyword.withName(keyword)).getOrElse {
      if (isFeature(keyword)) Feature
      else if (isBackground(keyword)) Background
      else if (isRule(keyword)) Rule
      else if (isScenario(keyword)) Scenario
      else if (isScenarioOutline(keyword)) `Scenario Outline`
      else if (isExample(keyword)) Example
      else Examples
    }
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

  def of(stepKeyword: String): BehaviorType.Value = 
    if (StepKeyword.isGiven(stepKeyword)) BehaviorType.Context
    else if (StepKeyword.isWhen(stepKeyword)) BehaviorType.Action
    else BehaviorType.Assertion

}

object StepKeyword extends Enumeration {

  type StepKeyword = Value
  val Given, When, Then, And, But = Value

  def nameOf(keyword: StepKeyword.Value): String = namesOf(keyword).head

  private def namesOf(keyword: StepKeyword.Value): List[String] = {
    val dialect = Dialect.instance
    (keyword match {
      case Given => dialect.getGivenKeywords
      case When => dialect.getWhenKeywords
      case Then => dialect.getThenKeywords
      case And => dialect.getAndKeywords
      case _ => dialect.getButKeywords
    }).asScala.toList.map(_.trim).filter(_ != "*")
  }
  
  def names: List[String] =
    ((Given.toString::namesOf(Given)) ++
    (When.toString::namesOf(When)) ++
    (Then.toString::namesOf(Then)) ++
    (And.toString::namesOf(And)) ++
    (But.toString::namesOf(But))).distinct

  def isGiven(keyword: String): Boolean = 
    Given.toString == keyword || namesOf(Given).exists(_ == keyword)

  def isWhen(keyword: String): Boolean = 
    When.toString == keyword || namesOf(When).exists(_ == keyword)

  def isThen(keyword: String): Boolean = 
    Then.toString == keyword || namesOf(Then).exists(_ == keyword)

  def isAnd(keyword: String): Boolean = 
    And.toString == keyword || namesOf(And).exists(_ == keyword)

  def isBut(keyword: String): Boolean = 
    But.toString == keyword || namesOf(But).exists(_ == keyword)

  def valueOf(keyword: String): StepKeyword.Value = 
    Try(StepKeyword.withName(keyword)).getOrElse {
      if (isGiven(keyword)) Given
      else if (isWhen(keyword)) When
      else if (isThen(keyword)) Then
      else if (isAnd(keyword)) And
      else But
    }

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

