/*
 * Copyright 2017-2021 Branko Juric, Brady Wood
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

import scala.jdk.CollectionConverters._
import scala.util.Try

object FeatureKeyword extends Enumeration {
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
