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

object StepKeyword extends Enumeration {

  val Given, When, Then, And, But = Value

  def nameOf(keyword: StepKeyword.Value): String = namesOf(keyword).filter(_ != "*").head

  private def namesOf(keyword: StepKeyword.Value): List[String] = {
    val dialect = Dialect.instance
    val names = (keyword match {
      case Given => dialect.getGivenKeywords
      case When => dialect.getWhenKeywords
      case Then => dialect.getThenKeywords
      case And => dialect.getAndKeywords
      case _ => dialect.getButKeywords
    }).asScala.toList.map(_.trim)
    if (keyword != And) names.filter(_ != "*")
    else names
  }
  
  def names: List[String] =
    ((Given.toString::namesOf(Given)) ++
    (When.toString::namesOf(When)) ++
    (Then.toString::namesOf(Then)) ++
    (And.toString::namesOf(And)) ++
    (But.toString::namesOf(But))).filter(_ != "*").distinct

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
  
  def maxLength: Int = names.map(_.length).max

}
