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

import gwen.core.node.GwenNode
import gwen.core.node.NodeType
import gwen.core.node.SourceRef
import gwen.core.status.EvalStatus

import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._

import io.cucumber.messages.{ types => cucumber }

import java.io.File

/**
  * Captures a gherkin rule.
  * 
  * @param sourceRef the location in source
  * @param keyword the Gherkin keyword for this Rule
  * @param name the rule name
  * @param description optional description
  * @param background optional background
  * @param scenarios list of scenarios (or examples)
  */
case class Rule(
    sourceRef: Option[SourceRef],
    keyword: String,
    name: String,
    description: List[String],
    background: Option[Background],
    scenarios: List[Scenario]) extends GherkinNode {
  
  override val nodeType: NodeType = NodeType.Rule

  override def siblingsIn(parent: GwenNode): List[GwenNode] = {
    parent match {
      case spec: Spec => spec.rules
      case _ => Nil
    }
  }

  /**
    * Gets the list of all steps contained in the rule. The list includes
    * all meta steps (if any) and all scenario steps (including any background 
    * steps).
    * 
    * @return a list containing all the steps (or an empty list if none exist)
    */
  def allSteps: List[Step] = background.map(_.steps).getOrElse(Nil) ++ scenarios.flatMap(_.allSteps)

  def evalScenarios: List[Scenario] = scenarios.flatMap(_.evalScenarios)
  
  /** Returns the evaluation status of this rule. */
  override val evalStatus: EvalStatus = EvalStatus(allSteps.map(_.evalStatus))

  def copy(
      withSourceRef: Option[SourceRef] = sourceRef,
      withKeyword: String = keyword,
      withName: String = name,
      withDescription: List[String] = description,
      withBackground: Option[Background] = background,
      withScenarios: List[Scenario] = scenarios): Rule = {
    Rule(withSourceRef, withKeyword, withName, withDescription, withBackground, withScenarios)
  }

  /**
    * Interpolate placeholder references in this rule.
    *
    * @param interpolator the interpolator to use
    * @return the interpolated step
    */
  override def interpolate(interpolator: String => String): Rule = {
    copy(
      withName = interpolator.apply(name),
      withDescription = description.map(interpolator)
    )
  }

}

object Rule {
  def apply(file: Option[File], rule: cucumber.Rule, verbatim: Boolean): Rule = {
    Rule(
      Option(rule.getLocation).map(loc => SourceRef(file, loc)),
      rule.getKeyword,
      rule.getName,
      Option(rule.getDescription).filter(_.length > 0).map(_.split("\n").toList.map(_.trim)).getOrElse(Nil),
      rule.getChildren.asScala.toList.flatMap(_.getBackground.toScala).headOption map { b => Background(file, b) },
      rule.getChildren.asScala.toList.flatMap(_.getScenario.toScala) map { s => Scenario(file, s, verbatim) }
    )
  }
}
