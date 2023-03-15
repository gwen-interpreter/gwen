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

import gwen.core._
import gwen.core.node.FeatureUnit
import gwen.core.node.GwenNode
import gwen.core.node.NodeType
import gwen.core.status._

import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._

import io.cucumber.messages.{ types => cucumber }

import java.io.File

/**
 * A Gherkin feature specification.
 *
 * @param feature the feature node
 * @param background optional background
 * @param scenarios list of scenarios
 * @param rules list of rules
 * @param metaSpecs list of meta specs
 */

case class Spec(
    feature: Feature, 
    background: Option[Background], 
    scenarios: List[Scenario],
    rules: List[Rule],
    metaSpecs: List[Spec]) extends GherkinNode {

  override val name = feature.name
  override val sourceRef = feature.sourceRef
  override val nodeType: NodeType = NodeType.valueOf(specType.toString)
  override def siblingsIn(parent: GwenNode): List[GwenNode] = {
    parent match {
      case _: FeatureUnit => List(feature)
      case _ => Nil
    }
  }

  def specFile: Option[File] = sourceRef.flatMap(_.file)
  def specType: SpecType = feature.specType

  def isMeta: Boolean = specType.isMeta

  /** Resource id */
  def uri = specFile.map(_.uri).getOrElse(uuid)

  /**
    * Gets the list of all steps contained in the feature spec. The list includes
    * all meta steps (if any) and all scenario steps (including any background 
    * steps).
    * 
    * @return a list containing all the steps (or an empty list if none exist)
    */
  def steps: List[Step] = scenarios.flatMap(_.allSteps) ++ rules.flatMap(_.allSteps)

  def evalScenarios = scenarios.flatMap(_.evalScenarios) ++ rules.flatMap(_.evalScenarios)

  /** Gets all attachments. */
  def attachments: List[(String, File)] = steps.flatMap(_.deepSteps.flatMap(_.attachments))

  /** Gets the number of sustained errors. */
  def sustainedCount: Int = Step.errorTrails(this).flatMap(_.lastOption).count(_.evalStatus.isSustained)
  
  /** Returns the evaluation status of this feature spec. */
  override val evalStatus: EvalStatus = {
    val specStatus = EvalStatus(steps.map(_.evalStatus))
    metaSpecs match {
      case Nil => specStatus
      case _ =>
        val totalStatus = EvalStatus((metaSpecs.flatMap(_.steps) ++ steps).map(_.evalStatus))
        specStatus match {
          case _: Passed => Passed(totalStatus.nanos)
          case _ => totalStatus
        }
    }
  }

  def copy(
      withFeature: Feature = feature,
      withBackground: Option[Background] = background,
      withScenarios: List[Scenario] = scenarios,
      withRules: List[Rule] = rules,
      withMetaSpecs: List[Spec] = metaSpecs): Spec = {
    Spec(withFeature, withBackground, withScenarios, withRules, withMetaSpecs)
  }
  
}

object Spec {
  def apply(file: Option[File], spec: cucumber.GherkinDocument, verbatim: Boolean): Spec = {
    val cFeature = spec.getFeature.toScala.get
    val feature = Feature(file, cFeature)
    val background = cFeature.getChildren.asScala.toList.flatMap(_.getBackground.toScala).headOption map { b => Background(file, b) }
    val scenarios = cFeature.getChildren.asScala.toList.flatMap(_.getScenario.toScala) map { s => Scenario(file, s, verbatim) }
    val rules = cFeature.getChildren.asScala.toList.flatMap(_.getRule.toScala).map { case r => Rule(file, r, verbatim) }
    Spec(feature, background, scenarios, rules, Nil)
  }
}
