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

package gwen.model.gherkin

import gwen.model._

import scala.jdk.CollectionConverters._

import io.cucumber.messages.{ Messages => Cucumber }

import java.io.File

/**
 * A Gherkin feature specification.
 *
 * @param name the feature name
 * @param background optional background
 * @param scenarios list of scenarios
 * @param specFile optional source feature file
 * @param metaSpecs optional list of meta specs
 */

case class Spec(
    feature: Feature, 
    background: Option[Background], 
    scenarios: List[Scenario],
    rules: List[Rule],
    specFile: Option[File],
    metaSpecs: List[Spec]) extends SpecNode {

  val name = feature.name
  val sourceRef = feature.sourceRef

  def specType: SpecType.Value = feature.specType
  def nodeType: NodeType.Value = NodeType.withName(specType.toString)

  def isMeta: Boolean = SpecType.isMeta(specType)

  /** Resource id */
  def uri = specFile.map(_.getPath).getOrElse(uuid)

  /**
    * Gets the list of all steps contained in the feature spec. The list includes
    * all meta steps (if any) and all scenario steps (including any background 
    * steps).
    * 
    * @return a list containing all the steps (or an empty list if none exist)
    */
  def steps: List[Step] = rules.flatMap(_.allSteps) ++ scenarios.flatMap(_.allSteps)

  def evalScenarios = scenarios.flatMap(_.evalScenarios) ++ rules.flatMap(_.evalScenarios)

  /** Gets all attachments. */
  def attachments: List[(String, File)] = steps.flatMap(_.attachments)

  /** Gets the number of sustained errors. */
  def sustainedCount: Int = {
    steps.flatMap { s1 =>
      s1.stepDef.map { case (s2, _) =>
        s2.allSteps.flatMap { s3 =>
          s3.stepDef map { case (s4, _) => s4.allSteps } getOrElse List(s3)
        }
      } getOrElse List(s1)
    } count(_.evalStatus.status == StatusKeyword.Sustained)
  }
  
  /** Returns the evaluation status of this feature spec. */
  override val evalStatus: EvalStatus = {
    val ss = steps.map(_.evalStatus)
    val specStatus = EvalStatus(ss)
    metaSpecs match {
      case Nil => specStatus
      case _ =>
        val totalStatus = EvalStatus((metaSpecs.flatMap(_.steps) ++ steps).map(_.evalStatus))
        specStatus match {
          case Passed(_) => Passed(totalStatus.nanos)
          case _ => totalStatus
        }
    }
  }

  def copy(
      withFeature: Feature = feature,
      withBackground: Option[Background] = background,
      withScenarios: List[Scenario] = scenarios,
      withRules: List[Rule] = rules,
      withSpecFile: Option[File] = specFile,
      withMetaSpecs: List[Spec] = metaSpecs): Spec = {
    Spec(withFeature, withBackground, withScenarios, withRules, withSpecFile, withMetaSpecs)
  }
  
}

object Spec {
  def apply(uri: String, spec: Cucumber.GherkinDocument, specFile: Option[File]): Spec = {
    val feature = Feature(uri, spec.getFeature, 0)
    val background = spec.getFeature.getChildrenList.asScala.toList.filter(_.hasBackground).headOption.map(x => Background(uri, x.getBackground, 0))
    val scenarios = spec.getFeature.getChildrenList.asScala.toList.filter(_.hasScenario).zipWithIndex.map { case (x, i) => Scenario(uri, x.getScenario, i) }
    val rules = spec.getFeature.getChildrenList.asScala.toList.filter(_.hasRule()).zipWithIndex.map { case (x, i) => Rule(uri, x.getRule, i) }
    Spec(feature, background, scenarios, rules, specFile, Nil)
  }
}
