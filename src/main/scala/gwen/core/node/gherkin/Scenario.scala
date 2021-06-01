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
import gwen.core.behavior.BehaviorType
import gwen.core.status._

import scala.jdk.CollectionConverters._

import io.cucumber.messages.{Messages => Cucumber }

import java.io.File

/**
  * Captures a gherkin scenario.
  * 
  * @param sourceRef the location in source
  * @param tags list of tags
  * @param keyword the Gherkin keyword
  * @param name the scenario name
  * @param description optional description
  * @param background optional background
  * @param steps list of scenario steps
  * @param examples optional list of examples (scenario outline entries)
  * @param params the step parameters
  */
case class Scenario(
    sourceRef: Option[SourceRef],
    tags: List[Tag],
    keyword: String,
    name: String,
    description: List[String],
    background: Option[Background],
    steps: List[Step],
    examples: List[Examples],
    params: List[(String, String)]) extends GherkinNode {

  def nodeType: NodeType.Value = {
    if (isStepDef) {
      NodeType.StepDef
    } else {
      NodeType.Scenario
    }
  }
  
  /**
    * Returns a list containing all steps.
    */
  def allSteps: List[Step] = background.map(_.steps).getOrElse(Nil) ++ (if (!isOutline) steps else examples.flatMap(_.allSteps))
  
  def evalScenarios: List[Scenario] = 
    if (isStepDef) Nil
    else if(isOutline) examples.flatMap(_.scenarios)
    else List(this)
  
  def isOutline: Boolean = examples.nonEmpty || tags.exists(_.name == ReservedTags.Examples.toString)
  def isExpanded: Boolean = examples.flatMap(_.scenarios).nonEmpty 
  def isStepDef: Boolean = tags.exists(_.name == ReservedTags.StepDef.toString)
  def isForEach: Boolean = tags.exists(_.name == ReservedTags.ForEach.toString)
  def isDataTable: Boolean = tags.exists(_.name.startsWith(ReservedTags.DataTable.toString))
  def isSynchronized: Boolean = tags.map(_.name).exists { 
    name => name == ReservedTags.Synchronized.toString || name == ReservedTags.Synchronised.toString
  }
  def isSynthetic: Boolean = Tag.findByName(tags, ReservedTags.Synthetic.toString).nonEmpty
  
  def attachments: List[(String, File)] = {
    allSteps.flatMap(step => step.deepAttachments)
  }
  
  /** Returns the evaluation status of this scenario. */
  override val evalStatus: EvalStatus =
    if (isOutline && examples.flatMap(_.scenarios).isEmpty) Pending
     else EvalStatus(allSteps.map(_.evalStatus), ignoreSustained = !isStepDef)

  def behaviorTag: Option[Tag] = tags.find(tag => BehaviorType.values.exists(_.toString == tag.name))

  def copy(
      withSourceRef: Option[SourceRef] = sourceRef,
      withTags: List[Tag] = tags,
      withKeyword: String = keyword,
      withName: String = name,
      withDescription: List[String] = description,
      withBackground: Option[Background] = background,
      withSteps: List[Step] = steps,
      withExamples: List[Examples] = examples,
      withParams: List[(String, String)] = params): Scenario = {
    Scenario(withSourceRef, withTags, withKeyword, withName, withDescription, withBackground, withSteps, withExamples, withParams)
  }

  def occurrenceIn(parent: GwenNode): Int = {
    parent match {
      case spec: Spec =>
        occurrenceIn(spec.scenarios)
      case rule: Rule =>
        occurrenceIn(rule.scenarios)
      case _ => 0
    }
  }
  
}

object Scenario {
  def apply(file: Option[File], scenario: Cucumber.GherkinDocument.Feature.Scenario): Scenario = {
    def tags = Option(scenario.getTagsList).map(_.asScala.toList).getOrElse(Nil).distinct map { t => Tag(file, t) }
    Scenario(
      Option(scenario.getLocation).map(loc => SourceRef(file, loc)),
      tags,
      keywordFor(tags, scenario.getKeyword),
      scenario.getName,
      Option(scenario.getDescription).filter(_.length > 0).map(_.split("\n").toList.map(_.trim)).getOrElse(Nil),
      None,
      Option(scenario.getStepsList).map(_.asScala.toList).getOrElse(Nil).map { case s => Step(file, s) },
      scenario.getExamplesList.asScala.toList.zipWithIndex map { case (examples, index) => Examples(file, examples) },
      Nil
    )
  }
  def keywordFor(scenario: Scenario): String = keywordFor(scenario.tags, scenario.keyword)
  def keywordFor(tags: List[Tag], keyword: String): String = {
    tags.map(_.name) find { name =>
      name == ReservedTags.StepDef.toString || name == ReservedTags.ForEach.toString || name == ReservedTags.If.toString || name == ReservedTags.Until.toString || name == ReservedTags.While.toString
    } getOrElse {
      keyword.trim
    }
  }
}