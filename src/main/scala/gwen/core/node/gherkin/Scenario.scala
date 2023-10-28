/*
 * Copyright 2014-2022 Branko Juric, Brady Wood
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

import io.cucumber.messages.{ types => cucumber }

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
    override val params: List[(String, String)],
    override val callerParams: List[(String, String)]) extends GherkinNode {

  override val nodeType: NodeType = {
    if (isStepDef) {
      NodeType.StepDef
    } else {
      NodeType.Scenario
    }
  }

  override val evalStatus: EvalStatus = {
    if (isOutline && examples.flatMap(_.scenarios).isEmpty) {
      Passed(0, abstained = true)
    } else {
      EvalStatus(allSteps.map(_.evalStatus), ignoreSustained = !isStepDef)
    }
  }

  override def siblingsIn(parent: GwenNode): List[GwenNode] = {
    parent match {
      case spec: Spec => spec.scenarios
      case rule: Rule => rule.scenarios
      case examples: Examples => examples.scenarios
      case _ => Nil
    }
  }
  
  /**
    * Returns a list containing all steps.
    */
  def allSteps: List[Step] = allSteps(expanded = true)

  /**
    * Returns a list containing all steps.
    */
  def allSteps(expanded: Boolean): List[Step] = { 
    background.map(_.steps).getOrElse(Nil) ++ (
      if (expanded) {
        if (!isOutline) steps else examples.flatMap(_.allSteps)
      } else {
        steps
      }
    )
  }
  
  def evalScenarios: List[Scenario] = {
    if (isStepDef) Nil
    else if(isOutline) examples.flatMap(_.scenarios)
    else List(this)
  }
  
  def isOutline: Boolean = examples.nonEmpty || tags.exists(_.name.startsWith(Annotations.Examples.toString))
  def isExpanded: Boolean = examples.flatMap(_.scenarios).nonEmpty 
  def isStepDef: Boolean = tags.exists(_.name == Annotations.StepDef.toString)
  def isForEach: Boolean = tags.exists(_.name == Annotations.ForEach.toString)
  def isIfCondition: Boolean = tags.exists(_.name == Annotations.If.toString)
  def isWhileCondition: Boolean = tags.exists(_.name == Annotations.While.toString)
  def isUntilCondition: Boolean = tags.exists(_.name == Annotations.Until.toString)
  def isGuarded: Boolean = isIfCondition || isWhileCondition || isUntilCondition
  def isDataTable: Boolean = tags.exists(_.name.startsWith(Annotations.DataTable.toString))
  def isHorizontalTable: Boolean = tags.exists(_.name == Annotations.HorizontalTable.toString)
  def isSynchronized: Boolean = tags.map(_.name).exists { 
    name => name == Annotations.Synchronized.toString || name == Annotations.Synchronised.toString
  }
  def isSynthetic: Boolean = Tag.findByName(tags, Annotations.Synthetic.toString).nonEmpty
  
  def attachments: List[(String, File)] = {
    allSteps.flatMap(step => step.deepAttachments)
  }
  
  def behaviorTag: Option[Tag] = {
    tags.find(tag => BehaviorType.values.exists(_.toString == tag.name))
  }

  def copy(
      withSourceRef: Option[SourceRef] = sourceRef,
      withTags: List[Tag] = tags,
      withKeyword: String = keyword,
      withName: String = name,
      withDescription: List[String] = description,
      withBackground: Option[Background] = background,
      withSteps: List[Step] = steps,
      withExamples: List[Examples] = examples,
      withParams: List[(String, String)] = params,
      withCallerParams: List[(String, String)] = callerParams): Scenario = {
    Scenario(withSourceRef, withTags, withKeyword, withName, withDescription, withBackground, withSteps, withExamples, withParams, withCallerParams)
  }

  def withCallerParams(caller: GwenNode): Scenario = {
    val names = callerParams map { case (n, _) => n }
    caller match {
      case step: Step => 
        step.cumulativeParams filter { case (name, _) => 
          !names.contains(name)
        } match {
          case Nil => this
          case sParams => copy(withCallerParams = callerParams ++ sParams)
        }
      case _ => this
    }
  }

  def cumulativeParams: List[(String, String)] = {
    val names = params map { case (n, _) => n }
    params ++ (
      callerParams filter { case (name, _) => 
        !names.contains(name)
      }
    )
  }

  /**
    * Interpolate placeholder references in this scenario.
    *
    * @param interpolator the interpolator to use
    * @return the interpolated step
    */
  override def interpolate(interpolator: String => String): Scenario = {
    copy(
      withTags = tags.map(_.interpolate(interpolator)),
      withName = interpolator.apply(name),
      withDescription = description.map(interpolator)
    )
  }

}

object Scenario {
  def apply(file: Option[File], scenario: cucumber.Scenario, verbatim: Boolean): Scenario = {
    def tags = Option(scenario.getTags).map(_.asScala.toList).getOrElse(Nil).distinct map { t => Tag(file, t) }
    Scenario(
      Option(scenario.getLocation).map(loc => SourceRef(file, loc)),
      tags,
      if (verbatim) scenario.getKeyword else keywordFor(tags, scenario.getKeyword),
      scenario.getName,
      Option(scenario.getDescription).filter(_.length > 0).map(_.split("\n").toList.map(_.trim)).getOrElse(Nil),
      None,
      Option(scenario.getSteps).map(_.asScala.toList).getOrElse(Nil).map { case s => Step(file, s) },
      scenario.getExamples.asScala.toList.zipWithIndex map { case (examples, index) => Examples(file, examples) },
      Nil,
      Nil
    )
  }
  def keywordFor(scenario: Scenario): String = keywordFor(scenario.tags, scenario.keyword)
  def keywordFor(tags: List[Tag], keyword: String): String = {
    tags.map(_.name) find { name =>
      name == Annotations.StepDef.toString || name == Annotations.ForEach.toString || name == Annotations.If.toString || name == Annotations.Until.toString || name == Annotations.While.toString
    } getOrElse {
      keyword.trim
    }
  }
}
