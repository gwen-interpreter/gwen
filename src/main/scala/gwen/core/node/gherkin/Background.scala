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

import gwen.core.node.NodeType
import gwen.core.node.SourceRef
import gwen.core.status.EvalStatus

import scala.jdk.CollectionConverters._

import io.cucumber.messages.{ Messages => Cucumber }

import java.io.File

/**
  * Captures a gherkin background node.
  *
  * @param sourceRef the location in source
  * @param keyword the Gherkin keyword for this Background
  * @param name the background name
  * @param description optional background description
  * @param steps list of background steps
 */
case class Background(
    sourceRef: Option[SourceRef],
    keyword: String, 
    name: String, 
    description: List[String], 
    steps: List[Step]) extends GherkinNode {

  def nodeType: NodeType.Value = NodeType.Background
  def gwtOrder: List[String] = steps.map(_.keyword).filter(k => !StepKeyword.isAnd(k))

  override val evalStatus: EvalStatus = EvalStatus(steps.map(_.evalStatus))

  def copy(
      withSourceRef: Option[SourceRef] = sourceRef,
      withKeyword: String = keyword, 
      withName: String = name, 
      withDescription: List[String] = description, 
      withSteps: List[Step] = steps): Background = {
    Background(withSourceRef, withKeyword, withName, withDescription, withSteps)
  }
  
}

object Background {
  def apply(file: Option[File], background: Cucumber.GherkinDocument.Feature.Background): Background = {
    Background(
      Option(background.getLocation).map(loc => SourceRef(file, loc)),
      background.getKeyword,
      background.getName,
      Option(background.getDescription).filter(_.length > 0).map(_.split("\n").toList.map(_.trim)).getOrElse(Nil),
      Option(background.getStepsList).map(_.asScala.toList).getOrElse(Nil).map { case s => Step(file, s) }
    )
  }
}
