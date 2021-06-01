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

import gwen.core.Errors
import gwen.core.node.GwenNode
import gwen.core.node.NodeType
import gwen.core.node.SourceRef
import gwen.core.status.EvalStatus

import scala.jdk.CollectionConverters._

import io.cucumber.messages.{ Messages => Cucumber }

import java.io.File

/**
  * Captures a gherkin scenario outline example group.
  *
  * @param sourceRef the location in source
  * @param tags list of tags
  * @param keyword the Gherkin keyword for this Examples clause
  * @param name the example name
  * @param description option description lines
  * @param table header and body data (tuple of line position and rows of data)
  * @param scenarios list of expanded scenarios
  */
case class Examples(
    sourceRef: Option[SourceRef],
    tags: List[Tag], 
    keyword: String, 
    name: String, 
    description: List[String], 
    table: List[(Int, List[String])], 
    scenarios: List[Scenario]) extends GherkinNode {

  def nodeType: NodeType.Value = NodeType.Examples

  def isExpanded: Boolean = scenarios.nonEmpty 

  /**
    * Returns a list containing all the background steps (if any) followed by
    * all the scenario steps.
    */
  def allSteps: List[Step] = scenarios.flatMap(_.allSteps)

  /** Returns the evaluation status of this examples group. */
  override val evalStatus: EvalStatus = EvalStatus(scenarios.map(_.evalStatus))

  def copy(
      withSourceRef: Option[SourceRef] = sourceRef,
      withTags: List[Tag] = tags, 
      withKeyword: String = keyword, 
      withName: String = name, 
      withDescription: List[String] = description, 
      withTable: List[(Int, List[String])] = table, 
      withScenarios: List[Scenario] = scenarios): Examples = {
    Examples(withSourceRef, withTags, withKeyword, withName, withDescription, withTable, withScenarios)
  }

  def occurrenceIn(parent: GwenNode): Int = {
    parent match {
      case scenario: Scenario =>
        occurrenceIn(scenario.examples)
      case _ => 0
    }
  }

}

object Examples {
  def apply(file: Option[File], examples: Cucumber.GherkinDocument.Feature.Scenario.Examples): Examples = {
    val header = examples.getTableHeader
    if (header == null) {
      Errors.syntaxError(
        s"Failed to read table body. Possible syntax error or missing column delimiter in table",
        file,
        examples.getLocation.getLine,
        examples.getLocation.getColumn)
    }
    val body = examples.getTableBodyList
    if (body == null) {
      Errors.syntaxError(
        s"Failed to read table header. Possible syntax error or missing column delimiter in table",
        file,
        examples.getLocation.getLine,
        examples.getLocation.getColumn)
    }
    Examples(
      Option(examples.getLocation).map(loc => SourceRef(file, loc)),
      Option(examples.getTagsList).map(_.asScala.toList).getOrElse(Nil) map { t => Tag(file, t) },
      examples.getKeyword,
      examples.getName,
      Option(examples.getDescription).filter(_.length > 0).map(_.split("\n").toList.map(_.trim)).getOrElse(Nil),
      (header.getLocation.getLine, header.getCellsList.asScala.toList.map(_.getValue)) ::
        body.iterator.asScala.toList.map { row =>
          (row.getLocation.getLine, row.getCellsList.asScala.toList.map(_.getValue))
        },
      Nil
    )
  }
}
