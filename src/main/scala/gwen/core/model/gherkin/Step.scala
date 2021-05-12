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

package gwen.core.model.gherkin

import gwen.core.model._

import scala.jdk.CollectionConverters._

import io.cucumber.messages.{ Messages => Cucumber }

import java.io.File

/**
  * Captures a gherkin step.
  *
  * @param sourceRef the location in source
  * @param keyword keyword identifier (Given, When, Then, etc..)
  * @param name the step name (that is: the text following the step keyword)
  * @param status optional evaluation status (default = Pending)
  * @param attachments file attachments as name-file pairs (default = Nil)
  * @param stepDef optional evaluated step def
  * @param table data table (List of tuples of line position and rows of data)
  * @param docString optional tuple of line, content, and content type
  * @param evalStatus the evaluation status of the step
  * @param ancestor the top most calling step
  */
case class Step(
    sourceRef: Option[SourceRef],
    keyword: String,
    name: String,
    attachments: List[(String, File)],
    stepDef: Option[(Scenario, List[(String, String)])],
    table: List[(Int, List[String])],
    docString: Option[(Int, String, Option[String])],
    override val evalStatus: EvalStatus) extends SpecNode {

  def nodeType: NodeType.Value = NodeType.Step

  def isExpanded(parent: Identifiable) = parent match {
    case scenario: Scenario => scenario.isExpanded
    case _ => false
  }
  
  def expression: String = docString map { case (_, content, _) =>
    val lines = content.split("""\r?\n""")
    s"""$name "${lines(0)}${if (lines.length > 1) "..." else ""}""""
  } getOrElse(name)

  /** Returns the given value if the step has no docString or the docString content otherwise. */
  def orDocString(value: String): String = docString.map(_._2).getOrElse(value)

  def hasDualColumnTable: Boolean = table.nonEmpty && table.head._2.size == 2

  /** Returns a string representation of this step. */
  override def toString: String = s"$keyword ${expression}"

  def copy(
      withSourceRef: Option[SourceRef] = sourceRef,
      withKeyword: String = keyword,
      withName: String = name,
      withAttachments: List[(String, File)] = attachments,
      withStepDef: Option[(Scenario, List[(String, String)])] = stepDef,
      withTable: List[(Int, List[String])] = table,
      withDocString: Option[(Int, String, Option[String])] = docString,
      withEvalStatus: EvalStatus = evalStatus): Step = {
    Step(withSourceRef, withKeyword, withName, withAttachments, withStepDef, withTable, withDocString, withEvalStatus)
  }

  lazy val errorTrails: List[List[Step]] = {
    if (EvalStatus.isError(evalStatus.status)) {
      stepDef map { case (sd, _) => 
        sd.allSteps.filter(step => EvalStatus.isError(step.evalStatus.status)).flatMap { step => 
          step.errorTrails map { trace => 
            this :: trace
          }
        }
      } getOrElse List(List(this))
    } else Nil
  }
}

object Step {
  def apply(uri: String, step: Cucumber.GherkinDocument.Feature.Step, index: Int): Step = {
    val dataTable = Option(step.getDataTable).map { dt =>
      dt.getRowsList.asScala.toList map { row =>
        (row.getLocation.getLine, row.getCellsList.asScala.toList.map(_.getValue))
      }
    } getOrElse Nil
    val docString = Option(step.getDocString()).filter(_.getContent().trim.length > 0) map { ds =>
      (ds.getLocation.getLine, ds.getContent, Option(ds.getMediaType).filter(_.trim.length > 0))
    }
    Step(
      Option(step.getLocation).map(loc => SourceRef(uri, loc, index)),
      step.getKeyword.trim, 
      step.getText, 
      Nil, 
      None, 
      dataTable, 
      docString, 
      Pending)
  }
  def errorTrails(node: SpecNode): List[List[Step]] = node match {
    case b: Background => b.steps.flatMap(_.errorTrails)
    case s: Scenario => s.allSteps.flatMap(_.errorTrails)
    case e: Examples => e.allSteps.flatMap(_.errorTrails)
    case r: Rule => r.allSteps.flatMap(_.errorTrails)
    case s: Step => s.errorTrails
    case _ => Nil
  }
}
