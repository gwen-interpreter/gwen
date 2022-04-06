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
import gwen.core.node.GwenNode
import gwen.core.node.NodeType
import gwen.core.node.SourceRef
import gwen.core.state.EnvState
import gwen.core.status._

import scala.jdk.CollectionConverters._

import io.cucumber.messages.{ types => cucumber }

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
  * @param params optional step parameters
  * @param callerParams optional caller parameters
  * @param tags list of optional tags/annotations
  * @param message: optional overriding error message
  */
case class Step(
    sourceRef: Option[SourceRef],
    keyword: String,
    name: String,
    attachments: List[(String, File)],
    stepDef: Option[Scenario],
    table: List[(Long, List[String])],
    docString: Option[(Long, String, Option[String])],
    override val evalStatus: EvalStatus,
    override val params: List[(String, String)],
    override val callerParams: List[(String, String)],
    tags: List[Tag],
    message: Option[String]) extends GherkinNode {

  override val nodeType: NodeType = NodeType.Step

  override def siblingsIn(parent: GwenNode): List[GwenNode] = {
    parent match {
      case background: Background => background.steps
      case scenario: Scenario => scenario.steps
      case _ => Nil
    }
  }

  def isExpanded(parent: GwenNode) = parent match {
    case scenario: Scenario => scenario.isExpanded
    case _ => false
  }
  
  def expression: String = docString map { case (_, content, _) =>
    val lines = content.split("""\r?\n""")
    s"""$name "${lines(0)}${if (lines.length > 1) "..." else ""}""""
  } getOrElse(name)

  def deepSteps: List[Step] = {
    List(this) ++ (stepDef map { sd => 
      sd.steps.flatMap(_.deepSteps)
    } getOrElse Nil)
  }

  def deepAttachments: List[(String, File)] = {
    val dSteps = deepSteps
    if (deepSteps.size > 1) {
      deepSteps.filter(!_.evalStatus.isIgnored).flatMap(_.attachments)
    } else this.attachments
  }

  /** Returns the given value if the step has no docString or the docString content otherwise. */
  def orDocString(value: String): String = docString.map(_._2).getOrElse(value)

  def docStringify: Option[Step] = {
    name match {
      case r"""^(?s)(.*)$prefix"\$$<(.+?)$param>"$$""" if docString.isEmpty =>
        Some(
          this.copy(
            withName = prefix.trim,
            withDocString = Some((0, s"<$param>", None))
          )
        )
      case _ => None
    }
  }

  def hasDualColumnTable: Boolean = table.nonEmpty && table.head._2.size == 2
  def printableTags: List[Tag] = tags.filter(_.name.toLowerCase != Annotations.Breakpoint.toString.toLowerCase)

  /** Returns a string representation of this step. */
  override def toString: String = s"$keyword ${expression}"

  def copy(
      withSourceRef: Option[SourceRef] = sourceRef,
      withKeyword: String = keyword,
      withName: String = name,
      withAttachments: List[(String, File)] = attachments,
      withStepDef: Option[Scenario] = stepDef,
      withTable: List[(Long, List[String])] = table,
      withDocString: Option[(Long, String, Option[String])] = docString,
      withEvalStatus: EvalStatus = evalStatus,
      withParams: List[(String, String)] = params,
      withCallerParams: List[(String, String)] = callerParams,
      withTags: List[Tag] = tags,
      withMessage: Option[String] = message): Step = {
    Step(withSourceRef, withKeyword, withName, withAttachments, withStepDef, withTable, withDocString, withEvalStatus, withParams, withCallerParams, withTags, withMessage)
  }

  /**
   * Add an attachment.
   * 
   * @param name the attachment name
   * @param extension the filename extension
   * @param content the content to write to the file
   */
  def addAttachment(name: String, extension: String, content: String): Step = { 
    val file = File.createTempFile(s"${Formatting.padWithZeroes(EnvState.nextAttachmentNo())}-", s".$extension")
    file.deleteOnExit()
    Option(content) foreach { file.writeText }
    addAttachment((name, file))
  }

  /**
   * Add a file attachment.
   * 
   * @param name the attachment name
   * @param file the file to attach
   */
  def addAttachment(name: String, file: File): Step = { 
    addAttachment(EnvState.nextAttachmentNo(), name, file)
  }

  /**
   * Add a file attachment.
   * 
   * @param attachmentNo the attachment number
   * @param name the attachment name
   * @param file the file to attach
   */
  def addAttachment(attachmentNo: Int, name: String, file: File): Step = { 
    val fileCopy = file.copyToFile(File.createTempFile(s"${Formatting.padWithZeroes(attachmentNo)}-${file.simpleName}-", s".${file.extension}"))
    fileCopy.deleteOnExit()
    addAttachment((name, fileCopy))
  }

  /**
    * Adds an attachment
    * 
    * @param attachment the attachment (name-file pair) to add
    */
  private def addAttachment(attachment: (String, File)): Step = {
    this.copy(
      withAttachments = (attachment :: this.attachments) sortBy { case (_, file) => file.getName }
    )
  }

  lazy val errorTrails: List[List[Step]] = {
    if (evalStatus.isError) {
      stepDef map { sd => 
        sd.allSteps.filter(step => step.evalStatus.isError).flatMap { step => 
          step.errorTrails map { trace => 
            this :: trace
          }
        }
      } getOrElse List(List(this))
    } else Nil
  }

  def withCallerParams(caller: GwenNode): Step = {
    val names = callerParams map { case (n, _) => n }
    caller match {
      case scenario: Scenario => 
        scenario.cumulativeParams filter { case (name, _) => 
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

  def isBreakpoint: Boolean = hasTag(Annotations.Breakpoint)
  def isFinally: Boolean = hasTag(Annotations.Finally)
  def loadStrategy: Option[LoadStrategy] = { 
    if (isEager) Some(LoadStrategy.Eager)
    else if (isLazy) Some(LoadStrategy.Lazy)
    else None
  }
  def isEager: Boolean = hasTag(Annotations.Eager)
  def isLazy: Boolean = hasTag(Annotations.Lazy)
  def isTry: Boolean = hasTag(Annotations.Try)
  private def hasTag(tag: Annotations) = tags.exists(_.name.toLowerCase == tag.toString.toLowerCase)

}

object Step {
  def apply(file: Option[File], step: cucumber.Step): Step = {
    val dataTable = Option(step.getDataTable).map { dt =>
      dt.getRows.asScala.toList map { row =>
        (Long2long(row.getLocation.getLine), row.getCells.asScala.toList.map(_.getValue))
      }
    } getOrElse Nil
    val docString = Option(step.getDocString()).filter(_.getContent().trim.length > 0) map { ds =>
      (Long2long(ds.getLocation.getLine), ds.getContent, Option(ds.getMediaType).filter(_.trim.length > 0))
    }
    val (name, tagList, message): (String, List[Tag], Option[String]) = {
      val (n, t) = step.getText.trim match {
        case r"""((?:@\w+\s+)+)$ts(.*)$name""" => 
          (name, ts.split("\\s+").toList.map(n => Tag(n.trim)))
        case _ => (step.getText, Nil)
      }
      n match {
        case r"""(.+)$s1\s+@Message\("(.+)$m"\)\s*(.*)?$s2""" => (s"${s1.trim} ${Option(s2).map(_.trim).getOrElse("")}", t, Some(s"${Formatting.ZeroChar}$m${Formatting.ZeroChar}"))
        case r"""(.+)$s1: "(.+)$m"\s*(.*)?$s2""" if !m.contains('"') => (s"${s1.trim} ${Option(s2).map(_.trim).getOrElse("")}", t, Some(s"${Formatting.ZeroChar}$m${Formatting.ZeroChar}"))
        case _ => (n, t, None)
      }
    }
    Step(
      Option(step.getLocation).map(loc => SourceRef(file, loc)),
      step.getKeyword.trim, 
      name.trim, 
      Nil, 
      None, 
      dataTable, 
      docString, 
      Pending,
      Nil,
      Nil,
      tagList,
      message)
  }
  def errorTrails(node: GwenNode): List[List[Step]] = node match {
    case b: Background => b.steps.flatMap(_.errorTrails)
    case s: Scenario => s.allSteps.flatMap(_.errorTrails)
    case e: Examples => e.allSteps.flatMap(_.errorTrails)
    case r: Rule => r.allSteps.flatMap(_.errorTrails)
    case s: Step => s.errorTrails
    case _ => Nil
  }
  
  def validate(steps: List[Step]): List[Step] = {
    steps.lastOption foreach { lastStep =>
      steps.filter(_.isFinally) foreach { step => 
        if (step != lastStep) {
          Errors.illegalStepAnnotationError(step, "@Finally permitted only in last step of parent node")
        }
      }
    }
    steps.filter(s => s.isEager || s.isLazy) foreach { step => 
      if (!step.name.matches(".+ (is|will be) defined .*by ((?!property|setting).)+"))  {
        val annotation = { 
          if (step.isEager) Annotations.Eager
          else if (step.isLazy) Annotations.Lazy
          else Annotations.Deferred
        }
        Errors.illegalStepAnnotationError(step, s"@$annotation annotation permitted only for '<x> defined by <y>' DSL steps")
      } else {
        val annotations = {
          (if (step.isEager) List(Annotations.Eager) else Nil) ++
          (if (step.isLazy) List(Annotations.Lazy) else Nil)
        }
        if (annotations.size > 1) {
          Errors.illegalStepAnnotationError(step, s"Only one of ${annotations.map(a => s"@$a").mkString(", ")} annotation permitted for step")
        }
      }
    }
    steps
  }
  
}
