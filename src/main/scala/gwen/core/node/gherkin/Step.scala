/*
 * Copyright 2014-2023 Branko Juric, Brady Wood
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
import gwen.core.Formatting.DurationFormatter
import gwen.core.node.GwenNode
import gwen.core.node.NodeType
import gwen.core.node.SourceRef
import gwen.core.state.EnvState
import gwen.core.status._

import scala.io.Source
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._

import io.cucumber.messages.{ types => cucumber }

import java.io.File
import scala.concurrent.duration.Duration

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
  * @param dryValues optional dry values
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
    message: Option[String],
    dryValues: List[(String, String)]) extends GherkinNode {

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
      sd.allSteps.flatMap(_.deepSteps)
    } getOrElse Nil)
  }

  def deepAttachments: List[(String, File)] = deepAttachments { _ => true }

  def deepAttachments(includeFilter: String => Boolean): List[(String, File)] = {
    val dSteps = deepSteps
    if (deepSteps.size > 1) filterAttachments(deepSteps, includeFilter) else this.attachments
  }

  def childAttachments(includeFilter: String => Boolean): List[(String, File)] = {
    val childSteps = deepSteps.filter(_ != this)
    filterAttachments(childSteps, includeFilter)
  }

  private def filterAttachments(steps: List[Step], includeFilter: String => Boolean): List[(String, File)] = {
    steps.filter(!_.evalStatus.isIgnored).flatMap(s => s.attachments.filter(nf => includeFilter(nf._1)))
  }

  /** Returns the given value if the step has no docString or the docString content otherwise. */
  def orDocString(value: String): String = docString.map(_._2).getOrElse(value)

  /** If step expression ends with a quoted parameter or property, move that into a docstring to 
   * support mutliline interpolation. */
  def docStringify: Option[Step] = {
    name match {
      case r"""^(?s)(.*)$prefix"\$$<(.+?)$param>"$$""" if docString.isEmpty =>
        Some(
          this.copy(
            withName = prefix.trim,
            withDocString = Some((0, s"$$<$param>", None))
          )
        )
      case r"""^(?s)(.*)$prefix"\$$\{(.+?)$property\}"$$""" if docString.isEmpty =>
        Some(
          this.copy(
            withName = prefix.trim,
            withDocString = Some((0, s"$${$property}", None))
          )
        )
      case _ => None
    }
  }

  def hasDualColumnTable: Boolean = table.nonEmpty && table.head._2.size == 2
  def isHorizontalForEachTable = table.nonEmpty && stepDef.exists(_.isHorizontalTable) && stepDef.exists(_.isForEach)
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
      withMessage: Option[String] = message,
      withDryValues: List[(String, String)] = dryValues): Step = {
    Step(withSourceRef, withKeyword, withName, withAttachments, withStepDef, withTable, withDocString, withEvalStatus, withParams, withCallerParams, withTags, withMessage, withDryValues)
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

  def addAttachments(attachments: List[(Int, String, File)]): Step = {
    attachments.foldLeft(this) { case (accStep, (attachmentNo, name, file)) =>
      accStep.addAttachment(attachmentNo, name, file)
    }
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
  def isData: Boolean = hasTag(Annotations.Data)
  def isNoData: Boolean = hasTag(Annotations.NoData)
  def isIgnoreCase: Boolean = hasTag(Annotations.IgnoreCase)
  def isTrim: Boolean = hasTag(Annotations.Trim)
  def isMasked: Boolean = hasTag(Annotations.Masked)
  def timeoutOpt: Option[Duration] = parseDurationInAnnotation(Annotations.Timeout)
  def delayOpt: Option[Duration] = parseDurationInAnnotation(Annotations.Delay)
  
  private def parseDurationInAnnotation(annotation: Annotations): Option[Duration] = {
    val name = annotation.toString
    tags.find(t => t.name.toLowerCase().startsWith(name.toLowerCase())) flatMap { annot =>
      Option(annot).filter(_.name == name).flatMap(_.value).map(DurationFormatter.parse) getOrElse {
        Errors.illegalDurationAnnotationError(name, annot.toString)
      }
    }
  }
  
  def assertionMode: AssertionMode = {
    AssertionMode.values find { mode => 
      hasTag(mode.annotation)
    } getOrElse GwenSettings.`gwen.assertion.mode`
  }

  private def hasTag(tag: Annotations) = tags.exists(_.name.toLowerCase == tag.toString.toLowerCase)

  /**
    * Interpolate placeholder references in this step.
    *
    * @param interpolator the interpolator to use
    * @return the interpolated step
    */
  override def interpolate(interpolator: String => String): Step = {
    if (isData || isNoData) this else {
      val iTags = tags map { tag => 
        Tag(tag.sourceRef, interpolator.apply(tag.toString))
      }
      val iName = interpolator.apply(name)
      val iTable = table map { case (line, record) =>
        (line, record.map(cell => interpolator.apply(cell)))
      }
      val iDocString = docString map { case (line, content, contentType) =>
        (line, interpolator.apply(content), contentType)
      }
      val iParams = params map { (name, value) => (name, interpolator.apply(value)) }
      val iStep = if (iTags.diff(tags).nonEmpty || iName != name || iTable != table || iDocString != docString || iParams.mkString != params.mkString) {
        copy(
          withTags = iTags,
          withName = iName,
          withTable = iTable,
          withDocString = iDocString,
          withParams = iParams,
        )
      } else this
    
      /* if the interplotion resulted in multi line step expression and the step has a trailing and quoted parameter
        or property, then move that into a docString and interpolate again, otherwise report illegal substitution */
      
      if (Source.fromString(iStep.name).getLines().size > 1) {
        docStringify.map(_.interpolate(interpolator)) getOrElse {
          Errors.multilineSubstitutionError("Illegal multiline placeholder substitution detected")
        }
      } else iStep
    }
  }

  def interpolateMessage(interpolator: String => String): Step = {
    if (isData || isNoData) this else {
      message map { msg => 
        val iMessage = interpolator.apply(msg)
        if (iMessage != msg) copy(withMessage = Some(iMessage)) else this
      } getOrElse (this)
    }
  }

  def dryValue(name: String): Option[String] = dryValues.find(_._1 == name).map(_._2)

}

object Step {
  def apply(file: Option[File], step: cucumber.Step): Step = {
    val sourceRef = Option(step.getLocation).map(loc => SourceRef(file, loc))
    val dataTable = step.getDataTable.toScala map { dt =>
      dt.getRows.asScala.toList map { row =>
        (Long2long(row.getLocation.getLine), row.getCells.asScala.toList.map(c => Formatting.escapeNewLineChars(c.getValue)))
      }
    } getOrElse Nil
    val docString = step.getDocString.toScala.filter(_.getContent().trim.length > 0) map { ds =>
      (Long2long(ds.getLocation.getLine), ds.getContent, ds.getMediaType.toScala.filter(_.trim.length > 0))
    }
    val (name, tagList, message, dryValues): (String, List[Tag], Option[String], List[(String, String)]) = {
      val (n, t) = Formatting.escapeNewLineChars(step.getText.trim) match {
        case r"""((?:@\w+(?:\(\S+\))?\s+)+)$ts(.*)$name""" => 
          (name, ts.split("\\s+").toList.map(n => Tag(n.trim)))
        case _ => (Formatting.escapeNewLineChars(step.getText), Nil)
      }
      n match {
        case r"""(.+)$s1\s+@Message\((.+)$message\)\s*(.*)?$s2""" => 
          val msg = Tag.parseSingleValue(sourceRef, Annotations.Message, None, message)
          (s"${s1.trim} ${Option(s2).map(_.trim).getOrElse("")}", t, Some(msg), Nil)
        case r""".+\@Message.*""" => 
          Errors.illegalStepAnnotationError(sourceRef, """Invalid @Message annotation syntax. Expected @Message('<message>') or @Message("<message>")""")
        case r"""(.+)$s1\s+@DryRun\(\s*name\s*=(.+)$name\s*,\s*value\s*=(.+)$value\)\s*(.*)?$s2""" =>
          val dvName = Tag.parseSingleValue(sourceRef, Annotations.DryRun, Some("name"), name)
          val values = Tag.parseListValue(sourceRef, Annotations.DryRun, Some("value"), value)
          val dvs = values map { v => (dvName, v) }
          (s"${s1.trim} ${Option(s2).map(_.trim).getOrElse("")}", t, None, dvs)
        case r""".+@DryRun.*""" => 
          Errors.illegalStepAnnotationError(sourceRef, "Invalid @DryRun annotation syntax. Expected @DryRun(name = '<name>', value = '<value>') or @DryRun(name = '<name>', value = ['<value-1>', '<value-2>', '<value-N>'])")
        case _ => 
          (n, t, None, Nil)
      }
    }
    Step(
      sourceRef,
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
      message,
      dryValues)
  }
  def errorTrails(node: GwenNode): List[List[Step]] = node match {
    case s: Spec => s.steps.flatMap(_.errorTrails)
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
          Errors.illegalStepAnnotationError(step.sourceRef, "@Finally permitted only in last step of parent node")
        }
      }
    }
    steps.filter(s => s.isEager || s.isLazy) foreach { step => 
      if (!step.name.matches(".+ (is|will be) defined .*by .+"))  {
        val annotation = { 
          if (step.isEager) Annotations.Eager
          else if (step.isLazy) Annotations.Lazy
          else Annotations.Deferred
        }
        Errors.illegalStepAnnotationError(step.sourceRef, s"@$annotation annotation permitted only for '<x> defined by <y>' DSL steps")
      } else {
        val annotations = {
          (if (step.isEager) List(Annotations.Eager) else Nil) ++
          (if (step.isLazy) List(Annotations.Lazy) else Nil)
        }
        if (annotations.size > 1) {
          Errors.illegalStepAnnotationError(step.sourceRef, s"Only one of ${annotations.map(a => s"@$a").mkString(", ")} annotation permitted for step")
        }
      }
    }
    steps
  }
  
}
