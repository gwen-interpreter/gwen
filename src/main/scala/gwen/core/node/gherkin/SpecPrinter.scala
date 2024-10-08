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

import gwen.core.node._
import gwen.core.status._
import gwen.core.result.SpecResult
import gwen.core.result.ResultsSummary

import org.fusesource.jansi.Ansi._

import scala.concurrent.duration.Duration
import scala.util.chaining._
import scala.util.Try


import java.io.PrintWriter
import java.io.StringWriter
import java.util.Date
/**
  * Pretty prints a spec node to a string.  This object recursively prints
  * each node to a string and can be invoked as a function.  For example,
  * `prettyPrint(spec)` prints an entire spec, and `prettyPrint(step)` prints
  * a single step.  Included in the output is the evaluation status of node
  * (if not pending).
  *
  * @author Branko Juric
  */
object SpecPrinter {

  val TagsColor = Color.CYAN
  val ClauseColor = Color.MAGENTA

}

class SpecPrinter(deep: Boolean, verbatim: Boolean, colors: Boolean) extends SpecWalker[PrintWriter](deep, verbatim) {

  private val inRule = ThreadLocal.withInitial[Boolean] { () => false }

  def prettyPrint(parent: GwenNode, node: GwenNode): String = {
    new StringWriter() tap { sw =>
      walk(parent, node, new PrintWriter(sw))
    } toString
  }

  override def onFeature(parent: GwenNode, feature: Feature, out: PrintWriter): PrintWriter = {
    inRule.set(false)
    val language = feature.language
    if (language != "en") {
      out.println(s"# language: $language")
      out.println()
    }
    printTags("", feature.tags, out)
    out.print(s"${if (colors) ansi.bold.fg(SpecPrinter.ClauseColor) else ""}${feature.keyword}:${if (colors) ansi.reset else ""}${if(feature.displayName.isEmpty) "" else s" ${feature.displayName}"}")
    if (deep || feature.description.nonEmpty) out.println()
    printDescription("  ", feature.description, out)
    out
  }

  override def onBackground(parent: GwenNode, background: Background, out: PrintWriter): PrintWriter = {
    val indent = indentFor(background)
    out.println()
    out.print(s"$indent${if (colors) ansi.bold.fg(SpecPrinter.ClauseColor) else ""}${background.keyword}:${if (colors) ansi.reset else ""}${if(background.displayName.isEmpty) "" else s" ${background.displayName}"}")
    if (deep || background.description.nonEmpty) out.println()
    printDescription(s"$indent  ", background.description, out)
    if (deep && background.description.nonEmpty && background.steps.nonEmpty) {
      out.println()
    }
    out
  }

  override def onScenario(parent: GwenNode, scenario: Scenario, out: PrintWriter): PrintWriter = {
    if (!scenario.isExpanded) {
      val indent = indentFor(scenario)
      val keyword = scenario.keyword
      out.println()
      printTags(indent, scenario.tags, out)
      out.print(s"$indent${if (colors) ansi.bold.fg(SpecPrinter.ClauseColor) else ""}${keyword}:${if (colors) ansi.reset else ""}${if(scenario.displayName.isEmpty) "" else s" ${scenario.displayName}"}")
      if (deep || scenario.description.nonEmpty) out.println()
      printDescription(s"$indent  ", scenario.description, out)
      if (deep && scenario.description.nonEmpty && scenario.steps.nonEmpty) {
        out.println()
      }
    }
    out
  }

  override def onStep(parent: GwenNode, step: Step, out: PrintWriter): PrintWriter = {
    if (!step.isExpanded(parent)) {
      val keyword = step.keyword
      val keywordMaxLength = StepKeyword.maxLength
      val indent = indentFor(step)
      val tags = filterStepTags(step)
      out.print(s"$indent${if (colors) ansi.bold else ""}${Formatting.leftPad(keyword, keywordMaxLength)}${if (colors) ansi.reset else ""} ${if (tags.nonEmpty) s"${if (colors) ansi.fg(SpecPrinter.TagsColor) else ""}${stringifyTags(tags)} ${if (colors) ansi.reset else ""}" else ""}${step.displayName}")
      if (verbatim && (step.message.nonEmpty || step.dryValues.nonEmpty)) {
        val max = step.siblingsIn(parent) match { 
          case Nil => 0
          case siblings => siblings.map(_.asInstanceOf[Step]).filter(s => s.message.nonEmpty || s.dryValues.nonEmpty).map(s => stepExpressionLength(filterStepTags(s), s)).max
        }
        val padding = " " * (max - stepExpressionLength(tags, step))
        step.message foreach { msg =>
          val msgTag = s"""@${Annotations.Message}(${Formatting.surroundWithQuotesForAnnotation(msg)})"""
          out.print(s"    $padding${if (colors) ansi.fg(SpecPrinter.TagsColor) else ""}${msgTag}${if (colors) ansi.reset else ""}")
        }
        val dryValues = step.dryValues
        if (dryValues.nonEmpty) {
          val dvs = dryValues.groupBy(_._1).toList map { (n, vs) => 
            val vss = vs.map(_._2)
            if (vss.size == 1) {
              val v = vss.head
              s"""name=${Formatting.surroundWithQuotesForAnnotation(n)},value=${Formatting.surroundWithQuotesForAnnotation(v)}"""
            }
            else s"""name=${Formatting.surroundWithQuotesForAnnotation(n)},value=[${vss.map(v => Formatting.surroundWithQuotesForAnnotation(v)).mkString(",")}]"""
          } mkString(", ")
          val dryRunTag = s"""@${Annotations.DryRun}($dvs)"""
          out.print(s"    $padding${if (colors) ansi.fg(SpecPrinter.TagsColor) else ""}${dryRunTag}${if (colors) ansi.reset else ""}")
        }
      }
      if (step.table.nonEmpty) {
        out.println()
        printTable(s"$indent ${" " * keywordMaxLength}", step.table, out)
      } else {
        step.docString foreach { docString =>
          out.println()
          printDocString(s"$indent ${" " * keywordMaxLength}", docString, out)
        }
      }
      if (deep) out.println(printStatus(step, Some(step.evalStatus.message), withIcon = true, withStatusIcon = false))
    }
    out
  }

  private def filterStepTags(step: Step): List[Tag] = {
    if (verbatim) step.tags.filter(_.name != Annotations.Message.toString) else step.printableTags
  }

  private def stringifyTags(tags: List[Tag]): String = tags.map(_.toString).mkString(" ")

  private def stepExpressionLength(tags: List[Tag], step: Step): Int = {
    s"${if (tags.nonEmpty) s"${stringifyTags(tags)} " else ""}${step.displayName}".length
  }

  override def onRule(parent: GwenNode, rule: Rule, out: PrintWriter): PrintWriter = {
    val indent = indentFor(rule)
    if (deep) out.println()
    printTags(indent,rule.tags, out)
    out.print(s"$indent${if (colors) ansi.bold.fg(SpecPrinter.ClauseColor) else ""}${rule.keyword}:${if (colors) ansi.reset else ""}${if(rule.name.isEmpty) "" else s" ${rule.name}"}")
    if (deep || rule.description.nonEmpty) out.println()
    printDescription(s"$indent  ", rule.description, out)
    inRule.set(true)
    out
  }

  override def onExamples(parent: GwenNode, examples: Examples, out: PrintWriter): PrintWriter = {
    if (!examples.isExpanded) {
      val indent = indentFor(examples)
      if (deep || examples.table.size == 1) out.println()
      printTags(indent, examples.tags, out)
      out.print(s"$indent${if (colors) ansi.bold.fg(SpecPrinter.ClauseColor) else ""}${examples.keyword}:${if (colors) ansi.reset else ""}${if(examples.name.isEmpty) "" else s" ${examples.name}"}")
      if (deep || examples.description.nonEmpty) out.println()
      printDescription(s"$indent  ", examples.description, out)
      if (examples.description.nonEmpty && examples.table.nonEmpty) {
        out.println()
      }
      if (examples.table.nonEmpty) {
        printTable(s"$indent    ", examples.table, out)
        if (deep) out.println()
      }
    }
    out
  }

  private def printDescription(indent: String, desc: List[String], out: PrintWriter): Unit = {
    if (desc.nonEmpty) {
      out.println()
      printTextLines(indent, desc, out)
      if (deep) out.println()
    }
  }

  private def printTable(indent: String, table: List[(Long, List[String])], out: PrintWriter): Unit = {
    printTextLines(indent, Formatting.splitLines(Formatting.formatTable(table)), out)
  }

  private def printDocString(indent: String, docString: (Long, String, Option[String]), out: PrintWriter): Unit = {
    printTextLines(indent, Formatting.splitLines(Formatting.formatDocString(docString)), out)
  }

  private def printTextLines(indent: String, lines: List[String], out: PrintWriter): Unit = {
    lines.zipWithIndex foreach { (line, idx) =>
      out.print(s"$indent$line")
      if (idx < (lines.length - 1)) out.println()
    }
  }

  private def printTags(indent: String, tags: List[Tag], out: PrintWriter): Unit = {
    if (tags.nonEmpty) {
      val tagsByLine = tags.groupBy(_.sourceRef.map(_.line).getOrElse(0L))
      val lines = tags.map(_.sourceRef.map(_.line).getOrElse(0L)).distinct.sorted
      lines foreach { line => 
        val tagline = tagsByLine(line) map { tag => 
          s"${if (colors) ansi.fg(SpecPrinter.TagsColor) else ""}$tag${if (colors) ansi.reset else ""}"
        } mkString (" ")
        out.println(s"$indent$tagline")
      }
    }
  }

  def printStatus(node: GwenNode, message: Option[String], withIcon: Boolean, withStatusIcon: Boolean): String = {
    val indent = indentForStatus(node)
    printStatus(indent, node.evalStatus, message, withIcon, withStatusIcon)
    
  }

  def printStatus(indent: String, status: EvalStatus, message: Option[String], withIcon: Boolean, withStatusIcon: Boolean): String = {
    val sw = new StringWriter()
    val pw = new PrintWriter(sw)
    printStatus(indent, status, message, withIcon, withStatusIcon, pw)
    sw.toString
  }

  private def printStatus(indent: String, status: EvalStatus, message: Option[String], withIcon: Boolean, withStatusIcon: Boolean, out: PrintWriter): Unit = {
    val keywordString = if (status.isAbstained) "Abstained" else status.keyword.toString
    val statusString = {
      if (!status.isAbstained) {
        if (withStatusIcon) {
          status.asStatusIconString(keywordString)
        } else if (withIcon) {
          status.asIconString(keywordString)
        } else {
          status.asString(keywordString)
        }
      } else {
        status.asString(keywordString)
      }
    }
    status match {
      case _: Failed => out.print(s"$indent${if (colors) ansi.fg(colorFor(status)) else ""}$statusString${message.map(msg => s"\n$msg").getOrElse("")}${if (colors) ansi.reset else ""}")
      case _: Sustained => out.print(s"$indent${if (colors) ansi.fg(colorFor(status)) else ""}$statusString${message.map(msg => s"\n$msg").getOrElse("")}${if (colors) ansi.reset else ""}")
      case _: Passed => out.print(s"$indent${if (colors) ansi.fg(colorFor(status)) else ""}$statusString${if (colors) ansi.reset else ""}")
      case Loaded => out.print(s"$indent${if (colors) ansi.fg(colorFor(status)) else ""}$statusString${if (colors) ansi.reset else ""}")
      case Pending => // No-op
      case _ => out.print(s"$indent${if (colors) ansi.fg(colorFor(status)) else ""}$statusString${if (colors) ansi.reset else ""}")
    }
  }

  def printSpecResult(result: SpecResult): String = {
    printSpecResult(result.started, result.finished, result.elapsedTime, result.evalStatus, result.statusCounts(withEmpty = false), Some(result.message))
  }

  private def printSpecResult(started: Date, finished: Date, elapsedTime: Duration, evalStatus: EvalStatus, statusCounts: List[(NodeType, Map[StatusKeyword, Int])], message: Option[String]): String = {
    val sw = new StringWriter()
    val pw = new PrintWriter(sw)
    val header: List[String] = "" :: StatusKeyword.reportables.map(_.toString)
    val details: List[List[String]] = statusCounts map { (node, counts) => 
      val sum = counts.values.sum
      s"$sum ${node}${if (sum == 1) "" else "s"}" :: (
        StatusKeyword.reportables map { keyword => 
          counts.get(keyword).map(_.toString).getOrElse("-")
        }
      )
    }
    val table = header :: details
    val widths = table.transpose.map(_.map(_.length).max + 2)
    pw.println()
    table.zipWithIndex foreach { (row, rowIndex)  => 
      pw.println(
        ((row zip widths).zipWithIndex) map { case ((cell, width), colIndex) => 
          val column = Formatting.leftPad(cell, width)
          if (colors) {
            if (rowIndex == 0) {
              if (colIndex == 0 || header(colIndex) == evalStatus.keyword.toString) {
                s"${ansi.bold.fg(colorFor(evalStatus))}$column${ansi.reset}"
              } else column
            } else if (colIndex > 0 && cell != "-") {
              s"${ansi.fg(colorFor(StatusKeyword.valueOf(header(colIndex))))}$column${ansi.reset}"
            } else column
          } else column
        } mkString
      )
    }
    pw.println()
    pw.println(s"${Formatting.leftPad("Started", widths(0))}  $started")
    pw.println(s"${Formatting.leftPad("Finished", widths(0))}  $finished")
    pw.println(s"${Formatting.leftPad("Elapsed", widths(0))}  ${Formatting.formatDuration(elapsedTime)}")
    pw.println()
    printStatus("  ", evalStatus, message, withIcon = false, withStatusIcon = true, pw)
    if (deep) pw.println()
    sw.toString
  }

  def printSummary(summary: ResultsSummary): String = {
    val sw = new StringWriter()
    val pw = new PrintWriter(sw)
    val status = summary.evalStatus.keyword
    pw.println()
    pw.println("Summary:")
    pw.println()
    val resultsByStatus = StatusKeyword.reportables.reverse flatMap { keyword => 
      val results = summary.results.filter(_.evalStatus.keyword == keyword)
      if (results.nonEmpty) Some(keyword, results) else None
    }
    val messages = resultsByStatus.zipWithIndex.foldLeft(List[String]()) { case (acc, ((keyword, results), idx)) => 
      if (idx > 0) pw.println()
      val widths = List(
        Try(results.map(r => printStatus(r.spec, None, withIcon = false, withStatusIcon = true)).map(_.length).max).getOrElse(0),
        Try(results.map(_.spec.feature.displayName).map(_.length).max).getOrElse(0),
        Try(results.map(_.spec.uri).map(_.length).max).getOrElse(0)
      )
      val msgs: List[String] = results flatMap { result =>
        val spec = result.spec
        pw.println(s"  ${Formatting.leftPad(printStatus(spec, None, withIcon = false, withStatusIcon = true), widths(0))}  ${Formatting.rightPad(spec.feature.displayName, widths(1))}  ${Formatting.rightPad(spec.uri, widths(2))}")
        if (result.evalStatus.isError) Some(result.message)
        else None
      }
      acc ++ msgs
    }
    pw.println(printSpecResult(summary.started, summary.finished, summary.elapsedTime, summary.evalStatus, summary.statusCounts(withEmpty = false), Some(messages.mkString("\n"))))
    sw.toString
  }

  private def indentForStatus(node: GwenNode): String = {
    node match {
      case _: Step => "  "
      case _ => indentFor(node)
    }
  }

  private def indentFor(node: GwenNode): String = {
    val indents = node match {
      case _: Background => if (inRule.get) 4 else 2
      case _: Scenario => if (inRule.get) 4 else 2
      case _: Step => if (inRule.get) 6 else 4
      case _: Examples => if (inRule.get) 6 else 4
      case _: Rule =>  2
      case _ => 0
    }
    " " * indents
  }

  private def colorFor(status: EvalStatus): Color = {
    status match {
      case _: Passed if status.isAbstained => Color.CYAN
      case _ => colorFor(status.keyword)
    }
  }

  private def colorFor(keyword: StatusKeyword): Color = {
    keyword match {
      case StatusKeyword.Failed => Color.RED
      case StatusKeyword.Sustained => Color.YELLOW
      case StatusKeyword.Skipped => Color.YELLOW
      case StatusKeyword.Passed => Color.GREEN
      case StatusKeyword.Loaded => Color.GREEN
      case _ => Color.CYAN
    }
  }

}
