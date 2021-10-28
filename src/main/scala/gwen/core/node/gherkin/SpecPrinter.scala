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

import gwen.core.node._
import gwen.core.status._

import scala.io.AnsiColor
import scala.util.chaining._

import java.io.PrintWriter
import java.io.StringWriter

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

  val TagsColor = AnsiColor.CYAN
  val ClauseColor = AnsiColor.MAGENTA

  def prettyPrint(node: GwenNode): String = {
    new StringWriter() tap { sw =>
      new SpecPrinter(deep = true, colors = false).walk(Root, node, new PrintWriter(sw))
    } toString
  }

}

class SpecPrinter(deep: Boolean, colors: Boolean) extends SpecWalker[PrintWriter](deep) {

  private val inRule = ThreadLocal.withInitial[Boolean] { () => false }

  def prettyPrint(node: GwenNode): String = {
    new StringWriter() tap { sw =>
      walk(Root, node, new PrintWriter(sw))
    } toString
  }

  override def onFeature(parent: GwenNode, feature: Feature, out: PrintWriter): PrintWriter = {
    val language = feature.language
    if (language != "en") {
      out.println(s"# language: $language")
      out.println()
    }
    printTags("", feature.tags, out)
    out.print(s"${if (colors) SpecPrinter.ClauseColor else ""}${feature.keyword}:${if (colors) AnsiColor.RESET else ""} ${feature.name}")
    if (deep) out.println()
    printDescription("  ", feature.description, out)
    out
  }

  override def onBackground(parent: GwenNode, background: Background, out: PrintWriter): PrintWriter = {
    val indent = indentFor(background)
    out.println()
    out.print(s"$indent${if (colors) SpecPrinter.ClauseColor else ""}${background.keyword}:${if (colors) AnsiColor.RESET else ""} ${background.name}")
    if (deep) out.println()
    printDescription(s"$indent  ", background.description, out)
    if (background.description.nonEmpty && background.steps.nonEmpty) {
      out.println()
    }
    //printStatus(background.evalStatus)
    out
  }

  override def onScenario(parent: GwenNode, scenario: Scenario, out: PrintWriter): PrintWriter = {
    if (!scenario.isExpanded) {
      val indent = indentFor(scenario)
      val keyword = scenario.keyword
      out.println()
      printTags(indent, scenario.tags, out)
      out.print(s"$indent${if (colors) SpecPrinter.ClauseColor else ""}${keyword}:${if (colors) AnsiColor.RESET else ""} ${scenario.name}")
      if (deep) out.println()
      printDescription(s"$indent  ", scenario.description, out)
      if (scenario.description.nonEmpty && scenario.steps.nonEmpty) {
        out.println()
      }
      //printStatus(scenario.evalStatus)
    }
    out
  }

  override def onStep(parent: GwenNode, step: Step, out: PrintWriter): PrintWriter = {
    if (!step.isExpanded(parent)) {
      val indent = indentFor(step)
      val keyword = step.keyword
      out.print(s"$indent${if (colors) AnsiColor.BOLD else ""}${rightJustify(keyword)}${if (colors) AnsiColor.RESET else ""} ${step.name}")
      if (deep || step.table.nonEmpty) out.println()
      //printStatus(step.evalStatus)
      if (step.table.nonEmpty) {
        printTable(s"$indent${keywordIndent}", step.table, out)
      } else {
        step.docString foreach { docString =>
          printDocString(s"$indent${keywordIndent}", docString, out)
        }
      }
    }
    out
  }

  override def onRule(parent: GwenNode, rule: Rule, out: PrintWriter): PrintWriter = {
    val indent = indentFor(rule)
    if (deep) out.println()
    out.print(s"$indent${if (colors) SpecPrinter.ClauseColor else ""}${rule.keyword}:${if (colors) AnsiColor.RESET else ""} ${rule.name}")
    if (deep) out.println()
    printDescription(s"$indent  ", rule.description, out)
    inRule.set(true)
    out
  }

  override def onExamples(parent: GwenNode, examples: Examples, out: PrintWriter): PrintWriter = {
    if (!examples.isExpanded) {
      val indent = indentFor(examples)
      if (deep) out.println()
      printTags(indent, examples.tags, out)
      out.print(s"$indent${if (colors) SpecPrinter.ClauseColor else ""}${examples.keyword}:${if (colors) AnsiColor.RESET else ""} ${examples.name}")
      if (deep) out.println()
      printDescription(s"$indent  ", examples.description, out)
      if (examples.description.nonEmpty && examples.table.nonEmpty) {
        out.println()
      }
      if (examples.table.nonEmpty) {
        printTable(s"$indent    ", examples.table, out)
      }
    }
    out
  }

  private def printDescription(indent: String, desc: List[String], out: PrintWriter): Unit = {
    if (desc.nonEmpty) {
      out.println()
      printTextLines(indent, desc, out)
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
      if (deep || idx < (lines.length -1)) out.println()
    }
  }

  private def printTags(indent: String, tags: List[Tag], out: PrintWriter): Unit = {
    if (tags.nonEmpty) {
      out.println(s"$indent${tags.map(t => s"${if (colors) SpecPrinter.TagsColor else ""}$t${if (colors) AnsiColor.RESET else ""}").mkString(" ")}")
    }
  }

  def printStatus(node: GwenNode): String = {
    val sw = new StringWriter()
    val pw = new PrintWriter(sw)
    val indent = indentForStatus(node)
    printStatus(indent, node.evalStatus, pw)
    sw.toString
  }

  private def printStatus(indent: String, status: EvalStatus, out: PrintWriter): Unit = {
    status match {
      case Failed(_, error) => out.print(s"$indent${if (colors) colorFor(status) else ""}$status: ${error.getMessage}${if (colors) AnsiColor.RESET else ""}")
      case Sustained(_, error) => out.print(s"$indent${if (colors) colorFor(status) else ""}$status: ${error.getMessage}${if (colors) AnsiColor.RESET else ""}")
      case _: OK => out.print(s"$indent${if (colors) colorFor(status) else ""}$status${if (colors) AnsiColor.RESET else ""}")
      case Loaded => out.print(s"$indent${if (colors) colorFor(status) else ""}$status${if (colors) AnsiColor.RESET else ""}")
      case Pending => // noop
      case _ => out.print(s"$indent${if (colors) colorFor(status) else ""}$status${if (colors) AnsiColor.RESET else ""}")
    }
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

  private def colorFor(status: EvalStatus): String = {
    status match {
      case _: Failed => AnsiColor.RED
      case _: Sustained => AnsiColor.YELLOW
      case Skipped => AnsiColor.YELLOW
      case _: OK => AnsiColor.GREEN
      case Loaded => AnsiColor.GREEN
      case _ => AnsiColor.CYAN
    }
  }

  private def keywordIndent: String = " " * (StepKeyword.maxLength + 1)

  private def rightJustify(keyword: String) = s"${" " * (StepKeyword.maxLength - keyword.length)}$keyword"

}
