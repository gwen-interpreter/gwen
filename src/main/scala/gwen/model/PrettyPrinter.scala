/*
 * Copyright 2014-2017 Branko Juric, Brady Wood
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

package gwen.model

import gwen._
import gwen.model.gherkin._

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
object prettyPrint {

  def apply(spec: Specification): String = {
    val sw = new StringWriter()
    new PrettyPrinter(spec, new PrintWriter(sw)).walk()
    sw.toString
  }

}

class PrettyPrinter(spec: Specification, out: PrintWriter) extends SpecWalker(spec) {

  override def onFeature(parent: Identifiable, feature: Feature): Unit = { 
    val language = feature.language
    if (language != "en") {
      out.println(s"# language: $language")
      out.println()
    }
    printTags("   ", feature.tags)
    out.print(s"   ${feature.keyword}: ${feature.name}")
    printTextLines(feature.description)
    out.println()
  }

  override def onBackground(parent: Identifiable, background: Background): Unit = { 
    out.println()
    out.print(s"${background.keyword}: ${background.name}")
    printTextLines(background.description)
    printStatus(background.evalStatus)
    out.println()
  }

  override def onScenario(parent: Identifiable, scenario: Scenario): Unit = { 
    if (!scenario.isExpanded) {
      val keyword = scenario.keyword
      out.println()
      printTags(paddingFor(keyword), scenario.tags)
      out.print(paddingFor(keyword))
      out.print(s"${keyword}: ${scenario.name}")
      printTextLines(scenario.description)
      printStatus(scenario.evalStatus)
      out.println()
    }
  }

  override def onStep(parent: Identifiable, step: Step): Unit = { 
    if (!step.isExpanded(parent)) {
      val keyword = step.keyword
      val table = step.table
      val docString = step.docString
      out.print("  ")
      out.print(rightJustify(keyword.toString))
      out.print(s"${keyword} ${step.name}")
      printStatus(step.evalStatus)
      if (table.nonEmpty) {
          val rows = table.indices.toList.map { rowIndex => Formatting.formatTableRow(table, rowIndex) }
          printTextLines(rows)
      } else if (docString.nonEmpty) {
          printTextLines(Formatting.formatDocString(docString.get).split("""\r?\n""").toList)
      }
      out.println()
    }
  }

  override def onRule(parent: Identifiable, rule: Rule): Unit = {
    out.println()
    out.print(s"      ${rule.keyword}: ${rule.name}")
    printTextLines(rule.description)
    out.println()
  }
  
  override def onExamples(parent: Identifiable, examples: Examples): Unit = {
    if (!examples.isExpanded) {
      printTags("  ", examples.tags)
      out.print(s"  ${examples.keyword}: ${examples.name}")
      printTextLines(examples.description)
      val table = examples.table
      val rows = table.indices.toList.map { rowIndex => Formatting.formatTableRow(table, rowIndex) }
      printTextLines(rows)
      out.println()
    }
  }
  
  private def printTextLines(lines: List[String]): Unit = {
    lines foreach { line =>
      out.println()
      out.print(s"            $line")
    }
  }
  
  private def printTags(indent: String, tags: List[Tag]): Unit = { 
    if (tags.nonEmpty) {
      out.println(s"$indent${tags.mkString(" ")}")
    }
  }
  
  private def printStatus(status: EvalStatus): Unit = {
    status match {
      case Failed(_, error) => out.print(s" # $status: ${error.getMessage}")
      case Pending => // noop
      case _ => print(s" # $status")
    }
  }
  
  private def rightJustify(keyword: String) = " " * (9 - keyword.length)

  private def paddingFor(keyword: String) = " "*(10 - keyword.split(' ')(0).length + (if (keyword.contains(' ')) 1 else 0))

}
