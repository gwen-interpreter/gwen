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
    new StringWriter() tap { sw =>
      new PrettyPrinter(spec).walk(Root, new PrintWriter(sw))
    } toString
  }

}

class PrettyPrinter(spec: Specification) extends SpecWalker[PrintWriter](spec) {

  override def onFeature(parent: Identifiable, feature: Feature, out: PrintWriter): PrintWriter = { 
    val language = feature.language
    if (language != "en") {
      out.println(s"# language: $language")
      out.println()
    }
    printTags("   ", feature.tags, out)
    out.print(s"   ${feature.keyword}: ${feature.name}")
    printDescription(feature.description, out)
    out.println()
    out
  }

  override def onBackground(parent: Identifiable, background: Background, out: PrintWriter): PrintWriter = { 
    out.println()
    out.print(s"${background.keyword}: ${background.name}")
    printDescription(background.description, out)
    printStatus(background.evalStatus, out)
    out.println()
    out
  }

  override def onScenario(parent: Identifiable, scenario: Scenario, out: PrintWriter): PrintWriter = { 
    if (!scenario.isExpanded) {
      val keyword = scenario.keyword
      out.println()
      printTags(paddingFor(keyword), scenario.tags, out)
      out.print(paddingFor(keyword))
      out.print(s"${keyword}: ${scenario.name}")
      printDescription(scenario.description, out)
      printStatus(scenario.evalStatus, out)
      out.println()
    }
    out
  }

  override def onStep(parent: Identifiable, step: Step, out: PrintWriter): PrintWriter = { 
    if (!step.isExpanded(parent)) {
      val keyword = step.keyword
      out.print("  ")
      out.print(rightJustify(keyword.toString))
      out.print(s"${keyword} ${step.name}")
      printStatus(step.evalStatus, out)
      if (step.table.nonEmpty) {
        printTable(step.table, out)
      } else {
        step.docString foreach { docString => 
          printDocString(docString, out)
        }
      }
      out.println()
    }
    out
  }

  override def onRule(parent: Identifiable, rule: Rule, out: PrintWriter): PrintWriter = { 
    out.println()
    out.print(s"      ${rule.keyword}: ${rule.name}")
    printDescription(rule.description, out)
    out.println()
    out
  }
  
  override def onExamples(parent: Identifiable, examples: Examples, out: PrintWriter): PrintWriter = { 
    if (!examples.isExpanded) {
      printTags("  ", examples.tags, out)
      out.print(s"  ${examples.keyword}: ${examples.name}")
      printDescription(examples.description, out)
      printTable(examples.table, out)
      out.println()
    }
    out
  }
  
  private def printDescription(desc: List[String], out: PrintWriter): Unit = {
    printTextLines("        ", desc, out)
  }

  private def printTable(table: List[(Int, List[String])], out: PrintWriter): Unit = {
    printTextLines("            ", Formatting.splitLines(Formatting.formatTable(table)), out)
  }

  private def printDocString(docString: (Int, String, Option[String]), out: PrintWriter): Unit = {
    printTextLines("            ", Formatting.splitLines(Formatting.formatDocString(docString)), out)
  }

  private def printTextLines(indent: String, lines: List[String], out: PrintWriter): Unit = {
    lines foreach { line =>
      out.println()
      out.print(s"$indent$line")
    }
  }
  
  private def printTags(indent: String, tags: List[Tag], out: PrintWriter): Unit = { 
    if (tags.nonEmpty) {
      out.println(s"$indent${tags.mkString(" ")}")
    }
  }
  
  private def printStatus(status: EvalStatus, out: PrintWriter): Unit = {
    status match {
      case Failed(_, error) => out.print(s" # $status: ${error.getMessage}")
      case Pending => // noop
      case _ => out.print(s" # $status")
    }
  }
  
  private def rightJustify(keyword: String) = " " * (9 - keyword.length)

  private def paddingFor(keyword: String) = " "*(10 - keyword.split(' ')(0).length + (if (keyword.contains(' ')) 1 else 0))

}
