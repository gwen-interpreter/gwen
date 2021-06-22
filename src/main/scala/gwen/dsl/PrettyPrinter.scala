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

package gwen.dsl

import gwen._

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

  def apply(spec: FeatureSpec): String = {
    apply(spec.feature) +
      formatStatus(spec.evalStatus) +
      spec.background.map(apply).getOrElse("") +
      printAll(spec.scenarios.map(apply), "", "") +
      printAll(spec.rules.map(apply), "", "")
  }
  
  /**
    * Prints an [[SpecNode]] to a string.  This method is implicitly 
    * called whenever `prettyPrint(specNode)` is called.  There is no need to 
    * call this method explicitly.   
    *
    * @param node the AST node to print
    */
  def apply(node: SpecNode): String = node match {
    case Feature(language, _, tags, keyword, name, description) =>
      s"${if (language != "en") s"# language: $language\n\n" else ""}${formatTags("   ", tags)}   $keyword: $name${formatTextLines(description)}"
    case background @ Background(_, keyword, name, description, steps) =>
      s"\n\n$keyword: $name${formatTextLines(description)}${formatStatus(background.evalStatus)}\n" + printAll(steps.map(apply), "  ", "\n")
    case scenario @ Scenario(_, tags, keyword, name, description, background, steps, examples, _, _) =>
      background.map(apply).getOrElse("") +
        (if (scenario.isOutline && scenario.examples.flatMap(_.scenarios).nonEmpty) "" else s"\n\n${formatTags(paddingFor(keyword), tags)}${paddingFor(keyword)}${scenario.keyword}: $name${formatTextLines(description)}${formatStatus(scenario.evalStatus)}\n" + printAll(steps.map(apply), "  ", "\n")) +
        printAll(examples.map(apply), "", "\n")
    case rule @ Rule(_, keyword, name, description, background, scenarios) =>
      s"\n\n      $keyword: $name${formatTextLines(description)}" +
        background.map(apply).getOrElse("") + 
        printAll(scenarios.map(apply), "", "")
    case Step(_, keyword, name, _, _, table, docString, evalStatus, _, _) =>
      rightJustify(keyword.toString) + s"$keyword $name${formatStatus(evalStatus)}" + s"${if (table.nonEmpty)
        formatTextLines(table.indices.toList.map { rowIndex => Formatting.formatTableRow(table, rowIndex) })
      else if (docString.nonEmpty)
        formatTextLines(Formatting.formatDocString(docString.get).split("""\r?\n""").toList)
      else ""}"
    case Examples(_, tags, keyword, name, description, table, scenarios) => scenarios match {
      case Nil =>
        s"\n${formatTags("  ", tags)}  $keyword: $name${formatTextLines(description)}${formatTextLines(table.indices.toList.map { rowIndex => Formatting.formatTableRow(table, rowIndex) })}"
      case _ =>
        scenarios.map(apply).mkString("")
    }
  }
  
  private def formatTextLines(lines: List[String]): String =
    lines.map { line =>
      s"\n            $line"
    }.mkString
  
  /**
    * Formats the given tags to a string.
    * 
    * @param tags the tags to format
    */
  private def formatTags(indent: String, tags: List[Tag]):String = tags match {
    case _ :: _ =>  s"$indent${tags.mkString(" ")}\n"
    case _ => ""
  }
  
  private def formatStatus(status: EvalStatus): String = status match {
    case Pending => ""
    case Failed(_, error) => s" # $status: ${error.getMessage}"
    case _ => s" # $status"
  }
  
  /**
    * Right justifies a step keyword by padding spaces in front of it. This
    * ensures that when steps are listed one after the other, their
    * clauses line up.
    */
  private def rightJustify(keyword: String) = " " * (9 - keyword.length)
  
  /**
    * Prints a list of nodes, giving each node a prefix and separator.
    * 
    * @param nodes the list of nodes to print
    * @param prefix the prefix to append the printed node to
    * @param separator the string separating each printed node
    */
  private def printAll[T](nodes: List[T], prefix: String, separator: String) =
    nodes.map(prefix + _) mkString separator

  private def paddingFor(keyword: String) = " "*(10 - keyword.split(' ')(0).length + (if (keyword.contains(' ')) 1 else 0))

}
