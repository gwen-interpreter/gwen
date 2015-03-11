/*
 * Copyright 2014 Branko Juric, Brady Wood
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

import java.io.File

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

  /**
    * Prints an [[SpecNode]] to a string.  This method is implicitly 
    * called whenever `prettyPrint(specNode)` is called.  There is no need to 
    * call this method explicitly.   
    *
    * @param node the AST node to print
    */
  def apply(node: SpecNode): String = node match {
    case spec @ FeatureSpec(feature, background, scenarios,  _, _) =>
      apply(feature) +
        formatStatus(spec.evalStatus) +
        background.map(apply).getOrElse("") +
        printAll(scenarios.map(apply), "", "")
    case Feature(tags, name, narrative) =>
      if (narrative.isEmpty) {
        s"${formatTags("   ", tags)}   Feature: ${name}"
      } else {
        val narrativeLines = narrative.map {line =>
          val words = line.split(" ")
          val keyword = s"${words(0)} ${words(1)}"
          val expression = words.drop(2).mkString(" ")
          s"\n${rightJustify(keyword)}  ${keyword} ${expression}"
        }
        s"${formatTags("   ", tags)}   Feature: ${name}${narrativeLines.mkString}"
      }
    case background @ Background(description, steps) =>
      s"\n\nBackground: ${description}${formatStatus(background.evalStatus)}\n" + printAll(steps.map(apply), "  ", "\n")
    case scenario @ Scenario(tags, description, background, steps) =>
      background.map(apply).getOrElse("") +
      s"\n\n${formatTags("  ", tags)}  Scenario: ${description}${formatStatus(scenario.evalStatus)}\n" + printAll(steps.map(apply), "  ", "\n")
    case Step(keyword, expression, evalStatus, attachments) =>
      rightJustify(keyword.toString) + s"${keyword} ${expression}${formatStatus(evalStatus)}"
  }
  
  /**
    * Formats the given tags to a string.
    * 
    * @param tags the tags to format
    */
  private def formatTags(indent: String, tags: Set[Tag]):String = tags.toList match {
    case _ :: _ =>  s"${indent}${tags.mkString(" ")}\n"
    case _ => ""
  }
  
  private def formatStatus(status: EvalStatus): String = status match {
    case Pending => ""
    case Failed(_, error) => s" # ${status}: ${error.getMessage()}" 
    case _ => s" # ${status}"
  }
  
  /**
    * Right justifies a step keyword by padding spaces in front of it. This
    * ensures that when steps are listed one after the other, their
    * clauses line up.
    */
  private def rightJustify(keyword: String) = (" " * (9  - keyword.length))
  
  /**
    * Prints a list of nodes, giving each node a prefix and separator.
    * 
    * @param nodes the list of nodes to print
    * @param prefix the prefix to append the printed node to
    * @param separator the string separating each printed node
    */
  private def printAll[T](nodes: List[T], prefix: String, separator: String) =
    nodes.map(prefix + _) mkString(separator)

}
