/*
 * Copyright 2021 Branko Juric, Brady Wood
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

package gwen.core.report.console

import gwen._
import gwen.core._
import gwen.core.node._
import gwen.core.node.gherkin._
import gwen.core.node.event.NodeEvent
import gwen.core.node.event.NodeEventListener
import gwen.core.result.SpecResult

import scala.Console
import gwen.core.status.StatusKeyword
import gwen.core.result.ResultsSummary

class ConsoleReporter(options: GwenOptions)
    extends NodeEventListener("Console Reporter", Set(NodeType.Meta, NodeType.StepDef)) {

  private val parallel = options.parallel || options.parallelFeatures
  private val printer = new SpecPrinter(parallel, GwenSettings.`gwen.console.log.colors`)

  override def beforeUnit(event: NodeEvent[FeatureUnit]): Unit = { 
    val unit = event.source
    val action = if (options.dryRun) "Checking" else "Executing"
    if (parallel) {
      Console.println(s"""[""" + Thread.currentThread.getName + "] " + action + " " + SpecType.Feature.toString.toLowerCase + ": " + unit.name + """
                          |""".stripMargin)
    } else {
      Console.println(("""|   _
                          |  { \," """ + action + " " + SpecType.Feature.toString.toLowerCase + """:
                          | {_`/   """ + unit.name + """
                          |    `
                          |""").stripMargin)
    }
  } 

  override def afterUnit(event: NodeEvent[FeatureUnit]): Unit = {
    if (!parallel) {
      Console.println()
    } else {
      val unit = event.source
      val action = if (options.dryRun) "Checked" else "Executed"
      unit.result foreach { result =>
        Console.println(
          ("""|   _
              |  { \," [""" + Thread.currentThread.getName + "] " + action + " " + SpecType.Feature.toString.toLowerCase + """:
              | {_`/   """ + unit.name + """
              |    `
              |
              |""").stripMargin + printer.prettyPrint(result.spec) + printer.printSpecResult(result)
        )
      }
    }
  }
    
  override def beforeSpec(event: NodeEvent[Spec]): Unit = {
    if (!parallel) {
      val spec = event.source
      Console.println(printer.prettyPrint(spec.feature))
    }
  }

  override def afterSpec(event: NodeEvent[SpecResult]): Unit = {
    if (!parallel) {
      val result = event.source
      Console.print(printer.printSpecResult(result))
    }
  }

  override def beforeBackground(event: NodeEvent[Background]): Unit = {
    if (!parallel) {
      val background = event.source
      Console.println(printer.prettyPrint(background))
    }
  }

  override def afterBackground(event: NodeEvent[Background]): Unit = {
    if (!parallel) {
      event.callChain.nodes.reverse.find { node => 
        node.isInstanceOf[Scenario]
      } map { node => 
        node.asInstanceOf[Scenario]
      } foreach { scenario =>
        Console.println(printer.prettyPrint(scenario))
      }
    }
  }

  override def beforeScenario(event: NodeEvent[Scenario]): Unit = {
    if (!parallel) {
      val scenario = event.source
      if (scenario.background.isEmpty) {
        Console.println(printer.prettyPrint(scenario))
      }
    }
  }

  override def afterScenario(event: NodeEvent[Scenario]): Unit = {  }

  override def beforeExamples(event: NodeEvent[Examples]): Unit = {
    if (!parallel) {
      val examples = event.source
      Console.println(printer.prettyPrint(examples))
    }
  }

  override def afterExamples(event: NodeEvent[Examples]): Unit = {  }

  override def beforeRule(event: NodeEvent[Rule]): Unit = {
    if (!parallel) {
      val rule = event.source
      Console.println(printer.prettyPrint(rule))
    }
  }

  override def afterRule(event: NodeEvent[Rule]): Unit = {  }

  override def beforeStepDef(event: NodeEvent[Scenario]): Unit = { }
  override def afterStepDef(event: NodeEvent[Scenario]): Unit = { }

  override def beforeStep(event: NodeEvent[Step]): Unit = {
    if (!parallel) {
      val step = event.source
      Console.print(printer.prettyPrint(step))
    }
  }

  override def afterStep(event: NodeEvent[Step]): Unit = {
    if (!parallel) {
      val step = event.source
      Console.println(printer.printStatus(step, withMessage = true))
    }
  }

  def printSummary(summary: ResultsSummary): Unit = {
    if (summary.results.size > 1) {
      Console.println(printer.printSummary(summary))
    }
    val reports = summary.reports
    if (reports.nonEmpty) {
      val maxWidh = (reports map { (format, _) => format.toString.length }).max
      reports foreach { (format, report) => 
        Console.println(s"${Formatting.leftPad(s"${format.toString.toUpperCase} report", maxWidh + 7)}  $report")
      }
      Console.println()
    }
  }
  
}
