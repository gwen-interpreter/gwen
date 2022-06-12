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
import gwen.core.result.ResultsSummary
import gwen.core.result.SpecResult

import java.io.PrintStream
import java.io.ByteArrayOutputStream

class ConsoleReporter(options: GwenOptions)
    extends NodeEventListener("Console Reporter", Set(NodeType.Meta)) {

  private val parallel = options.parallel || options.parallelFeatures
  private val printer = new SpecPrinter(false, ConsoleColors.isEnabled)
  
  private var depth = ThreadLocal.withInitial[Int] { () => 0 }
  private val parallelOut = {
    if (parallel) {
      Some(
        ThreadLocal.withInitial[(ByteArrayOutputStream, PrintStream)] { () => 
          val baos = new ByteArrayOutputStream()
          (baos, new PrintStream(baos))
        }
      )
    } else None
  }

  private def out: PrintStream = parallelOut.map(_.get._2).getOrElse(System.out)
  
  override def beforeUnit(event: NodeEvent[FeatureUnit]): Unit = { 
    val unit = event.source
    val action = if (options.dryRun) "Checking" else "Executing"
    if (parallel) {
      System.out.println(s"[${Thread.currentThread.getName}] $action ${SpecType.Feature.toString.toLowerCase} specification: ${unit.name}")
    } else {
      System.out.println(("""|   _
                             |  { \," """ + action + " " + SpecType.Feature.toString.toLowerCase + """ specification:
                             | {_`/   """ + unit.name + """
                             |    `
                             |""").stripMargin)
    }
  } 

  override def afterUnit(event: NodeEvent[FeatureUnit]): Unit = {
    if (!parallel) {
      System.out.println()
    } else {
      val unit = event.source
      val parent = event.callChain.previous
      val action = if (options.dryRun) "Checked" else "Executed"
      unit.result foreach { result =>
        parallelOut foreach { threadLocal =>
          val (outBuffer, outStream) = threadLocal.get
          outStream.flush()
          try {
            System.out.println(
              ("""|   _
                  |  { \," [""" + Thread.currentThread.getName + "] " + action + " " + SpecType.Feature.toString.toLowerCase + """ specification:
                  | {_`/   """ + unit.name + """
                  |    `
                  |
                  |""").stripMargin + outBuffer.toString
            )
          } finally {
            outStream.close()
          }
        }
      }
    }
  }
    
  override def beforeSpec(event: NodeEvent[Spec]): Unit = {
    val spec = event.source
    val parent = event.callChain.previous
    out.println(printer.prettyPrint(parent, spec.feature))
  }

  override def afterSpec(event: NodeEvent[SpecResult]): Unit = {
    val result = event.source
    out.print(printer.printSpecResult(result))
  }

  override def beforeBackground(event: NodeEvent[Background]): Unit = {
    if (depth.get == 0) {
      val background = event.source
      val parent = event.callChain.previous
      out.println(printer.prettyPrint(parent, background))
    }
  }

  override def afterBackground(event: NodeEvent[Background]): Unit = {
    if (depth.get == 0) {
      event.callChain.nodes.reverse.find { node => 
        node.isInstanceOf[Scenario]
      } map { node => 
        node.asInstanceOf[Scenario]
      } foreach { scenario =>
        if (!scenario.isExpanded) {
          val parent = event.callChain.previous
          out.println(printer.prettyPrint(parent, scenario))
        }
      }
    }
  }

  override def beforeScenario(event: NodeEvent[Scenario]): Unit = {
    if (depth.get == 0) {
      val scenario = event.source
      if (scenario.background.isEmpty && !scenario.isExpanded) {
        val parent = event.callChain.previous
        out.println(printer.prettyPrint(parent, scenario))
      }
    }
  }

  override def afterScenario(event: NodeEvent[Scenario]): Unit = {  }

  override def beforeExamples(event: NodeEvent[Examples]): Unit = {
    if (depth.get == 0) {
      val examples = event.source
      val parent = event.callChain.previous
      out.print(printer.prettyPrint(parent, examples))
    }
  }

  override def afterExamples(event: NodeEvent[Examples]): Unit = {  }

  override def beforeRule(event: NodeEvent[Rule]): Unit = {
    val rule = event.source
    val parent = event.callChain.previous
    out.println(printer.prettyPrint(parent, rule))
  }

  override def afterRule(event: NodeEvent[Rule]): Unit = {  }

  override def beforeStepDef(event: NodeEvent[Scenario]): Unit = {  }
  override def afterStepDef(event: NodeEvent[Scenario]): Unit = {  }

  override def beforeStep(event: NodeEvent[Step]): Unit = {
    depth.set(depth.get + 1)
    if (depth.get <= GwenSettings.`gwen.console.log.depth`) {
      val step = event.source
      val parent = event.callChain.previous
      if (step.indexIn(parent).getOrElse(0) == 0) { 
        out.println()
      }
      val indent = s"${" " * ((depth.get  - 1) * (StepKeyword.maxLength + 1))}"
      val stepString = printer.prettyPrint(parent, step).linesIterator.map(line => s"$indent$line") mkString(System.lineSeparator)
      out.print(stepString)
    }
  }

  override def afterStep(event: NodeEvent[Step]): Unit = {
    if (depth.get <= GwenSettings.`gwen.console.log.depth`) {
      val step = event.source
      val parent = event.callChain.previous
      if (depth.get == GwenSettings.`gwen.console.log.depth` || step.stepDef.isEmpty) {
        out.println(printer.printStatus(step, withMessage = true))
      }
    }
    depth.set(depth.get - 1)
  }

  def printSummary(summary: ResultsSummary): Unit = {
    if (summary.results.size > 1) {
      System.out.println(printer.printSummary(summary))
    }
    val reports = summary.reports
    if (reports.nonEmpty) {
      System.out.println()
      val maxWidh = (reports map { (format, _) => format.toString.length }).max
      reports foreach { (format, report) => 
        System.out.println(s"${Formatting.leftPad(s"${format.toString.toUpperCase} report", maxWidh + 7)}  $report")
      }
      System.out.println()
    }
  }
  
}
