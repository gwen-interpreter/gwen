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
import gwen.core.state.StateLevel

import java.io.PrintStream
import java.io.ByteArrayOutputStream
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.ConcurrentMap
 
class ConsoleReporter(options: GwenOptions)
    extends NodeEventListener("Console Reporter", Set(NodeType.Meta)) {

  private val parallel = options.parallel
  private val printer = new SpecPrinter(deep = false, verbatim = false, ConsoleColors.isEnabled)
  
  private var loadingStepDef = ThreadLocal.withInitial[Boolean] { () => false }
  private var depth = ThreadLocal.withInitial[Int] { () => 0 }
  private val parallelOut: Option[ThreadLocal[(ByteArrayOutputStream, PrintStream)]] = {
    if (parallel) {
      Some(
        ThreadLocal.withInitial[(ByteArrayOutputStream, PrintStream)] { () => 
          val baos = new ByteArrayOutputStream()
          (baos, new PrintStream(baos))
        }
      )
    } else None
  }
  private val parallelScenarioCache: Option[ConcurrentMap[String, List[(Long, String)]]] = {
    if (parallel && StateLevel.isScenario) {
      Some(new ConcurrentHashMap[String, List[(Long, String)]]())
    } else None
  }

  private def out: PrintStream = parallelOut.map(_.get._2).getOrElse(System.out)
  
  override def beforeUnit(event: NodeEvent[FeatureUnit]): Unit = { 
    val unit = event.source
    val action = if (options.dryRun) "Checking" else "Executing"
    if (parallel) {
      System.out.println(s"[${Thread.currentThread.getName}] $action ${SpecType.Feature.toString.toLowerCase} specification: ${unit.displayName}")
    } else {
      System.out.println(("""|   _
                             |  { \," """ + action + " " + SpecType.Feature.toString.toLowerCase + """ specification:
                             | {_`/   """ + unit.displayName + """
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
                  | {_`/   """ + unit.displayName + """
                  |    `
                  |
                  |""").stripMargin + outBuffer.toString
            )
          } finally {
            outStream.close()
          }
          val baos = new ByteArrayOutputStream()
          threadLocal.set((baos, new PrintStream(baos)))
        }
      }
    }
  }
    
  override def beforeSpec(event: NodeEvent[Spec]): Unit = {
    val spec = event.source
    val parent = event.callChain.previous
    out.println(printer.prettyPrint(parent, spec.feature))
    parallelScenarioCache foreach { cache =>
      cache.putIfAbsent(spec.uuid, Nil)
    }
  }

  override def afterSpec(event: NodeEvent[SpecResult]): Unit = {
    val result = event.source
    parallelScenarioCache foreach { cache =>
      event.callChain.nodes.find(_.isInstanceOf[Spec]).map(_.asInstanceOf[Spec]) foreach { spec =>
        out.print(cache.remove(spec.uuid) sortBy { (line, output) => line } map { (_, output) => output } mkString "")
      }
    }
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
      parallelScenarioCache foreach {_ =>
        val action = if (options.dryRun) "Checking" else "Executing"
        val unit = event.callChain.nodes.find(_.isInstanceOf[FeatureUnit]).map(_.asInstanceOf[FeatureUnit])
        val parent = event.callChain.previous
        val scenarioNo = scenario.indexIn(parent).map(_ + 1)
        System.out.println(s"[${Thread.currentThread.getName}] $action ${SpecType.Feature.toString.toLowerCase}${unit.map(u => s" specification: ${u.displayName}").getOrElse("")} scenario${scenarioNo.map(n => s"[$n]").getOrElse("")}: ${scenario.name}")
      }
      if (scenario.background.isEmpty && !scenario.isExpanded) {
        val parent = event.callChain.previous
        out.println(printer.prettyPrint(parent, scenario))
      }
    }
  }

  override def afterScenario(event: NodeEvent[Scenario]): Unit = {  
    parallelScenarioCache foreach { cache =>
      parallelOut foreach { threadLocal =>
        val (outBuffer, outStream) = threadLocal.get
        outStream.flush()
        event.callChain.nodes.find(_.isInstanceOf[Spec]).map(_.asInstanceOf[Spec]) foreach { spec =>
          try {
            cache.synchronized {
              cache.put(spec.uuid, (spec.sourceRef.map(_.line).getOrElse(0), outBuffer.toString) :: cache.get(spec.uuid))
            }
          } finally {
            outStream.close()
          }
        }
        val baos = new ByteArrayOutputStream()
        threadLocal.set((baos, new PrintStream(baos)))
      }
    }
  }

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

  override def beforeStepDef(event: NodeEvent[Scenario]): Unit = { 
    loadingStepDef.set(event.callChain.previous.isInstanceOf[Spec])
  }

  override def afterStepDef(event: NodeEvent[Scenario]): Unit = {
    loadingStepDef.set(false)
  }

  override def beforeStep(event: NodeEvent[Step]): Unit = {
    if (!loadingStepDef.get) {
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
  }

  override def afterStep(event: NodeEvent[Step]): Unit = {
    if (!loadingStepDef.get) {
      if (depth.get <= GwenSettings.`gwen.console.log.depth`) {
        val step = event.source
        val parent = event.callChain.previous
        if (depth.get == GwenSettings.`gwen.console.log.depth` || step.stepDef.isEmpty || step.stepDef.map(_.steps.isEmpty).getOrElse(false)) {
          out.println(printer.printStatus(step, withMessage = true))
        }
      }
      depth.set(depth.get - 1)
    }
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
