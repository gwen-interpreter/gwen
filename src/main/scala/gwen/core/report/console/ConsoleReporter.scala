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

object ConsoleReporter
    extends NodeEventListener("Console Reporter", Set(NodeType.Meta, NodeType.StepDef)) {

  private val printer = new SpecPrinter(deep = false, GwenSettings.`gwen.console.log.colors`)

  override def beforeUnit(event: NodeEvent[FeatureUnit]): Unit = { 
    val unit = event.source
    Console.println(("""|   _
                        |  { \," Executing """ + SpecType.Feature.toString.toLowerCase + """ file
                        | {_`/   """ + unit.name + """
                        |    `
                        |""").stripMargin)
  } 

  override def afterUnit(event: NodeEvent[FeatureUnit]): Unit = {
    val unit = event.source
    Console.println()
  }
    
  override def beforeSpec(event: NodeEvent[Spec]): Unit = {
    val spec = event.source
    Console.println(printer.prettyPrint(spec.feature))
  }

  override def afterSpec(event: NodeEvent[SpecResult]): Unit = {
    val result = event.source
  }

  override def beforeBackground(event: NodeEvent[Background]): Unit = {
    val background = event.source
    Console.println(printer.prettyPrint(background))
  }

  override def afterBackground(event: NodeEvent[Background]): Unit = {
    event.callChain.nodes.reverse.find { node => 
      node.isInstanceOf[Scenario]
    } map { node => 
      node.asInstanceOf[Scenario]
    } foreach { scenario =>
      Console.println(printer.prettyPrint(scenario))
    }
  }

  override def beforeScenario(event: NodeEvent[Scenario]): Unit = {
    val scenario = event.source
    if (scenario.background.isEmpty) {
      Console.println(printer.prettyPrint(scenario))
    }
  }

  override def afterScenario(event: NodeEvent[Scenario]): Unit = {
    val scenario = event.source
  }

  override def beforeExamples(event: NodeEvent[Examples]): Unit = {
    val examples = event.source
    Console.println(printer.prettyPrint(examples))
  }

  override def afterExamples(event: NodeEvent[Examples]): Unit = {
    val examples = event.source
  }

  override def beforeRule(event: NodeEvent[Rule]): Unit = {
    val rule = event.source
    Console.println(printer.prettyPrint(rule))
  }

  override def afterRule(event: NodeEvent[Rule]): Unit = {
    val rule = event.source
  }

  override def beforeStepDef(event: NodeEvent[Scenario]): Unit = { }

  override def afterStepDef(event: NodeEvent[Scenario]): Unit = { }

  override def beforeStep(event: NodeEvent[Step]): Unit = {
    val step = event.source
    Console.print(printer.prettyPrint(step))
  }

  override def afterStep(event: NodeEvent[Step]): Unit = {
    val step = event.source
    Console.println(printer.printStatus(step))
  }
  
}
