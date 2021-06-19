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

 package gwen.core.state

import gwen.core._
import gwen.core.node.GwenNode
import gwen.core.node.NodeChain
import gwen.core.node.NodeChainBuilder
import gwen.core.node.gherkin.Scenario
import gwen.core.node.gherkin.StepKeyword
import gwen.core.behavior.BehaviorType

import scala.util.chaining._

import java.util.concurrent.atomic.AtomicInteger
import java.io.File

class EnvState(val scopes: ScopedDataStack) {

  /** Loaded step defs. */
  private var stepDefs = Map[String, Scenario]()

   /** List of temporarily cached attachments (number-name-file tripples). */
  private var attachments: List[(Int, String, File)] = Nil

  /** Stack of behaviors. */
  private var behaviors = List[BehaviorType]()

  /** Current node chain builder. */
  private var nodeBuilder = new NodeChainBuilder()

  /** Provides access to step defs. */
  def getStepDefs = stepDefs

  /**
    * Adds a step definition to the context.
    *
    * @param stepDef the step definition to add
    */
  def addStepDef(stepDef: Scenario): Scenario = {
    addStepDef(stepDef.name, stepDef)
  }

  /**
    * Adds a step definition to the context.
    *
    * @param name the name
    * @param stepDef the step definition to add
    */
  def addStepDef(name: String, stepDef: Scenario): Scenario = {
    StepKeyword.names foreach { keyword =>
      if (stepDef.name.startsWith(keyword)) Errors.invalidStepDefError(stepDef, s"name cannot start with $keyword keyword")
    }
    stepDef tap { _ =>
      stepDefs += (name -> stepDef)
    }
  }

  /**
    * Removes (unloads) the stepdef with the given name
    *
    * @param name the step def name
    * @return the removed step def
    */
  def removeStepDef(name: String): Scenario = {
    stepDefs(name) tap { stepDef =>
      stepDefs -= name
    }
  }

  /** Returns current attachments before clearing them. */
  def popAttachments(): List[(Int, String, File)] = attachments tap { _ =>
    attachments = Nil
  }

  /**
    * Adds an attachment
    *
    * @param attachment the attachment (name-file pair) to add
    */
  def addAttachment(name: String, file: File): Unit = {
    attachments = (EnvState.nextAttachmentNo(), name, file) :: attachments
  }

  /** Adds the given behavior to the top of the stack. */
  def addBehavior(behavior: BehaviorType): Unit = {
    behaviors = behavior :: behaviors
  }

  /** Removes the behavior at the top of the stack. */
  def popBehavior(): Option[BehaviorType] = behaviors match {
    case head::tail =>
      behaviors = tail
      Some(head)
    case _ =>
      None
  }

  /** Gets the behavior at the top of the stack. */
  def currentBehavior: Option[BehaviorType] = behaviors.headOption

  /** Pushes a node onto the node chain.*/
  def pushNode(node: GwenNode): NodeChain = nodeBuilder.push(node)

  /** Pops a node off the node chain.*/
  def popNode(): (GwenNode, NodeChain) = nodeBuilder.pop()

  /** Gets the current call chain. */
  def nodeChain: NodeChain = nodeBuilder.nodeChain

}

object EnvState {

  private var attachmentCounter = new AtomicInteger(0)

  def apply(): EnvState = {
    new EnvState(new ScopedDataStack())
  }

  def apply(topScope: TopScope, stepDefs:  Option[Map[String, Scenario]], nodeChain: NodeChain): EnvState = {
    new EnvState(new ScopedDataStack()) tap { newState =>
      topScope.implicitAtts foreach { case (n, v) =>
        newState.scopes.topScope.set(n, v)
      }
      stepDefs foreach { sdefs =>
        newState.stepDefs = sdefs
      }
      newState.nodeBuilder = NodeChainBuilder(nodeChain)
    }
  }

  /** Gets the next attachment number. */
  def nextAttachmentNo() = attachmentCounter.incrementAndGet()

  /** Resets the attachment number to zero. */
  def resetAttachmentNo(): Unit = {
    attachmentCounter = new AtomicInteger(0)
  }

}
