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

 package gwen.core.model.state

import gwen.core._
import gwen.core.model.BehaviorType
import gwen.core.model.StepKeyword
import gwen.core.model.gherkin.Scenario

import java.util.concurrent.atomic.AtomicInteger
import java.io.File

class EnvState(val scopes: ScopedDataStack) {

  /** Loaded step defs. */
  private var stepDefs = Map[String, Scenario]()

   /** List of temporarily cached attachments (number-name-file tripples). */
  private var attachments: List[(Int, String, File)] = Nil

  /** Stack of behaviors. */
  private var behaviors = List[BehaviorType.Value]()

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
  def addBehavior(behavior: BehaviorType.Value): Unit = {
    behaviors = behavior :: behaviors
  }

  /** Removes the behavior at the top of the stack. */
  def popBehavior(): Option[BehaviorType.Value] = behaviors match {
    case head::tail => 
      behaviors = tail
      Some(head)
    case _ =>
      None
  }

  /** Gets the behavior at the top of the stack. */
  def currentBehavior: Option[BehaviorType.Value] = behaviors.headOption

}

object EnvState {

  private var attachmentCounter = new AtomicInteger(0)

  def apply(): EnvState = {
    new EnvState(new ScopedDataStack())
  }
  
  def apply(topScope: TopScope, stepDefs:  Option[Map[String, Scenario]]): EnvState = {
    new EnvState(new ScopedDataStack()) tap { newState => 
      topScope.implicitAtts foreach { case (n, v) => 
        newState.scopes.topScope.set(n, v)
      }
      stepDefs foreach { sdefs =>
        newState.stepDefs = sdefs
      }
    }
  }

  /** Gets the next attachment number. */
  def nextAttachmentNo() = attachmentCounter.incrementAndGet()

  /** Resets the attachment number to zero. */
  def resetAttachmentNo(): Unit = {
    attachmentCounter = new AtomicInteger(0)
  }

}
