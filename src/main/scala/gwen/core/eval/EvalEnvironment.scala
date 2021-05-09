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

package gwen.core.eval

import gwen.core._
import gwen.core.model._
import gwen.core.model.gherkin._
import gwen.core.model.state._

import scala.util.matching.Regex

import com.typesafe.scalalogging.LazyLogging

import java.io.File

/**
  * Base environment context providing access to all resources and state.
  * 
  * @author Branko Juric
  */
class EvalEnvironment() extends LazyLogging {
  
  private var state = new EnvState(new ScopedDataStack())

  val stateLevel: StateLevel.Value = GwenSettings.`gwen.state.level`

  def stepDefs = state.getStepDefs
  def stepScope = scopes.stepScope
  def scopes = state.scopes
  def topScope: TopScope = scopes.topScope

  /** Create a clone that preserves scoped data and step defs */
  def copy(): EvalEnvironment = {
    new EvalEnvironment() tap { env =>
      env.state = EnvState(topScope, Some(stepDefs))
    }
  }

  /**
    * Closes any resources associated with the evaluation context. This implementation
    * does nothing (but subclasses can override).
    */
  def close(): Unit = { }
  
  /** Resets the current context but does not close it so it can be reused. */
  def reset(level: StateLevel.Value): Unit = {
    logger.info(s"Resetting environment context")
    state = if (StateLevel.feature.equals(level)) {
      EnvState(topScope, None)
    } else {
      EnvState(topScope, Some(stepDefs))
    }
    
  }
    
  def asString: String = scopes.asString

  /** The spec type currently being evaluated. */
  def specType: SpecType.Value = topScope.getObject(SpecType.toString).map(_.asInstanceOf[SpecType.Value]).getOrElse(SpecType.Feature)
  
  /** Returns the current visible scopes. */  
  def visibleScopes: ScopedDataStack = scopes.visible
  
  /**
   * Filters all attributes in all scopes based on the given predicate.
   * 
   * @param pred the predicate filter to apply; a (name, value) => boolean function
   * @return a new Scoped data stack containing only the attributes accepted by the predicate; 
   */
  def filterAtts(pred: ((String, String)) => Boolean): ScopedDataStack = scopes.filterAtts(pred) 
  
  /**
    * Gets a named data scope (creates it if it does not exist)
    * 
    * @param name the name of the data scope to get (or create and get)
    */
  def addScope(name: String): ScopedData = scopes.addScope(name)
  
  /**
    * Adds a step definition to the context.
    * 
    * @param stepDef the step definition to add
    */
  def addStepDef(stepDef: Scenario): Scenario = { 
    state.addStepDef(stepDef)
  }

  /**
    * Adds a step definition to the context.
    * 
    * @param name the name
    * @param stepDef the step definition to add
    */
  def addStepDef(name: String, stepDef: Scenario): Scenario = {
    state.addStepDef(name, stepDef)
  }

  /**
    * Removes (unloads) the stepdef with the given name
    *
    * @param name the step def name
    * @return the removed step def
    */
  def removeStepDef(name: String): Scenario = {
    state.removeStepDef(name)
  }
  
  /**
    * Gets the executable step definition for the given expression (if there is
    * one).
    * 
    * @param expression the expression to match
    * @return the step definition if a match is found; false otherwise
    */
  def getStepDef(expression: String): Option[(Scenario, List[(String, String)])] = {
    stepDefs.get(expression) match {
      case None => getStepDefWithParams(expression)
      case Some(stepDef) => Some((stepDef, Nil))
    }
  }
  
  /**
    * Gets the paraterised step definition for the given expression (if there is
    * one).
    * 
    * @param expression the expression to match
    * @return the step definition and its parameters (name value tuples) if a 
    *         match is found; false otherwise
    */
  private def getStepDefWithParams(expression: String): Option[(Scenario, List[(String, String)])] = {
    val matches = stepDefs.values.view.flatMap { stepDef =>
      val pattern = Regex.quote(stepDef.name).replaceAll("<.+?>", """\\E(.*?)\\Q""").replaceAll("""\\Q\\E""", "")
      if (expression.matches(pattern)) {
        val names = "<.+?>".r.findAllIn(stepDef.name).toList
        names.groupBy(identity).collectFirst { case (n, vs) if vs.size > 1 =>
          Errors.ambiguousCaseError(s"$n parameter defined ${vs.size} times in StepDef '${stepDef.name}'")
        }
        val values = pattern.r.unapplySeq(expression).get
        val params = names zip values
        val resolved = params.foldLeft(stepDef.name) { (result, param) => result.replace(param._1, param._2) }
        if (expression == resolved) {
          Some((stepDef, params))
        } else None
      } else {
        None
      }
    }
    val iter = matches.iterator
    if (iter.hasNext) {
      val first = Some(iter.next())
      if (iter.hasNext) {
        val msg = s"Ambiguous condition in resolving '$expression': 1 StepDef match expected but ${matches.size} found"
        Errors.ambiguousCaseError(s"$msg: ${matches.map { case (stepDef, _) => stepDef.name }.mkString(",")}")
      } else first
    } else None
  }

  /** Adds current behavior. */
  def addBehavior(behavior: BehaviorType.Value): BehaviorType.Value = 
    behavior tap { _ => state.addBehavior(behavior) }

  /** Removes the current behavior. */
  def popBehavior(): Option[BehaviorType.Value] = state.popBehavior()

  /** Gets the current behavior. */
  def currentBehavior: Option[BehaviorType.Value] = state.currentBehavior

  /** Checks if a top level step is currently being evaluated). */
  def isEvaluatingTopLevelStep: Boolean = stepScope.isEmpty

  /**
    * Adds error attachments to the current context. This includes the error trace and environment context.
    * 
    * @param failure the failed status
    */
  def addErrorAttachments(failure: Failed): Unit = { 
    addAttachment("Error details", "txt", failure.error.writeStackTrace())
    addAttachment(s"Environment", "txt", scopes.visible.asString)
  }

  def addAttachment(name: String, extension: String, content: String): (String, File) = { 
    state.addAttachment(name, extension, content)
  }

  def addAttachment(name: String, file: File): (String, File) = { 
    state.addAttachment(name, file)
  }

  def popAttachments(): List[(String, File)] = state.popAttachments()

  def hasAttachments: Boolean = state.hasAttachments
  
}
