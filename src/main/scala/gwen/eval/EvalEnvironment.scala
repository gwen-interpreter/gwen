/*
 * Copyright 2014-2020 Branko Juric, Brady Wood
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

package gwen.eval

import gwen._
import gwen.model._
import gwen.model.gherkin._
import gwen.model.state._

import scala.util.matching.Regex

import com.typesafe.scalalogging.LazyLogging

import java.io.File

/**
  * Base environment context providing access to all resources and state.
  * 
  * @author Branko Juric
  */
class EvalEnvironment() extends LazyLogging {
  
  private var stepDefs = Map[String, Scenario]()
  private var state = new EnvState(new ScopedDataStack())

  var loadedMeta: List[File] = Nil
  val stateLevel: StateLevel.Value = GwenSettings.`gwen.state.level`

  private[eval] def stepScope = scopes.stepScope

  def scopes = state.scopes
  def topScope: TopScope = scopes.topScope

  /** Create a clone that preserves scoped data. */
  def copy(): EvalEnvironment = {
    new EvalEnvironment() tap { env =>
      env.loadedMeta = loadedMeta
      env.stepDefs = stepDefs
      env.state = EnvState(topScope)
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
    state = EnvState(topScope)
    if (StateLevel.feature.equals(level)) {
      stepDefs = Map[String, Scenario]()
      loadedMeta = Nil
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
  def addStepDef(stepDef: Scenario): Unit = {
    StepKeyword.names foreach { keyword =>
      if (stepDef.name.startsWith(keyword)) Errors.invalidStepDefError(stepDef, s"name cannot start with $keyword keyword")
    }
    val tags = stepDef.tags
    val virtualStep = s"${stepDef.name}$ZeroChar"
    if (stepDef.isForEach && stepDef.isDataTable) {
      stepDefs += (virtualStep ->
        stepDef.copy(
          withTags = tags.filter(_.name != ReservedTags.ForEach.toString).filter(!_.name.startsWith(ReservedTags.DataTable.toString))
        )
      )
      val keyword = Tag.findByName(stepDef.tags, ReservedTags.Context.toString) map { _ => 
        StepKeyword.Given
      } getOrElse {
        Tag.findByName(stepDef.tags, ReservedTags.Assertion.toString) map { _ => 
          StepKeyword.Then
        } getOrElse {
          StepKeyword.When
        }
      }
      val step = Step(None, keyword.toString, s"$virtualStep for each data record", Nil, None, Nil, None, Pending)
      stepDefs += (stepDef.name ->
        stepDef.copy(
          withSourceRef = None,
          withTags = List(Tag(ReservedTags.Synthetic)) ++ tags.filter(_.name != ReservedTags.ForEach.toString),
          withName = s"$virtualStep for each data record",
          withSteps = List(step)
        )
      )
    } else {
      stepDefs += (stepDef.name -> stepDef)
    }
  }
  
  /**
    * Gets the executable step definition for the given expression (if there is
    * one).
    * 
    * @param expression the expression to match
    * @return the step definition if a match is found; false otherwise
    */
  def getStepDef(expression: String): Option[(Scenario, List[(String, String)])] = 
    stepDefs.get(expression) match {
      case None => getStepDefWithParams(expression)
      case Some(stepDef) => Some((stepDef, Nil))
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
   * Gets the list of DSL steps supported by this context.  This implementation 
   * returns all user defined stepdefs. Subclasses can override to return  
   * addtional entries. The entries returned by this method are used for tab 
   * completion in the REPL.
   */
  def dsl: List[String] = stepDefs.keys.toList

  def addAttachment(name: String, extension: String, content: String): (String, File) = { 
    state.addAttachment(name, extension, content)
  }

  def addAttachment(name: String, file: File): (String, File) = { 
    state.addAttachment(name, file)
  }

  def popAttachments(): List[(String, File)] = state.popAttachments()

  def hasAttachments: Boolean = state.hasAttachments
  
}