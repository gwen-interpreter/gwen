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
import gwen.core.node.gherkin._
import gwen.core.behavior.BehaviorType

import scala.util.matching.Regex
import scala.util.chaining._

import com.typesafe.scalalogging.LazyLogging
import java.io.File

/**
  * Base environment context providing access to all resources and state.
  *
  * @author Branko Juric
  */
abstract class Environment(initialState: EnvState) extends LazyLogging {

  private var state = initialState

  val stateLevel: StateLevel = GwenSettings.`gwen.state.level`

  def stepDefs: Map[String, Scenario] = state.getStepDefs
  def paramScope: ParameterStack = scopes.paramScope
  def scopes: ScopedDataStack = state.scopes
  def topScope: TopScope = scopes.topScope
  def nodeChain: NodeChain = state.nodeChain

  /** Create a clone of the current environment state */
  def cloneState: EnvState = EnvState(topScope, Some(stepDefs), nodeChain)

  /**
    * Closes any resources associated with the evaluation context. This implementation
    * does nothing (but subclasses can override).
    */
  def close(): Unit = { }

  /** Resets the current context but does not close it so it can be reused. */
  def reset(level: StateLevel): Unit = {
    logger.info(s"Resetting environment")
    state = if (StateLevel.feature.equals(level)) {
      EnvState.resetAttachmentNo()
      EnvState(topScope, None, NodeChain())
    } else {
      EnvState(topScope, Some(stepDefs), nodeChain)
    }

  }

  def asString: String = scopes.asString

  /** The spec type currently being evaluated. */
  def specType: SpecType = topScope.getObject(SpecType.toString).map(_.asInstanceOf[SpecType]).getOrElse(SpecType.Feature)

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
    * Gets the paraterised step definition for the given expression.
    * 
    * @param expression the step expression to get the step definition for
    * @param docString optional step docString (parameter)
    * @return the step definition and its parameters (name value tuples) if a 
    *         match is found; None otherwise
    */
  def getStepDef(expression: String, docString: Option[String]): Option[Scenario] = {
    stepDefs.get(expression).orElse {
      val matches = stepDefs.values.view.flatMap { stepDef =>
        val pattern = Regex.quote(stepDef.name).replaceAll("<.+?>", """\\E(.*?)\\Q""").replaceAll("""\\Q\\E""", "")
        if (expression.matches(pattern)) {
          val names = "<.+?>".r.findAllIn(stepDef.name).toList map { name => 
            name.substring(1, name.length - 1)
          }
          names.groupBy(identity).collectFirst { case (n, vs) if vs.size > 1 =>
            Errors.ambiguousCaseError(s"$n parameter defined ${vs.size} times in StepDef '${stepDef.name}'")
          }
          val values = pattern.r.unapplySeq(expression).get
          val params = names zip values
          val resolved = params.foldLeft(stepDef.name) { (result, param) => result.replace(s"<${param._1}>", param._2) }
          if (expression == resolved) {
            Some(stepDef.copy(
              withParams = docString map { ds => 
                names zip (values.init :+ ds)
              } getOrElse params
            ))
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
          Errors.ambiguousCaseError(s"$msg: ${matches.map(_.name).mkString(",")}")
        } else first
      } else None
    }
  }

  /** Adds current behavior. */
  def addBehavior(behavior: BehaviorType): BehaviorType =
    behavior tap { _ => state.addBehavior(behavior) }

  /** Removes the current behavior. */
  def popBehavior(): Option[BehaviorType] = state.popBehavior()

  /** Gets the current behavior. */
  def currentBehavior: Option[BehaviorType] = state.currentBehavior

  /** Checks if a top level step is currently being evaluated). */
  def isEvaluatingTopLevelStep: Boolean = paramScope.isEmpty

  def addAttachment(name: String, file: File): Unit = {
    state.addAttachment(name, file)
  }

  def popAttachments(): List[(Int, String, File)] = state.popAttachments()

  /** Pushes a node onto the node chain.*/
  def pushNode(node: GwenNode): NodeChain = state.pushNode(node)

  /** Pops a node off the node chain.*/
  def popNode(): (GwenNode, NodeChain) = state.popNode()

}
