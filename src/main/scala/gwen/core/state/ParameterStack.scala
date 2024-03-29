/*
 * Copyright 2014-2023 Branko Juric, Brady Wood
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
import gwen.core.node.gherkin.Annotations

import scala.collection.mutable
import scala.util.chaining._

/**
  * Manages and maintains an in memory stack of parameters.
  *
  * @author Branko Juric
  */
class ParameterStack {

  object ForEachParam {
    val itemName: String = s"${Annotations.ForEach}.itemName"
    val itemIndex: String = s"${Annotations.ForEach}.itemIndex"
    val itemNumber: String = s"${Annotations.ForEach}.itemNumber"
  }

  /**
    * The parameters stack.
    */
  private val paramStack = mutable.Stack[ScopedData]()

  /**
    * Adds the given parameters (name-value pairs) to a new scope
    * and pushes it onto the stack
    *
    * @param scope the name of the scope entry to add
    * @param params the parameters to add
    * @return the newly added scope
    */
  def push(scope: String, params: List[(String, String)]): ScopedData = {
    ScopedData(scope) tap { data =>
      params foreach { case (name, value) =>
        data.set(s"<$name>", value)
      }
      paramStack.push(data)
    }
  }

  /** Pops the current parameters off the stack. */
  def pop(): ScopedData = paramStack.pop()

  /**
    * Finds and retrieves parameter bound in the current stack.
    *
    * @param name the name of the parameter to find
    * @return the value if it is found (or throws error)
    */
  def get(name: String): String =
    getOpt(name).getOrElse(Errors.unboundAttributeError(name, "local"))

  /**
    * Finds and retrieves an optional parameter bound in the current stack.
    *
    * @param name the name of the parameter to find
    * @return Some(value) if a value is found or None otherwise
    */
  def getOpt(name: String): Option[String] =
    paramStack.headOption.flatMap(_.getOpt(name)).headOption

  /**
    * Checks whether or not the parameter stack contains the
    * given scope.
    *
    * @param scope the scope name to check
    * @return true if the scope is found; false otherwise
    */
  def containsScope(scope: String): Boolean = paramStack.exists(_.scope == scope)

  /** Checks whether or not the local stack is empty. */
  def isEmpty = paramStack.isEmpty

  /**
    * Returns a string representation of parameters in the current stack
    */
  override def toString: String = {
    paramStack.headOption.map(scope => (scope.scope, scope.findEntries(_ => true).toList)) match {
      case Some((scope, entries)) if entries.nonEmpty =>
        s"params : { scope: $scope, entries : [ ${entries map { case (n, v) =>
          val name = n.substring(1, n.length - 1)
          s"{ $name: $v }"
        } mkString ", "} ] }"
      case _ =>
        "params : { }"
    }
  }

}
