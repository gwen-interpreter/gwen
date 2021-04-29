/*
 * Copyright 2014-2015 Branko Juric, Brady Wood
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

package gwen.model.state

import gwen._

import scala.collection.mutable

/**
  * Manages and maintains an in memory stack of [[ScopedData]] objects
  * for storing locally scoped StepDef parameters.
  * 
  * @author Branko Juric  
  */
class LocalDataStack {

  /**
    * The locally scoped data stack.
    */
  private val localData = mutable.Stack[ScopedData]()
  
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
        data.set(name, value)
      }
      localData.push(data)
    }
  }
  
  /** Pops the current data object off the stack. */
  def pop: ScopedData = localData.pop()
  
  /**
    * Finds and retrieves an attribute bound in the local stack.
    * Only the top of the stack is searched.
    *
    * @param name the name of the attribute to find
    * @return the value if it is found (or throws error)
    */
  def get(name: String): String =
    getOpt(name).getOrElse(Errors.unboundAttributeError(name, "local"))

  /**
    * Finds and retrieves an optional attribute bound in the local stack.
    * Only the top of the stack is searched.
    *
    * @param name the name of the attribute to find
    * @return Some(value) if a value is found or None otherwise
    */
  def getOpt(name: String): Option[String] =
    localData.headOption.flatMap(_.getOpt(name)).headOption
    
  /**
    * Checks whether or not this local stack contains the 
    * given scope.
    * 
    * @param scope the scope name to check
    * @return true if the scope is found; false otherwise 
    */
  def containsScope(scope: String): Boolean = localData.exists(_.scope == scope)
  
  /** Checks whether or not the local stack is empty. */
  def isEmpty = localData.isEmpty

  /**
    * Returns a string representation of the entire attribute stack
    */
  def asString: String = {
    val scopes = localData.reverse
    s"""localScope : {${
      scopes.toList match {
        case Nil => "| "
        case _ => scopes map {
          scope =>
            s"""|  ${scope.asString()}
                |"""".stripMargin
        }
      }}
    |}""".stripMargin
  }
  
}

