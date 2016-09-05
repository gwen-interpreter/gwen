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

package gwen.eval

import scala.collection.mutable.Stack

import gwen.Predefs.Kestrel
import gwen.errors.unboundAttributeError
import play.api.libs.json.JsObject
import play.api.libs.json.Json
import play.api.libs.json.Json.toJsFieldJsValueWrapper

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
  private val localData = Stack[ScopedData]()
  
  /**
    * Clears all items in the local stack.
    */
  def reset() {
    localData.clear()
  }
  
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
  def pop = localData.pop
  
  /**
    * Finds and retrieves an attribute bound in the local stack.
    * Only the top of the stack is searched.
    *
    * @param name the name of the attribute to find
    * @return Some(value) if a value is found or None otherwise
    */
  def get(name: String): String = 
    (localData.headOption map (_.getOpt(name)) collectFirst { 
      case Some(value) => value 
    }).getOrElse(unboundAttributeError(name, "local"))
    
  /**
    * Checks whether or not this local stack contains the 
    * given scope.
    * 
    * @param scope the scope name to check
    * @return true if the scope is found; false otherwise 
    */
  def containsScope(scope: String) = localData.find(_.scope == scope).isDefined
  
  /**
    * Returns a string representation of the entire attribute stack 
    * as a JSON object.
    */
  def json: JsObject = Json.obj("localScope" -> (localData.reverse map (_.json)))
  
}

