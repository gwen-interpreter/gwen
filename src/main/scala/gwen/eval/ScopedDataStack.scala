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

import scala.Option.option2Iterable
import scala.collection.mutable.Stack
import play.api.libs.json.Json
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.libs.json.JsObject
import gwen.Predefs.Kestrel

/**
 * Manages and maintains an in memory stack of [[ScopedData]] objects
 * for a particular scope and provides convenient access to all attributes. 
 * Individual steps utilise this to access any data in the stack at any time.  
 * Collaborating steps can pass data to each other through this stack.
 * 
 * The scope at the top of the stack is always the currently active one.
 * When a new scope is created, it is always added to the top of the 
 * stack.  Therefore, newly created scope automatically becomes the
 * currently active scope, and the previously active scope (and any
 * others beneath it) move down one level in the stack.
 * 
 * Attributes are always bound to the currently active scope (the one at
 * the top of the stack).  They cannot be bound to any of the other non active
 * scopes lower in the stack. Once an attribute is bound to a scope it is never
 * removed from that scope, and therefore cannot be replaced.  It can be 
 * overshadowed though by another attribute of the same name in a higher scope.  
 * In such cases where a scope contains multiple attributes of the same name, 
 * the most recently added scope always shadows the former when a lookup is 
 * performed.
 * 
 * Attributes can be looked up in one of two ways:
 * 
 *  - By currently active scope: using the `get` method
 *    
 *    - This lookup scans the currently active scope and all other scopes below 
 *      it in the stack that have the same name.
 *  
 *  - Or by a nominated scope: using the `getIn` method
 *    
 *    - This lookup scans all scopes in the stack that have the nominated scope 
 *      name.
 *     
 * Although their visibilities are different, both lookups return the first
 * attribute found and scan from the top most visible scope down to the lowest.
 * This ensures that the most recently bound value for an attribute is always
 * returned if found.
 * 
 * @author Branko Juric  
 */
class ScopedDataStack() {

  /**
   * The scoped attribute stack.  The 'current' scope is always the one that is 
   * on the top of the stack.  All other scopes that are not at the
   * top of the stack are 'historical' scopes.
   */
  private var scopes: Stack[ScopedData] = _
  
  reset()
  
  /**
   * Resets the data stack.
   */
  def reset() {
      scopes = Stack[ScopedData]() tap { _ push ScopedData("feature") }
  }
  
  /**
   * Provides access to the global features scope (which is always at the
   * bottom of the stack).
   */
  private[eval] def featureScope =  scopes.last
  
  /**
   * Creates and adds a new scope to the internal stack and makes it the 
   * currently active scope. Keeps the current scope if it has the same name.
   * 
   * @param scope
   * 			the name of the scope to add
   * @return
   * 			the newly added scope
   */
  def addScope(scope: String): ScopedData = 
    if (scope != current.scope) {
      if (scope == featureScope.scope) {
        featureScope
      } else {
        if (current != featureScope && current.isEmpty) {
          scopes pop
        }
        scopes push ScopedData(scope) head
      }
    } else {
      current
    }
  
  /**
   * Provides access to the currently active scope.
   * 
   * @return the currently active scope
   */
  def current: ScopedData = scopes.head
  
  /**
   * Gets the currently visible scoped data
   * 
   */
  def visibleScopes: Stack[ScopedData] = scopes.reverse.filter {
    (scopedData: ScopedData) => 
      featureScope == scopedData || current.scope == scopedData.scope 
  }
  
  /**
   * Binds an attribute to the currently active scope.  An error is thrown
   * if no current scope is set.
   * 
   * @param name
   * 			the name of the attribute to bind
   * @return
   * 			the value to bind to the attribute
   */
  def set(name: String, value: String) { 
      if (!getOpt(name).map(_ == value).getOrElse(false)) {
        current.set(name, value)
      }
  }
  
  /**
   * Finds and retrieves an attribute in the currently active scope by scanning
   * for it in all scopes in the stack (starting with the currently active 
   * scope and working down).  The value in the first scope found to contain 
   * the attribute is the one that is returned.
   *
   * @param name
   * 			the name of the attribute to find
   *
   * @return Some(value) if the attribute found or None otherwise
   */
  def get(name: String): String = 
    getOpt(name).getOrElse(throw new AttrNotFoundException(name, current.scope))
  
  /**
   * Finds and retrieves an attribute in the currently active scope by scanning
   * for it in all scopes in the stack (starting with the currently active 
   * scope and working down).  The value in the first scope found to contain 
   * the attribute is the one that is returned.
   *
   * @param name
   * 			the name of the attribute to find
   *
   * @return Some(value) if the attribute found or None otherwise
   */
  def getOpt(name: String): Option[String] = getInOpt(current.scope, name)
  
  /**
   * Finds and retrieves all attributes in the currently active scope by scanning
   * for them in all scopes in the stack (starting with the currently active 
   * scope and working down).  All values found are returned.
   *
   * @param name
   * 			the name of the attribute to find
   *
   * @return a sequence of found attribute values or Nil otherwise
   */
  def getAll(name: String): Seq[String] = getAllIn(current.scope, name)
    
  /**
   * Finds and retrieves an attribute in the a named scope by scanning for it 
   * in all scopes in the stack (starting with the top most scope with that 
   * name and working down).  The value in the first scope found to contain 
   * the attribute is the one that is returned.
   *
   * @param scope
   * 			the scope name to scan
   * @param name
   * 			the name of the attribute to find
   * 
   *
   * @throws AttrNotFoundException if the attribute is not found
   */
  def getIn(scope: String, name: String): String = 
    getInOpt(scope, name).getOrElse(throw new AttrNotFoundException(name, scope))
  
  /**
   * Finds and retrieves an attribute in the a named scope by scanning for it 
   * in all scopes in the stack (starting with the top most scope with that 
   * name and working down).  The value in the first scope found to contain 
   * the attribute is the one that is returned.
   *
   * @param scope
   * 			the scope name to scan
   * @param name
   * 			the name of the attribute to find
   * 
   *
   * @return Some(value) if the attribute found or None otherwise
   */
  def getInOpt(scope: String, name: String): Option[String] = 
    scopes.toIterator filter(_.scope == scope) map (_.getOpt(name)) collectFirst { 
      case Some(value) => value 
    } match {
      case None if (scope != featureScope.scope) =>
        getInOpt(featureScope.scope, name)
      case x => x
    }
  
  /**
   * Finds and retrieves all attributes in the named scope by scanning for 
   * them in all scopes in the stack (starting with the 
   * top most scope with that name and working down).  All values found are 
   * returned.
   *
   * @param scope
   * 			the scope name to scan
   * @param name
   * 			the name of the attribute to find
   *
   * @return a sequence of found attribute values or Nil otherwise
   */
  def getAllIn(scope: String, name: String): Seq[String] = 
    scopes.toList filter(_.scope == scope) flatMap (_.getAll(name)) match {
      case Nil if (scope != featureScope.scope) =>
        getAllIn(featureScope.scope, name)
      case x => x
    }
    
  /**
   * Returns a string representation of the entire attribute stack 
   * as a JSON object.
   */
  def json: JsObject = Json.obj("scopes" -> (scopes.reverse map (_.json)))
  
  /**
   * Returns a string representation of the visible attribute stack
   * as a JSON object.
   */
  def visibleJson = Json.obj("scopes" -> (visibleScopes map (_.json)))
}

class AttrNotFoundException(name: String, scope: String) 
	extends Exception(s"$name not found in $scope scope")
