/*
 * Copyright 2014 Branko Juric, Brady Wood
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
 *  - Or by a nominated scope name: using the `getIn` method
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
class ScopedDataStack(val scopeName: String) {

  /**
   * The scoped attribute stack.  The 'current' scope is always the one that is 
   * on the top of the stack.  All other scopes that are not at the
   * top of the stack are 'historical' scopes.
   */
  private val stack = Stack[ScopedData]()
  
  // add global scope when stack is created
  addScope(ScopedData.GlobalScopeName)
  
  /**
   * Creates and adds a new scope to the internal stack and makes it the 
   * currently active scope.
   * 
   * @param name
   * 			the name of the scope to add
   * @return
   * 			the newly added scope
   */
  def addScope(name: String): ScopedData = {
    val scope = ScopedData(scopeName, name)
    stack push scope
    scope
  }
  
  /**
   * Provides access to the currently active scope (if one exists).
   * 
   * @return Some(ScopedData) if a currently active scope exists or None 
   *         otherwise
   */
  def current: Option[ScopedData] = stack.headOption
  
  /**
   * Binds an attribute to the currently active scope.  An error is thrown
   * if no current scope is set.
   * 
   * @param name
   * 			the name of the attribute to bind
   * @return
   * 			the value to bind to the attribute
   */
  def set(name: String, value: String) = current match {
    case (Some(scope)) => scope.set(name, value)
    case _ => sys.error(s"""No currently active data scope found in $scopeName scopes. Please call addScope("name") first to create and activate one.""")
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
    getOpt(name).getOrElse(throw new AttrNotFoundException(name, current.map(_.name).getOrElse(""), this))
  
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
  def getOpt(name: String): Option[String] = current match {
    case Some(scope) => getInOpt(scope.name, name)
    case _ => None
  }
  
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
  def getAll(name: String): Seq[String] = current match {
    case Some(scope) => getAllIn(scope.name, name)
    case _ => Nil
  }
    
  /**
   * Finds and retrieves an attribute in the a named scope by scanning for it 
   * in all scopes in the stack (starting with the top most scope with that 
   * name and working down).  The value in the first scope found to contain 
   * the attribute is the one that is returned.
   *
   * @param scopeName
   * 			the scope name to scan
   * @param name
   * 			the name of the attribute to find
   * 
   *
   * @throws AttrNotFoundException if the attribute is not found
   */
  def getIn(scopeName: String, name: String): String = 
    getInOpt(scopeName, name).getOrElse(throw new AttrNotFoundException(name, scopeName, this))
  
  /**
   * Finds and retrieves an attribute in the a named scope by scanning for it 
   * in all scopes in the stack (starting with the top most scope with that 
   * name and working down).  The value in the first scope found to contain 
   * the attribute is the one that is returned.
   *
   * @param scopeName
   * 			the scope name to scan
   * @param name
   * 			the name of the attribute to find
   * 
   *
   * @return Some(value) if the attribute found or None otherwise
   */
  def getInOpt(scopeName: String, name: String): Option[String] = 
    stack.toIterator filter(_.name == scopeName) map (_.get(name)) collectFirst { 
      case Some(value) => value 
    } match {
      case None if (scopeName != ScopedData.GlobalScopeName) =>
        getInOpt(ScopedData.GlobalScopeName, name)
      case x => x
    }
  
  /**
   * Finds and retrieves all attributes in the named scope by scanning for 
   * them in all scopes in the stack (starting with the 
   * top most scope with that name and working down).  All values found are 
   * returned.
   *
   * @param scopeName
   * 			the scope name to scan
   * @param name
   * 			the name of the attribute to find
   *
   * @return a sequence of found attribute values or Nil otherwise
   */
  def getAllIn(scopeName: String, name: String): Seq[String] = 
    stack.toSeq filter(_.name == scopeName) flatMap (_.get(name)) match {
      case Nil if (scopeName != ScopedData.GlobalScopeName) =>
        getAllIn(ScopedData.GlobalScopeName, name)
      case x => x
    }
    
  /**
   * Returns a string representation of the entire attribute stack.
   * Each scope entry is printed in JSON format.
   */
  def toJson = {
    Json.obj(scopeName -> (stack.reverse map (_.toJson)))
  }
}

class AttrNotFoundException(attrName: String, scopeName: String, stack: ScopedDataStack) 
	extends Exception(s"$attrName not found in ${scopeName} ${stack.scopeName} scope")
