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
import scala.collection.mutable.ArrayStack
import play.api.libs.json.Json
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.libs.json.JsObject
import gwen.Predefs.Kestrel
import gwen.errors._
import scala.collection.mutable.Map

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
  private var scopes: ArrayStack[ScopedData] = _
  
  /** 
    *  Provides access to the local StepDef scope (StepDef parameters
    *  are pushed and poped in and out of this scope as StepDef calls
    *  are made). 
    */
  private[eval] val paramScope = new LocalDataStack()
  
  reset()
  
  /** Resets the data stack. */
  def reset() {
      scopes = ArrayStack[ScopedData]() tap { _ push new FeatureScope() }
      paramScope.reset()
  }
  
  /**
    * Provides access to the global features scope (which is always at the
    * bottom of the stack).
    */
  private[eval] def featureScope: FeatureScope =  scopes.last.asInstanceOf[FeatureScope]
  
  /**
    * Provides access to the currently active scope.
    * 
    * @return the currently active scope
    */
  def current: ScopedData = scopes.head
  
  /**
    * Creates and adds a new scope to the internal stack and makes it the 
    * currently active scope. Keeps the current scope if it has the same name.
    * 
    * @param scope the name of the scope to add
    * @return the newly added scope
    */
  def addScope(scope: String): ScopedData = 
    if (scope != current.scope) {
      current.flashScope = None
      featureScope.currentScope = None
      if (scope == featureScope.scope) {
        featureScope
      } else {
        if (current != featureScope && current.isEmpty) {
          scopes pop
        }
        scopes push ScopedData(scope)
        current tap { _ =>
          current.flashScope = Some(Map[String, String]()) 
          featureScope.currentScope = Some(current)
        }
      }
    } else {
      current
    }
  
  /**
   * Filters scoped data based on a predicate.
   * 
   * @param pred the predicate to filter with
   */
  def filterData(pred: ScopedData => Boolean): ScopedDataStack = ScopedDataStack(scopes.filter(pred))
  
  /** Gets the currently visible scoped data. */
  def visible: ScopedDataStack = filterData { data => 
      data.isFeatureScope || current.scope == data.scope 
  }
  
  /**
   * Filters all attributes in all scopes based on the given predicate.
   * 
   * @param pred the predicate filter to apply; a (name, value) => boolean function
   * @return a new Scoped data stack containing only the attributes accepted by the predicate; 
   */
  def filterAtts(pred: ((String, String)) => Boolean): ScopedDataStack = 
    ScopedDataStack(scopes.flatMap(_.filterAtts(pred)))
  
  /**
    * Binds an attribute to the currently active scope.  An error is thrown
    * if no current scope is set.
    * 
    * @param name the name of the attribute to bind
    * @return the value to bind to the attribute
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
    * @param name the name of the attribute to find
    * @return the attribute value
    * @throws gwen.errors.UnboundAttributeException if the attribute is 
    *         not bound to the given name
    */
  def get(name: String): String = 
    getOpt(name).getOrElse(unboundAttributeError(name, current.scope))
  
  /**
    * Finds and retrieves an attribute in the currently active scope by scanning
    * for it in all scopes in the stack (starting with the currently active 
    * scope and working down).  The value in the first scope found to contain 
    * the attribute is the one that is returned.
    *
    * @param name the name of the attribute to find
    * @return Some(value) if the attribute found or None otherwise
    */
  def getOpt(name: String): Option[String] = getInOpt(current.scope, name)
  
  /**
   * Finds the first entry that matches the given predicate.
   * 
   * @param pred the predicate filter to apply; a (name, value) => boolean function
   * @return Some((name, value)) or None if no match is found
   */
  def findEntry(pred: ((String, String)) => Boolean): Option[(String, String)] =
    scopes.toIterator filter(_.scope == current.scope) map (_.findEntry(pred)) collectFirst { 
      case Some(value) => value 
    } match {
      case None if (!current.isFeatureScope) =>
        featureScope.findEntry(pred)
      case x => x
    }
  
  /**
    * Finds and retrieves all attributes in the currently active scope by scanning
    * for them in all scopes in the stack (starting with the currently active 
    * scope and working down).  All values found are returned.
    *
    * @param name the name of the attribute to find
    * @return a sequence of found attribute values or Nil otherwise
    */
  def getAll(name: String): Seq[String] = getAllIn(current.scope, name)
    
  /**
    * Finds and retrieves an attribute in the a named scope by scanning for it 
    * in all scopes in the stack (starting with the top most scope with that 
    * name and working down).  The value in the first scope found to contain 
    * the attribute is the one that is returned.
    *
    * @param scope the scope name to scan
    * @param name the name of the attribute to find
    * @throws gwen.errors.UnboundAttributeException if the attribute is bound 
    *         to the given name in the given scope
    */
  def getIn(scope: String, name: String): String = 
    getInOpt(scope, name).getOrElse(unboundAttributeError(name, scope))
  
  /**
    * Finds and retrieves an attribute in the a named scope by scanning for it 
    * in all scopes in the stack (starting with the top most scope with that 
    * name and working down).  The value in the first scope found to contain 
    * the attribute is the one that is returned.
    *
    * @param scope the scope name to scan
    * @param name the name of the attribute to find
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
    * @param scope the scope name to scan
    * @param name the name of the attribute to find
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
  def json: JsObject = Json.obj("scopes" -> (scopes.filter(!_.isEmpty).reverse map (_.json)))
  
}

object ScopedDataStack {
  
  /**
   * Merges a stack of scopes into a single ScopedDataStack object.
   * 
   * @param scopes the scopes to merge
   */
  def apply(scope: Option[ScopedData]): ScopedDataStack = 
    scope.map(x => ScopedDataStack(ArrayStack(x))).getOrElse(ScopedDataStack(ArrayStack[ScopedData]()))
  
  /**
   * Merges a stack of scopes into a single ScopedDataStack object.
   * 
   * @param scopes the scopes to merge
   */
  def apply(scopes: ArrayStack[ScopedData]): ScopedDataStack = 
    new ScopedDataStack() tap { stack =>
      if (scopes.exists(_.isFeatureScope)) stack.scopes.pop
      scopes.reverse.foreach { data =>
        stack.scopes.push(data)
      }
  }
  
}
