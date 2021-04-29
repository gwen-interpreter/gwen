/*
 * Copyright 2016-2019 Branko Juric, Brady Wood
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

/**
  * Binds all top level attributes and adds flash attributes in the current scope when necessary. Also
  * included is a cache for storing non string objects. The top scope can be a feature or scenario 
  * level scope depending on the `gwen.state.level` setting.
  * 
  * @author Branko Juric
  */
class TopScope() extends ScopedData(GwenSettings.`gwen.state.level`.toString) {
  
  override val isTopScope = true
  
  /** 
    *  Provides access to the current (non top) scope. 
    */
  private[state] var currentScope: Option[ScopedData] = None

  /** Map of object stacks. */
  private var objectStack = Map[String, List[Any]]()

  /**
    * Binds a new attribute value to the scope.  If an attribute of the same
    * name already exists, then this new attribute overrides the existing one
    * but does not replace it. If an attribute of the same name exists in the 
    * current scope, then the attribute is also added to its flash scope (so 
    * that overriding occurs). 
    *
    * @param name the name of the attribute to bind
    * @param value the value to bind to the attribute
    * @return the current scope containing the old attributes plus the 
    *         newly added attribute
    */
  override def set(name: String, value: String): ScopedData =
    super.set(name, value) tap { _=>
      currentScope foreach { cs =>
        if (cs.findEntries { case (n, _) => n == name || n.startsWith(s"$name/") } nonEmpty) {
          cs.flashScope.foreach { fs => 
            fs += ((name, value)) 
          }
        }
      }
    }

  /**
    * Pushes a named object to the object stack.
    *
    * @param name the name to bind the object to
    * @param obj the object to push
    */
  def pushObject(name: String, obj: Any): Unit = {
    objectStack.get(name) match {
      case Some(objs) => objectStack += (name -> (obj :: objs))
      case None => objectStack += (name -> List(obj))
    }
  }

  /**
    * Gets a bound object from the object stack.
    *
    * @param name the name of the bound object to get
    * @return Some(bound object) or None
    */
  def getObject(name: String): Option[Any] = objectStack.get(name).flatMap(_.headOption)

  /**
    * Clears a bound object from the object stack. Performs no operation if no object is not bound to the name.
    *
    * @param name the name of the bound object to pop
    */
  def popObject(name: String): Option[Any] = {
    objectStack.get(name) match {
      case Some(head::tail) =>
        if (tail.nonEmpty) {
          objectStack += (name -> tail)
        } else {
          objectStack -= name
        }
        Option(head)
      case obj @ Some(_) =>
        objectStack -= name
        obj
      case _ => None
    }
  }

  /** Gets all implicit attributes. */
  def implicitAtts: Seq[(String, String)] = 
    findEntries { case (n, _) => n.matches("""^gwen\.(feature\.(name|file\.(name|path|absolutePath))|rule\.name)$""")}

}
