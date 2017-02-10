/*
 * Copyright 2016 Branko Juric, Brady Wood
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

import gwen.Predefs.Kestrel

/**
  * Binds all global feature level attributes and adds flash 
  * attributes in the current scope when necessary.
  * 
  * @author Branko Juric
  */
class FeatureScope extends ScopedData("feature") {
  
  override val isFeatureScope = true
  
  /** 
    *  Provides access to the current (non feature) scope. 
    */
  private[eval] var currentScope: Option[ScopedData] = None
  
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
  
}
