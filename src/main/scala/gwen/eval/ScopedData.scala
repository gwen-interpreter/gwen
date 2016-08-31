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

import gwen.Predefs.Kestrel
import play.api.libs.json.Json
import play.api.libs.json.JsValue
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import com.typesafe.scalalogging.slf4j.LazyLogging
import play.api.libs.json.JsArray
import play.api.libs.json.JsObject
import gwen.errors._

/**
  * Binds data attributes to an arbitrary scope that has a name. 
  * Attributes are stored as name value pairs in a JSON sequence as follows:
  *
  * {{{
  * {
  *   "scope" : "name"
  *   "atts" : {
  *     "name1" : "value1",
  *     "name2" : "value2",
  *     ...
  *     "nameN" : "valueN"
  *   }
  * }
  * }}}
  *
  * When a new data attribute is added to the scope, it is appended to the end 
  * of the `atts` sequence.  This is done internally. The order in which 
  * attributes are stored reflects the same order in which they were added.  
  * Once an attribute is added, it is never removed.  If a same named attribute 
  * is added more than once, then it will appear in the scope multiple times 
  * (once for each time it is added).
  *
  * When an attribute is looked up, its most recent (latest) entry is
  * returned. For example, if the scope contains two attribute entries with
  * the same name, then the value of the second entry is returned.
  * 
   * Data attributes are internally stored in immutable JSON data structures.
  * 
  * @author Branko Juric
  * 
  * @param scope the scope name
  */
class ScopedData(val scope: String) extends LazyLogging {
  
  /**
    * The internal JSON object where attributes are stored.  When a new 
    * attribute is added (by calling `set`), it is appended to the end of 
    * the existing sequence to create a new sequence that is then assigned 
    * back to this variable.
    */
  private var atts = Json.arr()
  
  /**
    * Checks if the scoped data is empty.
    * 
    * @return true if empty; false otherwise
    */
  def isEmpty = atts.value.isEmpty

  /**
    * Finds and retrieves an attribute value from the scope by name.  If 
    * multiple entries exist for the given name, then the most recently 
    * (or latest) added attribute value for that name is returned.
    *
    * @param name the name of the attribute to find
    * @return Some(value) if the attribute value is found or None otherwise
    */
  def getOpt(name: String): Option[String] = 
    (atts \\ name).lastOption tap { valueOpt =>
      valueOpt foreach { value =>
          logger.debug(s"Found ${Json.obj(name -> value)} in scope/$scope")
      }
    } map (_.as[String]) match {
      case (Some(null)) => None
      case result => result
    }
    
  /**
    * Finds and retrieves an attribute from the scope (throws error if not found)
    *
    * @param name the name of the attribute to find
    * @return the attribute value if found (throws error otherwise)
    * @throws gwen.errors.UnboundAttributeException if the attribute is bound 
    *         to the given name
    */
  def get(name: String): String = 
    getOpt(name).getOrElse(unboundAttributeError(name, scope))
  
  /**
    * Finds and retrieves all attribute values from the scope by name.
    *
    * @param name the name of the attribute to find
    * @return a sequence of Strings of values are found or Nil otherwise
    */
  def getAll(name: String): Seq[String] = 
    (atts \\ name) tap { values =>
      logger.debug(s"Found [${values.map(value => Json.obj(name -> value)).mkString(",")}]' in scope/$scope")
    } map (_.as[String])

  /**
    * Binds a new attribute value to the scope.  If an attribute of the same
    * name already exists, then this new attribute overrides the existing one
    * but does not replace it.
    *
    * @param name the name of the attribute to bind
    * @param value the value to bind to the attribute
    * @return the current scope containing the old attributes plus the 
    *         newly added attribute
    */
  def set(name: String, value: String): ScopedData = {
    if(!((atts \\ name).lastOption.map(_.as[String] == value).getOrElse(false))) {
      Json.obj(name -> value) tap { nvp =>
        logger.debug(s"Binding $nvp to scope/$scope")
        atts = atts :+ nvp
      }
    }
    this
  }
    
  /**
   * Filters all contained attributes based on the given predicate.
   * 
   * @param pred the predicate filter to apply; a (name, value) => boolean function
   * @return Some(ScopedData) containing only the attributes accepted by the predicate; 
   *         or None if no attributes are accepted
   */
  def filterAtts(pred: ((String, String)) => Boolean): Option[ScopedData] = {
    val filteredAtts = (atts.value.flatMap { 
      case JsObject(values) => 
        values.map(nvp => (nvp._1, nvp._2.as[String]))
      case _ => None
      }).filter(pred(_))
    val result = filteredAtts.foldLeft(ScopedData(scope)) { (data, attr) =>
      data.set(attr._1, attr._2)
    }
    if (result.isEmpty) None else Some(result)
  }

  /**
    * Returns this entire scope as a JSON object.
    */
  def json = Json.obj("scope" -> scope, "atts" -> atts)

}

/**
 * ScopedData factory.
 * 
 * @author Branko Juric
 */
object ScopedData {

  /**
    * Create a new ScopedData object with an empty attribute list.
    *
    * @param name the scope name 
    */
  def apply(name: String): ScopedData = new ScopedData(name)
  
  /**
    * Create a new ScopedData object with the given attribute list.
    *
    * @param name the scope name 
    * @param atts the list of attributes to include in the scope
    */
  def apply(name: String, atts: JsArray) = new ScopedData(name) tap { _.atts = atts }

}