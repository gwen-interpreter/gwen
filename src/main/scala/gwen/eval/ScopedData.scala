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

import com.typesafe.scalalogging.slf4j.LazyLogging

import gwen.Predefs.Kestrel
import play.api.libs.json.Json
import play.api.libs.json.Json.toJsFieldJsValueWrapper

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
 */
class ScopedData(val scope: String, val name: String) extends LazyLogging {
  
  /**
   * The internal JSON object where attributes are stored.  When a new 
   * attribute is added (by calling `set`), it is appended to the end of 
   * the existing sequence to create a new sequence that is then assigned 
   * back to this variable.
   */
  private[this] var atts = Json.arr()

  /**
   * Finds and retrieves an attribute value from the scope by name.  If 
   * multiple entries exist for the given name, then the most recently 
   * (or latest) added attribute value for that name is returned.
   *
   * @param name
   * 			the name of the attribute to find
   *
   * @return Some(value) if the attribute value is found or None otherwise
   */
  def get(name: String): Option[String] = 
    (atts \\ name).lastOption tap { valueOpt =>
      valueOpt foreach { value =>
          logger.debug(s"Found ${Json.obj(name -> value)} in ${scope}/scope/${this.name}")
      }
    } map (_.as[String])
  
  /**
   * Finds and retrieves all attribute values from the scope by name.
   *
   * @param name
   * 			the name of the attribute to find
   *
   * @return a sequence of Strings of values are found or Nil otherwise
   */
  def getAll(name: String): Seq[String] = 
    (atts \\ name) tap { values =>
      logger.debug(s"Found [${values.map(value => Json.obj(name -> value)).mkString(",")}]' in ${scope}/scope/${this.name}")
    } map (_.as[String])

  /**
   * Binds a new attribute value to the scope.  If an attribute of the same
   * name already exists, then this new attribute overrides the existing one
   * but does not replace it.
   *
   * @param name
   * 			the name of the attribute to bind
   * @param value
   * 			the value to bind to the attribute
   *
   * @return 
   * 			the current scope containing the old attributes plus the 
   *            newly added attribute
   */
  def set(name: String, value: String): ScopedData = {
    if(!((atts \\ name).lastOption.map(_.as[String] == value).getOrElse(false))) {
      Json.obj(name -> value) tap { nvp =>
        logger.debug(s"Binding $nvp to ${scope}/scope/${this.name}")
        atts = atts :+ nvp
      }
    }
    this
  }

  /**
   * Returns this entire scope as a JSON object.
   */
  def toJson = Json.obj("scope" -> name, "atts" -> atts)

}

/**
 * ScopedData factory.
 * 
 * @author Branko Juric
 */
object ScopedData {

  val GlobalScopeName = "global"
    
  /**
   * Create a new ScopedData object with an empty attribute list.
   *
   * @param scope
   * 			the data scope
   * @param name
   * 			the scope name 
   */
  def apply(scope: String, name: String): ScopedData = new ScopedData(scope, name)

}