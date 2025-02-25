/*
 * Copyright 2014-2024 Branko Juric, Brady Wood
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
import gwen.core.eval.binding.BindingType

import scala.collection.mutable
import scala.util.chaining._

import com.typesafe.scalalogging.LazyLogging

/**
  * Binds data attributes to an arbitrary scope that has a name.
  * Attributes are stored as name value pairs in a as a list of tuples that
  * look like this:
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
  * of the `atts` list.  This is done internally. The order in which
  * attributes are stored reflects the same order in which they were added.
  * Once an attribute is added, it is never removed.  If a same named attribute
  * is added more than once, then it will appear in the scope multiple times
  * (once for each time it is added).
  *
  * When an attribute is looked up, its most recent (latest) entry is
  * returned. For example, if the scope contains two attribute entries with
  * the same name, then the value of the second entry is returned.
  *
  * @author Branko Juric
  *
  * @param scope the scope name
  */
class ScopedData(val scope: String) extends Mutability with LazyLogging {

  /**
    * The internal list of name-value tuples for storing attributes.  When a
    * new  attribute is added (by calling `set`), it is appended to the end of
    * the list.
    */
  private val atts = mutable.ListBuffer[(String, String)]()

  val isTopScope = false

  /** Creates a deep clone containing all data. */
  def deepClone: ScopedData = deepCopyInto(new ScopedData(scope))

  /** Copies all data. */
  def deepCopyInto(sd: ScopedData): ScopedData = sd tap { _ =>
    atts foreach { (n, v) => sd.set(n, v, force = true) }
  }
  
  /**
    * Checks if the scoped data is empty.
    *
    * @return true if empty; false otherwise
    */
  def isEmpty: Boolean = atts.isEmpty

  /**
    * Finds and retrieves an attribute value from the scope by name.  If
    * multiple entries exist for the given name, then the most recently
    * (or latest) added attribute value for that name is returned.
    *
    * @param name the name of the attribute to find
    * @return Some(value) if the attribute value is found or None otherwise
    */
  def getOpt(name: String): Option[String] =
    findEntry { case (n, _) => n == name } tap { value =>
      value.foreach { nvp =>
        logger.debug(s"Found $nvp in scope/$scope")
      }
    } map (_._2)

  /**
    * Finds and retrieves an attribute from the scope (throws error if not found)
    *
    * @param name the name of the attribute to find
    * @return the attribute value if found (throws error otherwise)
    */
  def get(name: String): String =
    getOpt(name).getOrElse(Errors.unboundReferenceError(name, scope))

  /**
    * Finds and retrieves all attribute values from the scope by name.
    *
    * @param name the name of the attribute to find
    * @return a sequence of Strings of values are found or Nil otherwise
    */
  def getAll(name: String): Seq[String] = findEntries { case (n, _) => n == name } tap { nvps =>
    logger.debug(s"Found [${nvps.mkString(",")}]' in scope/$scope")
  } map(_._2)

  /**
    * Finds the visible entry
    *
    * @param name the name of the entry to find
    * @return Some visible entry or None
    */
  def hasValue(name: String, value: String): Boolean = {
    findEntry((n, _) => n == name || n.startsWith(s"$name/")) map { (_, v) => v == value } getOrElse false
  }

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
  def set(name: String, value: String, force: Boolean = false): ScopedData = {
    if (!hasValue(name, value)) {
      if (!force) checkMutability(name, this)
      (name, value) tap { nvp =>
        logger.debug(s"Binding $nvp to scope/$scope")
        atts += nvp
      }
    }
    this
  }

  /**
    * Clears the given attribure.
    * 
    * @param name the name of the attribute to clear
    */
  def clear(name: String, force: Boolean = false): ScopedData = {
    (findEntries { case (n, _) =>
      (n == name || n.startsWith(s"$name/")) && !n.endsWith(s"/${BindingType.dryValue}")
    } map { case (n, _) =>
      n
    } distinct) filter { n => 
      getOpt(n).nonEmpty
    } foreach { n =>
      if (!force) checkMutability(name, this)
      set(n, null)
    }
    this
  }

  /**
    * Finds the named entry
    *
    * @param name the name of the entry to find
    * @param pred the conditions to match
    * @return Some named entry or None
    */
  def namedEntry(name: String)(pred: ((String, String)) => Boolean): Option[(String, String)] = {
    findEntries((n, _) => n == name || n.startsWith(s"$name/")).filter(pred).lastOption.filter((_, v) => v != null)
  }

  /**
   * Filters all contained attributes based on the given predicate.
   *
   * @param pred the predicate filter to apply; a (name, value) => boolean function
   * @return Some(ScopedData) containing only the attributes accepted by the predicate;
   */
  def filterAtts(pred: ((String, String)) => Boolean): ScopedData = {
    findEntries(pred).foldLeft(if (isTopScope) new TopScope(StateLevel.valueOf(scope)) else ScopedData(scope)) { (data, entry) =>
      val (n, v) = entry
      data.set(n, v, force = true)
    }
  }

  /**
   * Finds the first entry that matches the given predicate.
   *
   * @param pred the predicate filter to apply; a (name, value) => boolean function
   * @return Some((name, value)) or None if no match is found
   */
  def findEntry(pred: ((String, String)) => Boolean): Option[(String, String)] =
    findEntries(pred).lastOption match {
      case Some((_, null)) => None
      case x => x
    }

  /**
   * Finds all entries that match the given predicate.
   *
   * @param pred the predicate filter to apply; a (name, value) => boolean function
   * @return a sequence of name-value pairs or Nil if no entries match the predicate
   */
  def findEntries(pred: ((String, String)) => Boolean): Seq[(String, String)] = {
    atts.filter(pred).toSeq
  }

  /**
    * Returns this entire scope as a String.
    */
  def asString(env: Boolean): String = {
    val scopeStr =
      s"""|scope : "$scope" {${
          atts.toList match {
            case Nil => " }"
            case _ => s"""${atts map {
              case (n, v) =>
                s"\n  $n : ${if(v == null) String.valueOf(v) else s""""${Formatting.padTailLines(v, s"  ${n.replaceAll(".", " ")}")}""""}"
            } mkString}
            |}"""
          }}""".stripMargin
    Formatting.stripZeroChar(
      if (env) {
        s"""|env {
            |${scopeStr.linesIterator.map(l => s"  $l").mkString("\n|")}
            |}""".stripMargin
      } else {
        scopeStr
      }
    )
  }

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
  def apply(name: String, atts: List[(String, String)]): ScopedData = new ScopedData(name) tap { _.atts ++= atts }

}
