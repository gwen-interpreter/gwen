/*
 * Copyright 2015 Branko Juric, Brady Wood
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

package gwen

import scala.collection.JavaConverters._
import java.io.File
import java.util.Properties
import java.io.FileReader
import gwen.errors._

/**
  * Provides access to system properties loaded from properties files.
  * If a gwen.properties file exists in the user's home directory, then
  * its properties are loaded first. Once a property is loaded it is never
  * replaced. Therefore it is important to load properties in the right 
  * order. Existing system properties are not overriden by values in 
  * properties files (and therefore have precedence).
  *
  * @author Branko Juric
  */
object Settings {
  
  private val InlineProperty = """.*\$\{(.+?)\}.*""".r
  
  loadAll(UserOverrides.UserProperties.toList)
  
  /**
    * Loads all properties from the given files.
    * 
    * @param propsFiles the properties files to load
    */
  def loadAll(propsFiles: List[File]): Unit = {
    val sysProps = sys.props.keySet
    val props = propsFiles.foldLeft(new Properties()) { 
      (props, file) => 
        props.load(new FileReader(file))
        props.entrySet().asScala foreach { entry =>
          val key = entry.getKey.asInstanceOf[String]
          if (key == null || key.trim.isEmpty) invalidPropertyError(entry.toString, file)
        }
        props
    }
    props.entrySet().asScala.foreach { entry =>
      val key = entry.getKey.asInstanceOf[String]
      if (!sysProps.contains(key)) {
        try {
          val value = resolve(props.getProperty(key), props)
          Settings.add(key, value, overrideIfExists = true)
        } catch {
          case e: Throwable => propertyLoadError(key, e)
        }
      }
    }
  }
  
  /**
   * Resolves a given property by performing any property substitutions.
   * 
   * @param value the value to resolve
   * @param props the properties already read (candidates for substitution)
   * @throws MissingPropertyException if a property cannot be substituted 
   *         because it is missing from the given props
   */
  private[gwen] def resolve(value: String, props: Properties): String = value match {
    case InlineProperty(key) =>
      val inline = if (props.containsKey(key)) {
        props.getProperty(key)
      } else {
        sys.props.get(key).getOrElse(missingPropertyError(key))
      }
      val resolved = inline match {
        case InlineProperty(_) => resolve(inline, props)
        case _ => inline
      }
      resolve(value.replaceAll("""\$\{""" + key + """\}""", resolved), props)
    case _ => value
  }
  
  /**
    * Gets an optional property (returns None if not found)
    * 
    * @param name the name of the property to get
    */
  def getOpt(name: String): Option[String] = sys.props.get(name)
  
  /**
    * Gets a mandatory property (throws exception if not found)
    * 
    * @param name the name of the property to get
    * @throws gwen.errors.MissingPropertyException if no such property is set
    */
  def get(name: String): String = getOpt(name).getOrElse(missingPropertyError(name))
  
  /**
    * Adds a property.
    * 
    * @param name the name of the property to add
    * @param value the value to bind to the property
    */
  def add(name: String, value: String): Unit = {
    add(name, value, overrideIfExists = false)
  }
  
   /**
    * Adds a property.
    * 
    * @param name the name of the property to add
    * @param value the value to bind to the property
    */
  private[gwen] def add(name: String, value: String, overrideIfExists: Boolean): Unit = {
    if (overrideIfExists || !sys.props.contains(name)) {
      sys.props += ((name, value))
    }
  }
  
}