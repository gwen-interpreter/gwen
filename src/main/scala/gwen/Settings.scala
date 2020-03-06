/*
 * Copyright 2015-2020 Branko Juric, Brady Wood
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
import gwen.Predefs.FileIO

import scala.collection.mutable

/**
  * Provides access to enviornment variables and system properties loaded from properties files. 
  * Values  are loaded in the following order of precedence:
  *   1. Environment variables (setting names that start with `env.`)
  *   2. System properties passed through -D Java command line option
  *   3. ~/gwen.properties (user overrides)
  *   4. Properties files passed into Gwen through the -p/--properties command line option
  *      - These are loaded in the order provided so that later ones override earlier ones
  *   5. ./gwen.properties (working directory)
  *   6. ~/.gwen/gwen.properties (global properties)
  *
  * Once a property is loaded it is never replaced. Therefore it is important to load properties in the right
  * order as per above. System properties are not overridden.
  *
  * @author Branko Juric
  */
object Settings {

  object Lock

  val userProperties: Option[File] = FileIO.getUserFile("gwen.properties")
  val workingProperties: Option[File] = FileIO.getFileOpt("gwen.properties")
  val defaultProperties: Option[File] = FileIO.getUserFile(".gwen/gwen.properties")

  val UserMeta: Option[File] = FileIO.getUserFile("gwen.meta")

  // thread local settings
  private final val localSettings = new ThreadLocal[Properties]() {
    override protected def initialValue: Properties = new Properties()
  }

  private final val InlineProperty = """.*\$\{(.+?)\}.*""".r
  
  /**
    * Loads the given properties files provided to Gwen on the command line in the order provided (later ones override
    * earlier ones). See class level comment for full details about how properties are loaded and their precedences.
    * 
    * @param cmdProperties command line properties passed into Gwen
    */
  def loadAll(cmdProperties: List[File]): Unit = {

    // create list of properties files in order of precedence (stripping out any duplicates)
    val pFiles = cmdProperties.reverse ++ List(workingProperties, defaultProperties).flatten
    val propFiles = pFiles.foldLeft(userProperties.toList) { FileIO.appendFile }

    // load files in reverse  order to ensure those with lesser precedence are overwritten by higher ones
    val props = propFiles.reverse.foldLeft(new Properties()) {
      (props, file) => 
        props.load(new FileReader(file))
        props.entrySet().asScala foreach { entry =>
          val key = entry.getKey.asInstanceOf[String]
          if (key == null || key.trim.isEmpty) invalidPropertyError(entry.toString, file)
        }
        props
    }

    // resolve all nested values and store in settings
    props.entrySet().asScala.foreach { entry =>
      val key = entry.getKey.asInstanceOf[String]
      if (!names.contains(key)) {
        try {
          val value = resolve(props.getProperty(key), props)
          Settings.set(key, value)
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
        getEnvOpt(key).getOrElse(props.getProperty(key))
      } else {
        getOpt(key).getOrElse(missingPropertyError(key))
      }
      val resolved = inline match {
        case InlineProperty(_) => resolve(inline, props)
        case _ => inline
      }
      resolve(value.replaceAll("""\$\{""" + key + """\}""", resolved), props)
    case _ => value
  }
  
  /**
    * Gets an optional property or enviroment variable (returns None if not found)
    * 
    * @param name the name of the property to get ( `env.` prefix for env VARs)
    */
  def getOpt(name: String): Option[String] = {
    getEnvOpt(name) match {
      case None => Option(localSettings.get.getProperty(name)).orElse(sys.props.get(name))
      case res @ _ => res
      
    }
  }

  /** 
   * Gets an optional environment variable.
   * 
   * @param name the name of the environment variable (with optional `.env` prefix)
   */
  def getEnvOpt(name: String) = 
    if (name.startsWith("env.") && name.length() > 4) {
      sys.env.get(name.substring(4))
    } else {
      sys.env.get(name)
    }

  /**
    * Gets a mandatory property (throws exception if not found)
    * 
    * @param name the name of the property to get
    * @throws gwen.errors.MissingPropertyException if no such property is set
    */
  def get(name: String): String = getOpt(name).getOrElse(missingPropertyError(name))

  /**
    * Finds all properties that match the given predicate.
    *
    * @param predicate name => Boolean
    * @return map of properties that match the predicate
    */
  def findAll(predicate: String => Boolean): Map[String, String] = entries.filter {
    case (name, _) => predicate(name)
  }

  /**
   * Provides access to multiple settings. This method merges a comma separated list of name-value pairs
   * set in the given multiName property with all name-value properties that start with singleName.
   * See: https://github.com/SeleniumHQ/selenium/wiki/DesiredCapabilities
   */
  def findAllMulti(multiName: String, singleName: String): Map[String, String] = {
    val assignments: Seq[String] = Settings.getOpt(multiName).map(_.split(",")).map(_.toList).getOrElse(Nil)
    val nvps: Seq[(String, String)] = assignments.map(_.split('=')).map { nvp =>
      if (nvp.length == 2) (nvp(0), nvp(1))
      else if (nvp.length == 1) (nvp(0), "")
      else propertyLoadError(nvp(0), "name-value pair expected")
    }
    (nvps ++ Settings.findAll(_.startsWith(s"$singleName.")).map { case (n, v) =>
      (n.substring(singleName.length + 1), v)
    }).toMap
  }

  /**
   * Provides access to multiple settings. This method merges a comma separated list of name-value pairs
   * set in the given multiName property with all name-value properties that start with singleName.
   * See: https://github.com/SeleniumHQ/selenium/wiki/DesiredCapabilities
   */
  def findAllMulti(name: String): List[String] = {
    val values = Settings.getOpt(name).map(_.split(",").toList.map(_.trim)).getOrElse(Nil)
    values ++ Settings.findAll(_.startsWith(s"$name.")).map { case (_, v) => v }
  }

  /** Gets all settings names (keys). */
  def names: mutable.Set[String] = localSettings.get.keySet.asScala.map(_.toString) ++ sys.props.keySet

  /** Gets number of names (keys). */
  def size: Int = names.size

  /** Gets all settings entries (name-value pairs). */
  def entries: Map[String, String] =
    localSettings.get.entrySet().asScala.map(entry => (entry.getKey.toString, entry.getValue.toString)).toMap ++
      sys.props.toMap.filter{case (k, _) => !localSettings.get.containsKey(k)}

  /**
    * Checks to see if a setting exists.
    * @param name the name of the setting to check
    * @return true if the setting exists, false otherwise
    */
  def contains(name: String): Boolean = getOpt(name).nonEmpty

  /**
    * Adds a system property (overrides any existing value)
    *
    * @param name the name of the property to add
    * @param value the value to bind to the property
    */
  def set(name: String, value: String): Unit = {
    sys.props += ((name, value))
  }

  /**
    * Clears local and global settings of the given name.
    *
    * @param name the name of the setting to clear
    */
  def clear(name: String): Unit = {
    localSettings.get.remove(name)
    sys.props -= name
  }
   /**
    * Adds a system property.
    * 
    * @param name the name of the property to add
    * @param value the value to bind to the property
    */
  private[gwen] def add(name: String, value: String, overrideIfExists: Boolean): Unit = {
    if (overrideIfExists || !contains(name)) {
      if (overrideIfExists && localSettings.get.containsKey(name)) {
        localSettings.get.setProperty(name, value)
      }
      set(name, value)
    }
  }

  /**
    * Adds a thread local Gwen setting (overrides any existing value)
    *
    * @param name the name of the setting to set
    * @param value the value to bind to the setting
    */
  def setLocal(name: String, value: String): Unit = {
    if (!name.startsWith("gwen.")) unsupportedLocalSetting(name)
    localSettings.get.setProperty(name, value)
  }

  /**
    * Clears a local Gwen settings entry.
    * @param name the setting entry to remove
    */
  def clearLocal(name: String): Unit = {
    if (!name.startsWith("gwen.")) unsupportedLocalSetting(name)
    localSettings.get.remove(name)
  }

  /**
    * Clears all local Gwen settings.
    */
  def clearLocal(): Unit = {
    localSettings.get.clear()
  }

  def exclusively[T](body: => T):T = {
    Settings.Lock.synchronized {
      body
    }
  }
  
}