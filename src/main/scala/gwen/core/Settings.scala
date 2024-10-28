/*
 * Copyright 2024 - Branko Juric, Brady Wood
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

package gwen.core

import gwen.core.state.SensitiveData

import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.util.chaining._
import scala.util.Try

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import net.minidev.json.JSONArray

import scala.util.Success
import scala.jdk.CollectionConverters._
import scala.compiletime.uninitialized

import java.io.File
import java.util.Properties
import java.io.FileReader
import com.typesafe.config.ConfigValueType

object Settings extends LazyLogging {

  private object Lock

  private var config: Config = uninitialized
  private var configProps: Properties = uninitialized
  private val envOverrides = mutable.Map[String, String]()

  // thread local settings
  private val localSettings = new ThreadLocal[Properties]() {
    override protected def initialValue: Properties = new Properties()
  }
  
  private val resolver = new Interpolator(name => getOpt(name)).settings
  
  private val userProjectSettingsFiles: List[File] = {
    val userSettingsFile: Option[File] = FileIO.userDir.flatMap(d => settingsFileInDir(d, "gwen"))
    val projectSettingsFile: Option[File] = settingsFileInDir(new File("."), "gwen")
    userSettingsFile.toList ++ projectSettingsFile.toList
  }

  init()
  
  private def settingsFileInDir(dir: File, name: String): Option[File] = {
    val files = List("conf", "json", "properties") map { ext => 
      new File(dir, s"$name.$ext")
    } filter(_.exists)
    if (files.size > 1) {
      logger.warn(s"Multiple settings files found in directory: ${dir.getPath} >> Will load ${files.head.getName}")
    }
    files.headOption
  }
  
  val UserMeta: Option[File] = FileIO.getUserFile("gwen.meta")

  def exclusively[T](body: => T):T = {
    Lock.synchronized {
      body
    }
  }

  def addEnvOverrides(overrides: (String, String)*): Unit = {
    envOverrides ++= overrides
  }

  def init(settingsFiles: List[File] = Nil): Unit = {

    exclusively {

      configProps = new Properties()
      val orphans = new Properties()
      val sFiles = (settingsFiles.foldLeft(userProjectSettingsFiles) { FileIO.appendFile }).reverse
      config = sFiles filter(!_.exists) match {
        case Nil =>
          (
            sFiles.foldLeft(ConfigFactory.defaultOverrides()) { (conf, settingsFile) => 
              conf.withFallback(
                if (FileIO.hasFileExtension("properties", settingsFile)) {
                  val props = new Properties()
                  props.load(new FileReader(settingsFile))
                  props.entrySet.asScala foreach { entry => 
                    val key = entry.getKey.toString
                    if (!orphans.contains(key)) {
                      orphans.setProperty(key, entry.getValue.toString)
                    }
                  }
                  ConfigFactory.parseProperties(props)
                } else {
                  ConfigFactory.parseFile(settingsFile)
                }
              )
            }
          ).withFallback(ConfigFactory.load("gwen")).resolve()
        case filesNotFound =>
          Errors.settingsNotFound(filesNotFound)
      }

      // store orphaned properties so we don't lose a.b=x if a.b.c=y is loaded
      orphans.entrySet.asScala foreach { entry => 
        val name = entry.getKey.toString
        if (!name.contains("\"")) configProps.setProperty(name, entry.getValue.toString)
      }

      names map { name => 
        (name, name.replaceAll("\"", ""))
      } foreach { (name, parsedName) => 
        if (SensitiveData.isMaskedName(parsedName)) {
          SensitiveData.parse(parsedName, get(name)) foreach { case (mKey, mValue) => 
            configProps.setProperty(mKey, mValue)
          }
        }
        else if (name != parsedName) {
          configProps.setProperty(parsedName, resolve(config.getString(name)))
        }
      }
    }

  }

  /**
    * Gets an mandatory setting that could be deprecated or an enviroment variable (returns None if not found)
    * 
    * @param name the name of the setting to get ( `env.` prefix for env VARs)
    * @param deprecatedName optional deprecated name of the setting to get ( `env.` prefix for env VARs)
    */
  def get(name: String, deprecatedName: Option[String] = None): String = {
    getOpt(name, deprecatedName).getOrElse(Errors.missingSettingError(name))
  }

  /**
    * Gets an optional setting that could be deprecated or an enviroment variable (returns None if not found)
    * 
    * @param name the name of the setting to get ( `env.` prefix for env VARs)
    * @param deprecatedName the deprecated name of the setting to get ( `env.` prefix for env VARs)
    */
  def getOpt(name: String, deprecatedName: Option[String] = None): Option[String] = {
    deprecatedName flatMap { dName =>
      getOpt(dName, None) tap { value =>
        if (value.nonEmpty) {
          Deprecation.fail("Setting", dName, Some(name))
        }
      }
    } orElse {
      getEnvOpt(name) match {
        case None => 
          name match {
            case r"(.+?)$n:JSONArray" => 
              Option(getList(n)) map { lv => 
                if (lv.isEmpty) Errors.missingSettingError(n)
                JSONArray.toJSONString(lv.asJava)
              }
            case _ =>
              Option(localSettings.get.getProperty(name)) orElse {
                sys.props.get(name) orElse {
                  Option(configProps.getProperty(name)) orElse {
                    Try(config.getString(name)).map(v => Option(v)).getOrElse(None)
                  }
                }
              } map resolve
          }
        case res @ _ => res
      }
    }
  }

  /**
   * Resolves a given property by performing any property substitutions.
   * 
   * @param value the value to resolve
   */
  private def resolve(value: String): String = {
    try {
      resolver.interpolate(value)
    } catch {
      case e: Errors.UnboundReferenceException => 
        Errors.missingSettingError(e.name)
    }
  }

  /** 
   * Gets an optional environment variable.
   * 
   * @param name the name of the environment variable (with optional `.env` prefix)
   */
  private def getEnvOpt(name: String): Option[String] = {
    if (name.startsWith("env.") && name.length() > 4) {
      sys.env.get(name.substring(4))
    } else {
      envOverrides.get(name) map { envName =>
        sys.env.get(envName)
      } getOrElse sys.env.get(name)
    }
  }

  def getList(name: String, deprecatedName: Option[String] = None): List[String] = {
    def getConList(name: String, deprecatedName: Option[String] = None): List[String] = {
      deprecatedName map { dName =>
        Try(config.getAnyRefList(dName)) match {
          case Success(list) => 
            Deprecation.fail("Setting", dName, Some(name))
            Nil
          case _ => 
            getList(name, None)
        }
      } getOrElse {
        Try(config.getAnyRefList(name)) match {
          case Success(list) => 
            list.asScala.toList.map(_.toString).map(value => resolve(value))
          case _ => 
            Nil
        }
      }
    }
    def getPropsList(props: Properties, name: String, deprecatedName: Option[String] = None): List[String] = {
      val keys = props.keySet.asScala.map(_.toString).toList.sorted
      deprecatedName map { dName =>
        if (keys.filter(_.startsWith(s"$dName.")).nonEmpty) {
          Deprecation.fail("Setting", dName, Some(name))
          Nil
        } else Nil
      } getOrElse {
        keys.filter(_.startsWith(s"$name.")) filter { key => 
          key.substring(name.length() + 1).count(_ == '.') < 2
        } map { key => 
          props.getProperty(key)
        }
      }
    }
    def getCsvList(name: String, deprecatedName: Option[String] = None): List[String] = {
      getOpt(name, deprecatedName).map(_.split(",").toList.map(_.trim).filter(_.length > 0)).getOrElse(Nil)
    }
    List(
      getConList(name, deprecatedName),
      getPropsList(System.getProperties, name, deprecatedName),
      getPropsList(configProps, name, deprecatedName),
      getCsvList(name, deprecatedName)
    ).flatten.distinct
  }

  def getFile(name: String): File = {
    toFile(get(name))
  }

  def getFileOpt(name: String, deprecatedName: Option[String] = None): Option[File] = {
    getOptAndConvert(name, deprecatedName, "File system directories") { value =>
      toFile(value)
    }
  }

  def toFile(path: String): File = {
    if (path.startsWith("./") || path.startsWith(".\\")) {
      if (path.length > 2) new File(path.substring(2))
      else new File(".")
    } else {
      new File(path)
    }
  }

  def getBoolean(name: String, deprecatedName: Option[String] = None): Boolean = {
    getBooleanOpt(name, deprecatedName).getOrElse(Errors.missingSettingError(name))
  }

  def getBooleanOpt(name: String, deprecatedName: Option[String] = None): Option[Boolean] = {
    getOptAndConvert(name, deprecatedName, Set(true, false).mkString(", ")) { value =>
      value.toBoolean
    }
  }

  def getInt(name: String, deprecatedName: Option[String] = None): Int = {
    getIntOpt(name, deprecatedName).getOrElse(Errors.missingSettingError(name))
  }

  def getIntOpt(name: String, deprecatedName: Option[String] = None): Option[Int] = {
    getOptAndConvert(name, deprecatedName, "Integers") { value =>
      value.toInt 
    }
  }

  def getLong(name: String, deprecatedName: Option[String] = None): Long = {
    getLongOpt(name, deprecatedName).getOrElse(Errors.missingSettingError(name))
  }

  def getLongOpt(name: String, deprecatedName: Option[String] = None): Option[Long] = {
    getOptAndConvert(name, deprecatedName, "Long integers") { value =>
      value.toLong 
    }
  }

  def getOptAndConvert[T](name: String, deprecatedName: Option[String], validValues: String)(conversion: String => T): Option[T] = {
    getOpt(name, deprecatedName) map { value =>
      convert(name, value, validValues)(conversion)
    }
  }

  def convert[T](name: String, value: String, validValues: String)(conversion: String => T): T = {
    Try {
      conversion(value)
    } getOrElse {
      Errors.illegalSettingError(name, value, validValues)
    }
  }

  /**
   * Provides access to multiple settings. This method merges a comma separated list of name-value pairs
   * set in the given multiName property with all name-value properties that start with singleName.
   * See: https://github.com/SeleniumHQ/selenium/wiki/DesiredCapabilities
   */
  def getMap(multiName: String, singleName: String): Map[String, String] = {
    val nvps: Seq[(String, String)] = getList(multiName).map(_.split('=')).flatMap { nvp =>
      if (nvp.length == 2) Some((nvp(0), nvp(1)))
      else None
    }
    (nvps ++ findAll(_.startsWith(s"$singleName.")).map { case (n, v) =>
      (n.substring(singleName.length + 1), v)
    } ++ findAll(_.startsWith(s"$multiName.")).filter((_, v) => !v.contains(",")).map { case (n, v) =>
      (n.substring(multiName.length + 1), v)
    }).toMap
  }

  /** Gets all settings name entries. */
  private def names: Set[String] = {
    localSettings.get.keySet.asScala.toSet.map(_.toString) ++ sys.props.keySet.map(_.toString) ++ configProps.keySet.asScala.map(_.toString) ++ config.entrySet.asScala.map(_.getKey)
  }

  /**
    * Finds all settings that match the given predicate.
    *
    * @param predicate name => Boolean
    * @return map of properties that match the predicate
    */
  def findAll(predicate: String => Boolean): Map[String, String] = {
    val props = config.entrySet.asScala.filter(e => predicate(e.getKey)).foldLeft(new Properties()) {
      (properties, entry) => {
        val name = entry.getKey.replace("\\", "").replace("\"", "")
        val cValue = entry.getValue
        if (ConfigValueType.LIST == cValue.valueType()) {
          config.getAnyRefList(entry.getKey).asScala.zipWithIndex foreach { (value, idx) => 
            setProperties(s"$name.${Formatting.padWithZeroes(idx, 10)}", value, properties)
          }
        } else {
          setProperties(name, cValue.unwrapped, properties)
        }
        properties
      }
    }
    names.filter(predicate).filter(!_.contains("\"")) foreach { key => 
      if (!props.containsKey(key)) {
        getOpt(key) foreach { value => 
          props.setProperty(key, value)
        }
      }
    }
    props.asScala.toMap
  }

  private def setProperties(name: String, value: Object, properties: Properties): Unit = {
    Try(value.asInstanceOf[java.util.List[Object]]) match {
      case Success(list) =>
        list.asScala.zipWithIndex foreach { (listValue, idx) =>
          val listName = s"$name.${Formatting.padWithZeroes(idx, 10)}"
          Try(listValue.asInstanceOf[java.util.Map[String, Object]]) match {
            case Success(map) =>
              map.entrySet().asScala foreach { entry => 
                setProperties(s"$listName.${entry.getKey}", entry.getValue, properties)
              }
            case _ =>
              setProperties(listName, listValue, properties)
          }
        }
      case _ =>
        Try(value.asInstanceOf[java.util.Map[String, Object]]) match {
          case Success(map) =>
            map.entrySet().asScala foreach { entry => 
              setProperties(s"$name.${entry.getKey}", entry.getValue, properties)
            }
          case _ =>
            properties.setProperty(name, resolve(value.toString))
        }
    }
  }

  /**
    * Adds a thread local Gwen setting (overrides any existing value)
    *
    * @param name the name of the setting to set
    * @param value the value to bind to the setting
    */
  def set(name: String, value: String): Unit = {
    configProps.setProperty(name, value)
  }

  /**
    * Adds a thread local Gwen setting (overrides any existing value)
    *
    * @param name the name of the setting to set
    * @param value the value to bind to the setting
    */
  def setLocal(name: String, value: String): Unit = {
    if (!name.startsWith("gwen.")) Errors.unsupportedLocalSetting(name)
    val (n, v) = SensitiveData.parse(name, value).getOrElse((name, value))
    localSettings.get.setProperty(n, v)
  }

  /**
    * Clears a local Gwen settings entry.
    * @param name the setting entry to remove
    */
  def clearLocal(name: String): Unit = {
    if (!name.startsWith("gwen.")) Errors.unsupportedLocalSetting(name)
    localSettings.get.remove(name)
  }

  /**
    * Clears all local Gwen settings.
    */
  def clearLocal(): Unit = {
    localSettings.get.clear()
  }

}