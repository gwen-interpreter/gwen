/*
 * Copyright 2015-2021 Branko Juric, Brady Wood
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

import gwen.core.report.ReportFormat
import gwen.core.state.SensitiveData

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import com.typesafe.config.ConfigValueType
import com.typesafe.scalalogging.LazyLogging

import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import scala.util.chaining._

import java.io.File
import java.io.FileReader
import java.util.Properties

/**
  * Provides access to enviornment variables and system properties loaded from JSON, HOCON or properties files. 
  */
object Settings extends LazyLogging {

  object Lock

  private val InlineProperty = """.*\$\{(.+?)\}.*""".r
  private val MaskedNameSuffix = ":masked"
  private val MaskedNamePattern = s"(.+?)$MaskedNameSuffix".r

  // thread local settings
  private val localSettings = new ThreadLocal[Properties]() {
    override protected def initialValue: Properties = new Properties()
  }

  val UserMeta: Option[File] = FileIO.getUserFile("gwen.meta")

  private def settingsFileInDir(dir: File, name: String): Option[File] = {
    val files = List("conf", "json", "properties") map { ext => 
      new File(dir, s"$name.$ext")
    } filter(_.exists)
    if (files.size > 1) {
      logger.warn(s"Multiple settings files found in directory: ${dir.getPath} >> Will load ${files.head.getName}")
    }
    files.headOption
  } 

  /**
    * Initialises all settings in the following order of precedence:
    *   1. System properties passed through -D Java command line option
    *   2. ~/gwen.properties (user overrides)
    *   3. Settings files passed into this method (@launchSettings param)
    *      - later ones override earlier ones
    *   4. Project settings (gwen settings in your project root)
    *   5. Gwen defaults
    */
  def init(settingsFiles: File*): Unit = {
    init(prime = true, settingsFiles*)
  }

  /**
    * Adds the given settings.
    */
  def add(settingsFiles: File*): Unit = {
    init(prime = false, settingsFiles*)
  }

  /**
    * Loads and initialises all settings in the following order of precedence:
    *   1. System properties passed through -D Java command line option
    *   2. ~/gwen.properties (user overrides)
    *   3. Settings files passed into this method (@launchSettings param)
    *      - later ones override earlier ones
    *   4. Project settings (gwen settings in your project root)
    *   5. Gwen defaults
    */
  private def init(prime: Boolean, launchSettings: File*): Unit = {

    val settingsFiles = {
      if (prime) {
        val userSettingsFile: Option[File] = FileIO.userDir.flatMap(d => settingsFileInDir(d, "gwen"))
        val projectSettingsFile: Option[File] = settingsFileInDir(new File("."), "gwen")
        (launchSettings.reverse ++ projectSettingsFile.toList).foldLeft(userSettingsFile.toList) { FileIO.appendFile }
      } else {
        launchSettings.toList.reverse
      }
    }

    val (config, orphans) = load(settingsFiles*)
    
    // write into properties object
    val props = config.entrySet.asScala.foldLeft(new Properties()) {
      (properties, entry) =>
        val name = entry.getKey.asInstanceOf[String].replace("\\", "").replace("\"", "")
        val cValue = entry.getValue
        if (ConfigValueType.LIST == cValue.valueType()) {
          config.getAnyRefList(entry.getKey).asScala.map(_.toString) foreach { value => 
            properties.setProperty(s"$name.${System.nanoTime()}", value)
          }
        } else {
          val value = cValue.unwrapped().toString
          properties.setProperty(name, value)
          orphans.remove(name)
        }
        properties
    }
    orphans.entrySet.asScala foreach { entry => 
      val key = entry.getKey.toString
      if (!props.contains(key)) {
        props.setProperty(key, entry.getValue.toString)
      }
    }

    if (prime) {
      // Make mask char available in sys props for GwenSettings.`gwen.mask.char` to work
      sys.props += (("gwen.mask.char", props.getProperty("gwen.mask.char")))

      // mask any senstive ("*:masked") properties passed in the raw from the command line
      sys.props.toMap filter { case (key, value) => 
        SensitiveData.isMaskedName(key) 
      } foreach { case (key, value) =>
        SensitiveData.parse(key, resolve(value, props)) foreach { case (mKey, mValue) => 
          sys.props -= key
          sys.props += ((mKey, mValue))
        }
      }
    }

    // resolve and store all settings
    props.entrySet.asScala.foreach { entry =>
      val name = entry.getKey.asInstanceOf[String]
      if (prime || !sys.props.contains(name)) {
        val rawValue = entry.getValue.toString
        val value = SensitiveData.parse(name, resolve(rawValue, props)) map { (_, mValue) => 
          sys.props -= name
          mValue
        } getOrElse {
          rawValue
        }
        val rValue = resolve(value, props)
        Settings.add(name, rValue, overrideIfExists = true)
      }
    }
    
  }

   /**
    * Loads and initialises the given settings files
    */
  private [core] def load(settingsFiles: File*): (Config, Properties) = {
    val orphans = new Properties(); // track orphaned properties so we don't lose a.b=x if a.b.c=y is loaded
    val config = settingsFiles.toList.filter(!_.exists) match {
      case Nil =>
        (
          settingsFiles.foldLeft(ConfigFactory.defaultOverrides()) { (conf, settingsFile) => 
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

    (config, orphans)
    
  }

  /**
   * Resolves a given property by performing any property substitutions.
   * 
   * @param value the value to resolve
   * @param props the properties already read (candidates for substitution)
   * @throws MissingPropertyException if a property cannot be substituted 
   *         because it is missing from the given props
   */
  private [core] def resolve(value: String, props: Properties): String = value match {
    case InlineProperty(key) =>
      val inline = if (props.containsKey(key)) {
        getEnvOpt(key).getOrElse(props.getProperty(key))
      } else {
        getOpt(key).getOrElse(Errors.missingSettingError(key))
      }
      val resolved = inline match {
        case InlineProperty(_) => resolve(inline, props)
        case _ => inline
      }
      resolve(value.replaceAll("""\$\{""" + key + """\}""", resolved), props)
    case _ => value
  }

  /**
   * Resolves a given property by performing any property substitutions.
   * 
   * @param value the value to resolve
   * @param conf the config already read (candidates for substitution)
   * @throws MissingPropertyException if a property cannot be substituted 
   *         because it is missing from the given props
   */
  private [core] def resolve(value: String, conf: Config): String = {
    value match {
      case InlineProperty(key) =>
        val inline = if (conf.hasPath(key)) {
          getEnvOpt(key).getOrElse(conf.getString(key))
        } else {
          getOpt(key).getOrElse(Errors.missingSettingError(key))
        }
        val resolved = inline match {
          case InlineProperty(_) => resolve(inline, conf)
          case _ => inline
        }
        resolve(value.replaceAll("""\$\{""" + key + """\}""", resolved), conf)
      case _ => value
    }
  }

  /**
    * Gets an optional setting that could be deprecated or an enviroment variable (returns None if not found)
    * 
    * @param name the name of the setting to get ( `env.` prefix for env VARs)
    * @param deprecatedName the deprecated name of the setting to get ( `env.` prefix for env VARs)
    * @param config optional config object to read setting from
    */
  def getOpt(name: String, deprecatedName: Option[String] = None, config: Option[Config] = None): Option[String] = {
    deprecatedName flatMap { dName =>
      getOpt(dName, None, config) tap { value =>
        if (value.nonEmpty) {
          Deprecation.fail("Setting", dName, name)
        }
      }
    } orElse {
      getEnvOpt(name) match {
        case None => 
          Option(localSettings.get.getProperty(name)) orElse {
            config flatMap { conf => 
              if (conf.hasPath(name)) Option(resolve(conf.getString(name), conf)) else None
            } orElse {
              sys.props.get(name)
            }
          }
        case res @ _ => res
      }
    }
  }

  /** 
   * Gets an optional environment variable.
   * 
   * @param name the name of the environment variable (with optional `.env` prefix)
   */
  def getEnvOpt(name: String): Option[String] = {
    if (name.startsWith("env.") && name.length() > 4) {
      sys.env.get(name.substring(4))
    } else {
      sys.env.get(name)
    }
  }

  /**
    * Gets an mandatory setting that could be deprecated or an enviroment variable (returns None if not found)
    * 
    * @param name the name of the setting to get ( `env.` prefix for env VARs)
    * @param deprecatedName optional deprecated name of the setting to get ( `env.` prefix for env VARs)
    * @param config optional config object to read setting from
    */
  def get(name: String, deprecatedName: Option[String] = None, config: Option[Config] = None): String = {
    getOpt(name, deprecatedName, config).getOrElse(Errors.missingSettingError(name))
  }

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
  def getMap(multiName: String, singleName: String): Map[String, String] = {
    val nvps: Seq[(String, String)] = Settings.getList(multiName).map(_.split('=')).map { nvp =>
      if (nvp.length == 2) (nvp(0), nvp(1))
      else if (nvp.length == 1) (nvp(0), "")
      else Errors.propertyLoadError(nvp(0), "name-value pair expected")
    }
    (nvps ++ Settings.findAll(_.startsWith(s"$singleName.")).map { case (n, v) =>
      (n.substring(singleName.length + 1), v)
    }).toMap
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
    sys.props += SensitiveData.parse(name, value).getOrElse((name, value))
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
  private [core] def add(name: String, value: String, overrideIfExists: Boolean): Unit = {
    if (overrideIfExists || !contains(name)) {
      if (overrideIfExists && localSettings.get.containsKey(name)) {
        if (SensitiveData.isMaskedName(name)) {
          Errors.unsupportedMaskedPropertyError(s"Cannot mask $name: Masks can only be defined in proeprties files.")
        }
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

  def exclusively[T](body: => T):T = {
    Settings.Lock.synchronized {
      body
    }
  }

  def getList(name: String, deprecatedName: Option[String] = None, config: Option[Config] = None): List[String] = {
    (config map { conf => 
      deprecatedName map { dName =>
        getList(dName, conf) match {
          case Nil =>
            getList(name, conf)
          case _ =>
            Deprecation.fail("Setting", dName, name)
            Nil
        }
      } getOrElse {
        getList(name, conf)
      }
    } getOrElse {
      getOpt(name, deprecatedName, config).map(_.split(",").toList.map(_.trim).filter(_.length > 0)).getOrElse(Nil)
    }) ++ (
      if (config.isEmpty) {
        Settings.findAll(_.startsWith(s"$name.")).map { case (n, v) => v }
      } else {
        Nil
      }
    )
  }

  private def getList(name: String, config: Config): List[String] = {
    Try(config.getAnyRefList(name)) match {
      case Success(list) => 
        list.asScala.toList.map(_.toString).map(value => resolve(value, config))
      case _ => 
        Nil
    }
  }

  def getBoolean(name: String, deprecatedName: Option[String] = None, config: Option[Config] = None): Boolean = {
    getBooleanOpt(name, deprecatedName, config).getOrElse(Errors.missingSettingError(name))
  }

  def getBooleanOpt(name: String, deprecatedName: Option[String] = None, config: Option[Config] = None): Option[Boolean] = {
    getOptAndConvert(name, deprecatedName, Set(true, false).mkString(", "), config) { value =>
      value.toBoolean
    }
  }

  def getLong(name: String, deprecatedName: Option[String] = None, config: Option[Config] = None): Long = {
    getLongOpt(name, deprecatedName, config).getOrElse(Errors.missingSettingError(name))
  }

  def getLongOpt(name: String, deprecatedName: Option[String] = None, config: Option[Config] = None): Option[Long] = {
    getOptAndConvert(name, deprecatedName, "Long integers", config) { value =>
      value.toLong 
    }
  }

  def getInt(name: String, deprecatedName: Option[String] = None, config: Option[Config] = None): Int = {
    getIntOpt(name, deprecatedName, config).getOrElse(Errors.missingSettingError(name))
  }

  def getIntOpt(name: String, deprecatedName: Option[String] = None, config: Option[Config] = None): Option[Int] = {
    getOptAndConvert(name, deprecatedName, "Integers", config) { value =>
      value.toInt 
    }
  }

  def getFile(name: String, deprecatedName: Option[String] = None, config: Option[Config] = None): File = {
    getFileOpt(name, deprecatedName, config).getOrElse(Errors.missingSettingError(name))
  }

  def getFileOpt(name: String, deprecatedName: Option[String] = None, config: Option[Config] = None): Option[File] = {
    getOptAndConvert(name, deprecatedName, "File system directories", config) { value =>
      toFile(value)
    }
  }

  def getReportFormat(name: String, value: String): ReportFormat = {
    convert(name, value, ReportFormat.values.mkString(", ")) { value =>
      ReportFormat.valueOf(value)
    }
  }

  def getAndConvert[T](name: String, deprecatedName: Option[String], validValues: String, config: Option[Config] = None)(conversion: String => T): T = {
    (getOptAndConvert(name, deprecatedName, validValues, config)(conversion)).getOrElse(Errors.missingSettingError(name))
  }

  def getOptAndConvert[T](name: String, deprecatedName: Option[String], validValues: String, config: Option[Config] = None)(conversion: String => T): Option[T] = {
    getOpt(name, deprecatedName, config) map { value =>
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

  def toFile(path: String): File = {
    if (path.startsWith("./") || path.startsWith(".\\")) {
      if (path.length > 2) new File(path.substring(2))
      else new File(".")
    } else {
      new File(path)
    }
  }
  
}