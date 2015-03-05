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

import scala.collection.JavaConversions._
import java.io.File
import com.typesafe.config.ConfigFactory
import com.typesafe.config.Config
import scala.util.Properties
import java.util.Properties
import java.io.FileReader
import scala.annotation.tailrec

/**
 * Provides access to system properties loaded from properties files.
 * If a gwen.properties file exists in the user's home directory, then
 * its properties are loaded first. Once a property is loaded it is never
 * replaced. Therefore it is important to load properties in the right 
 * order.
 *
 * @author Branko Juric
 */
object Settings {
  
  private val InlineProperty = """.*\$\{(.+?)\}.*""".r
  
  loadAll(UserOverrides.UserProperties.toList)
  
  /**
   * Loads all properties from the given files.
   * 
   * @param propsFiles
   * 			the properties files to load
   */
  def loadAll(propsFiles: List[File]): Unit = {
    val props = propsFiles.foldLeft(new Properties()) { 
      (props, file) => 
        props.load(new FileReader(file))
        props
    }
    props.entrySet() foreach { entry =>
      val key = entry.getKey().asInstanceOf[String]
      if (!sys.props.contains(key)) {
        val value = resolve(props.getProperty(key), props)
        sys.props += ((key, value))
      }
    }
  }
  
  @tailrec
  private[gwen] def resolve(value: String, props: Properties): String = value match {
    case InlineProperty(key) =>
      val inline = if (props.containsKey(key)) {
        props.getProperty(key)
      } else {
        sys.props.get(key).getOrElse(sys.error(s"Property not found: $key"))
      }
      resolve(value.replaceAll("""\$\{""" + key + """\}""", inline), props)
    case _ => value
  }
  
  /**
   * Gets an optional property (returns None if not found)
   * 
   * @param name 
   * 			the name of the property to get
   */
  def getOpt(name: String): Option[String] = sys.props.get(name)
  
  /**
   * Gets a mandatory property (throws exception if not found)
   * 
   * @param name 
   * 			the name of the property to get
   */
  def get(name: String): String = getOpt(name).getOrElse(sys.error(s"System property $name not set"))
  
  /**
   * Adds a property.
   * 
   * @param name
   * 			the name of the property to add
   * @param value
   * 			the value to bind to the property
   */
  def add(name: String, value: String): Unit = {
    sys.props += ((name, value))
  }
  
}