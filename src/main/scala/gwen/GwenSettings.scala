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

package gwen

import scala.collection.JavaConversions._

import java.io.File
import com.typesafe.config.ConfigFactory
import com.typesafe.config.Config

/**
 * Provides access to system properties and default configuration settings.
 * This class uses the [[https://github.com/typesafehub/config Config]] 
 * library to load properties from the following sources:
 *
 *  - System properties
 *  - gwen.properties in the user's home directory (if found)
 *  - Any default properties found in reference.conf files in the classpath
 *
 * @author Branko Juric
 */
object gwenSetting {
  
  val config = 
    ConfigFactory.load()
      .withFallback(ConfigFactory.parseFile(
        new File(sys.props.get("user.home").get, "gwen.properties")))
      .withFallback(ConfigFactory.load("gwen"))
  
  // copy all loaded properties to system properties
  config.entrySet() foreach { entry =>
    val key = entry.getKey()
    if (!sys.props.contains(key)) {
      sys.props += ((key, config.getString(key)))
    }
  }
  
  def getOpt(path: String): Option[String] = sys.props.get(path) match {
    case None =>
      if (config.hasPath(path)) {
    	Option(config.getString(path))
      } else {
        None
      }
    case value => value
  }
  
  def get(path: String): String = getOpt(path) match {
    case Some(value) => value
    case None => config.getString(path)
  }
  
}