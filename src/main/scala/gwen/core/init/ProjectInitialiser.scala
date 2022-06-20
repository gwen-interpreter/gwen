/*
 * Copyright 2022 Branko Juric, Brady Wood
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

package gwen.core.init

import gwen.core._

import scala.jdk.CollectionConverters._
import scala.util.chaining._

import java.io.File
import java.nio.file.Files

/**
  * Initilises a new Gwen project directory.
  */
trait ProjectInitialiser {

  val CurrentDir = new File(".")

  /**
    * Subclasses must implement this to perform project initialisation.
    *
    * @param isNew true if the project is new, false otherwise
    * @param flat true if initialising in current or nested directory
    * @param options Gwen options
    */
  def init(isNew: Boolean, flat: Boolean, options: GwenOptions): Unit

  /** Initialises a project direcotry given the user options */
  final def initProject(options: GwenOptions): Unit = {
    val gwenConf = List("gwen.conf", "gwen.json", "gwen.properties").map(f => new File(f)).find(_.exists)
    val isNew = gwenConf.isEmpty
    parseInitDirectory(isNew, gwenConf, options) flatMap { dir =>
      operation(dir, isNew, options) map { op =>
        val action = s"Initialising"
        println(("""|   _
                    |  { \," """ + action + " " + op + s"${if (options.initOptions.contains(InitOption.force)) " (forced)" else ""}" + """
                    | {_`/   
                    |    `   
                    |""").stripMargin)
        val flat = isCurrentDir(dir)
        if (isNew) dir.mkdirs()
        if (dir.isSame(options.initDir)) {
          init(isNew, flat, options)
        } else {
          init(isNew, flat, options.copy(initDir = dir))
        }
      }
    } getOrElse {
      println("No operation performed. The project may not have been initialised.")
      println()
    }
  }

  private def parseInitDirectory(isNew: Boolean, gwenConf: Option[File], options: GwenOptions): Option[File] = {

    def okIfHasFeaturesDir(dir: File): Option[File] = {
      if (dir.containsDir("features")) {
        Some(dir)
      } else {
        Errors.initProjectError(s"Cannot initialise ${operation(dir, false, options).getOrElse(dirName(dir))}. The project may not have been first initialised there.")
      }
    }

    val dir = options.initDir
    val force = options.initOptions.contains(InitOption.force)
    if (isNew) {
      if(dir.exists && dir.isFile) {
        Errors.initProjectError(s"Cannot initialise ${dirName(dir)} because a file of that name already exists")
      }
      Some(dir)
    } else if (options.initOptions.contains(InitOption.docker) || options.initOptions.contains(InitOption.jenkins)) {
      okIfHasFeaturesDir(dir)
    } else {
      if (force) {
        okIfHasFeaturesDir(dir)
      } else {
        gwenConf.map(_.getPath) foreach { confFile =>
          Errors.initProjectError(s"$confFile file found. This may be an existing project (use --force option to replace).")
        }
        None
      }
    }
  }

  private def dirName(dir: File): String = s"${if (isCurrentDir(dir)) "current" else dir.getPath} directory"

  private def isCurrentDir(dir: File): Boolean = dir.isSame(CurrentDir)

  private def operation(dir: File, isNew: Boolean, options: GwenOptions): Option[String] = {
    if (isNew) {
      Some(s"project in ${dirName(dir)}")
    } else if (options.initOptions.contains(InitOption.docker)) {
      Some(s"Docker${if (options.initOptions.contains(InitOption.jenkins)) " and Jenkins" else ""} files in ${dirName(dir)}")
    } else if (options.initOptions.contains(InitOption.jenkins)) {
      Some(s"Jenkinsfile in ${dirName(dir)}")
    } else {
      if (options.initOptions.contains(InitOption.force)) {
        Some(s"project in ${dirName(dir)}")
      } else {
        None
      }
    }
  }
  
}
