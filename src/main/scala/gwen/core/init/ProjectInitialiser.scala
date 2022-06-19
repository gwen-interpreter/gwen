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

  private val CurrentDir = new File(".")

  /**
    * Subclasses must implement this to perform project initialisation.
    *
    * @param isNew true if the project is new, false otherwise
    * @param standalone true if initialising a standalone project, false otherwise
    * @param options Gwen options
    */
  def init(isNew: Boolean, standalone: Boolean, options: GwenOptions): Unit

  /** Initialises a project direcotry given the user options */
  final def initProject(options: GwenOptions): Unit = {
    val gwenConf = List("gwen.conf", "gwen.json", "gwen.properties").map(f => new File(f)).find(_.exists)
    val isNew = gwenConf.isEmpty
    parseInitDirectory(isNew, gwenConf, options) flatMap { dir =>
      operation(dir, isNew, options) map { op =>
        println(("""|   _
                    |  { \," Initialising """ + op + """
                    | {_`/   
                    |    `   
                    |""").stripMargin)
        val standalone = isCurrentDir(dir)
        if (isNew) dir.mkdirs()
        if (dir.isSame(options.initDir)) {
          init(isNew, standalone, options)
        } else {
          init(isNew, standalone, options.copy(initDir = dir))
        }
      }
    } getOrElse {
      println("No operation performed (the project may not have been initialised).")
      println()
    }
  }

  private def parseInitDirectory(isNew: Boolean, gwenConf: Option[File], options: GwenOptions): Option[File] = {
    val dir = options.initDir
    if (isNew) {
      if(dir.exists) {
        if (dir.isFile) {
          Errors.initProjectError(s"Cannot initialise ${dirName(dir)} because a file of that name already exists")
        } else if (Files.newDirectoryStream(dir.toPath).iterator.hasNext) {
          Errors.initProjectError(s"Cannot initialise ${dirName(dir)} because it is not empty")
        } 
      }
      Some(dir)
    } else if (options.docker || options.jenkins) {
      if (dir.containsDir("features")) {
        Some(dir)
      } else {
        Errors.initProjectError(s"Cannot initialise ${operation(dir, false, options).getOrElse(dirName(dir))} (project may not have been pre-initialised there: try with trailing dot or a subdirectory)")
      }
    } else {
      gwenConf.map(_.getPath) foreach { confFile =>
        Errors.initProjectError(s"$confFile file found in current directory. The project may already have been initialised.")
      }
      None
    }
  }

  private def dirName(dir: File): String = s"${if (isCurrentDir(dir)) "current" else dir.getPath} directory"

  private def isCurrentDir(dir: File): Boolean = dir.isSame(CurrentDir)

  private def operation(dir: File, isNew: Boolean, options: GwenOptions): Option[String] = {
    if (isNew) {
      Some(s"project in ${dirName(dir)}")
    } else if (options.docker) {
      Some(s"Docker files in ${dirName(dir)}")
    } else if (options.jenkins) {
      Some(s"Jenkinsfile in ${dirName(dir)}")
    } else {
      None
    }
  }
  
}
