/*
 * Copyright 2014-2019 Branko Juric, Brady Wood
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

import gwen.Predefs.Exceptions
import gwen.Predefs.Kestrel

import com.typesafe.scalalogging.LazyLogging

/**
  * Gwen interpreter application.
  * 
  * @param interpreter the gwen interpreter
  */
class GwenApp[T <: EnvContext](interpreter: GwenInterpreter[T]) extends App with LazyLogging {
    
  printBanner("Welcome to ")
  println()

  try {
    System.exit(run(GwenOptions(args)))
  } catch {
    case e: Throwable =>
      logger.whenDebugEnabled {
        println(e.writeStackTrace())
      }
      System.err.println(s"ERROR - ${e.getMessage}")
      println()
      System.exit(1)
  }
  
  /** 
    * Runs the interpreter with the given options
    * 
    * @param options the command line options
    * @return 0 if successful; 1 otherwise
    */  
  private[eval] def run(options: GwenOptions)(implicit launcher: GwenLauncher[T] = new GwenLauncher(interpreter)): Int = {
    val envOpt = if (options.batch) None else Some(interpreter.initialise(options))
    try {
      launcher.run(options, envOpt).exitCode tap { _ =>
        envOpt foreach { env =>
          if (options.features.nonEmpty || env.loadedMeta.nonEmpty) {
            printBanner("")
          }
          createRepl(env).run()
        }
      }
    } finally {
      envOpt foreach { _.close() }
    }
  }
  
  /**
    * Returns the console REPL.
    * 
    * @param env the environment context
    */
  private[eval] def createRepl(env: T): GwenREPL[T] = new GwenREPL[T](interpreter, env)
  
  private def printBanner(intro: String): Unit = {
    println(("""|
                |   __ ___      _____ _ __     _
                |  / _` \ \ /\ / / _ \ '_ \   { \,"
                | | (_| |\ V  V /  __/ | | | {_`/
                |  \__, | \_/\_/ \___|_| |_|   `
                |  |___/
                |
                |""" + intro + interpreter.implName + " v" + interpreter.implVersion + interpreter.noticeMsg.map(msg => s"${System.lineSeparator}$msg").getOrElse("")).stripMargin)
  }
  
}
