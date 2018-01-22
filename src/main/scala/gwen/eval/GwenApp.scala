/*
 * Copyright 2014-2015 Branko Juric, Brady Wood
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

import gwen.Predefs.Kestrel

/**
  * Gwen interpreter application.
  * 
  * @param interpreter the gwen interpreter
  */
class GwenApp[T <: EnvContext](interpreter: GwenInterpreter[T]) extends App {
    
  printBanner("Welcome to ")
  println()

  try {
    System.exit(run(GwenOptions(args)))
  } catch {
    case ie: gwen.errors.InvocationException =>
      System.err.println(ie.getMessage)
      System.exit(1)
    case ge: gwen.errors.GwenException =>
      System.err.println(s"ERROR - ${ge.getMessage}")
      System.exit(1)
    case e: Throwable =>
      e.printStackTrace()
      System.err.println(s"ERROR - ${e.getMessage}")
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
      envOpt foreach { interpreter.close }
    }
  }
  
  /**
    * Returns the console REPL.
    * 
    * @param env the environment context
    */
  private[eval] def createRepl(env: T): GwenREPL[T] = new GwenREPL[T](interpreter, env)
  
  private def printBanner(intro: String) {
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
