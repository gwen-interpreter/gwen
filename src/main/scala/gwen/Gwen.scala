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

package gwen

import gwen.eval.GwenInterpreter
import gwen.eval.GwenLauncher
import gwen.eval.GwenREPL

import com.typesafe.scalalogging.LazyLogging
import gwen.eval.EvalContext

/**
  * Main Gwen application.
  * 
  * @param interpreter the gwen interpreter
  */
class Gwen[T <: EvalContext](interpreter: GwenInterpreter[T]) extends App with LazyLogging {
    
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
  private[gwen] def run(options: GwenOptions)(implicit launcher: GwenLauncher[T] = new GwenLauncher(interpreter)): Int = {
    val ctxOpt = if (options.batch) None else Some(interpreter.initialise(options))
    try {
      launcher.run(options, ctxOpt).exitCode tap { _ =>
        ctxOpt foreach { ctx =>
          ctx.withEnv { env =>
            if (options.features.nonEmpty || env.loadedMeta.nonEmpty) {
              printBanner("")
            }
          }
          createRepl(ctx).run()
        }
      }
    } finally {
      ctxOpt.foreach(_.close())
    }
  }
  
  /**
    * Returns the console REPL.
    * 
    * @param ctx the evaluation context
    */
  private[gwen] def createRepl(ctx: T): GwenREPL[T] = new GwenREPL[T](interpreter, ctx)
  
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
