/*
 * Copyright 2014-2021 Branko Juric, Brady Wood
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

import gwen.core._
import gwen.core.eval.EvalContext
import gwen.core.eval.EvalEngine
import gwen.core.eval.GwenLauncher
import gwen.core.eval.GwenREPL
import gwen.core.node.gherkin.Dialect
import gwen.core.state.EnvState
import gwen.core.status.Failed

import com.typesafe.scalalogging.LazyLogging

/**
  * Default Gwen interpreter application.
  */
object DefaultGwenInterpreter extends GwenInterpreter(EvalEngine.DefaultInstance)

/**
  * Main Gwen application superclass.
  *
  * @param engine the evaluation engine
  */
class GwenInterpreter[T <: EvalContext](engine: EvalEngine[T]) extends GwenLauncher(engine) with GwenInfo with LazyLogging {

  def main(args: Array[String]): Unit = {
    printBanner("Welcome to ")
    println()
    val start = System.nanoTime
    try {
      val options = GwenOptions(args)
      logger.info("Initialising settings")
      Settings.init(options.configFiles*)
      GwenSettings.check()
      Dialect.instance
      System.exit(run(options))
    } catch {
      case e: Throwable =>
        logger.whenDebugEnabled {
          println(e.writeStackTrace())
        }
        System.err.println(s"ERROR - ${Failed(System.nanoTime - start, e).message}")
        println()
        System.exit(1)
    }
  }

  /**
    * Runs the interpreter with the given options
    *
    * @param options the command line options
    * @param launcher Gwen launcher
    * @return 0 if successful; 1 otherwise
    */
  private [gwen] def run(options: GwenOptions): Int = {
    val ctxOpt = if (options.batch || options.init) None else Some(engine.init(options, EnvState()))
    try {
      val evalStatus = run(options, ctxOpt)
      if (!options.init) {
        ctxOpt foreach { ctx =>
          if (evalStatus.isEvaluated || evalStatus.isLoaded) {
            printBanner("")
          }
          createRepl(ctx).run()
        }
      }
      evalStatus.exitCode
    } finally {
      ctxOpt.foreach(_.close())
    }
  }

  /**
    * Returns the console REPL.
    *
    * @param ctx the evaluation context
    */
  private [gwen] def createRepl(ctx: T): GwenREPL[T] = new GwenREPL[T](engine, ctx)

  private def printBanner(intro: String): Unit = {
    println(("""|
                |                              _
                |   __ ___      _____ _ __    { \,"
                |  / _` \ \ /\ / / _ \ '_ \  {_`/
                | | (_| |\ V  V /  __/ | | |   `
                |  \__, | \_/\_/ \___|_| |_|
                |  |___/
                |
                |""" + intro + implName + " v" + implVersion + noticeMsg.map(msg => s"${System.lineSeparator}$msg").getOrElse("")).stripMargin)
  }

}
