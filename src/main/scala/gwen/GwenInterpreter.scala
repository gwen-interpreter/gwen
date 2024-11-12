/*
 * Copyright 2014-2024 Branko Juric, Brady Wood
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
import gwen.core.init.NoopProjectInitialiser
import gwen.core.node.gherkin.Dialect
import gwen.core.report.console.ConsoleReporter
import gwen.core.state.EnvState
import gwen.core.status.Failed
import gwen.core.status.Pending

import ch.qos.logback.classic.LoggerContext
import ch.qos.logback.classic.joran.JoranConfigurator
import com.typesafe.scalalogging.LazyLogging
import org.fusesource.jansi.AnsiConsole
import org.slf4j.LoggerFactory;
import org.slf4j.bridge.SLF4JBridgeHandler

import scala.util.chaining._

import java.io.File
import java.io.OutputStream
import java.io.PrintStream
import java.net.URL
import java.util.{ logging => jul}
import java.util.Date
import gwen.core.node.FeatureStream


/**
  * Default Gwen interpreter application.
  */
object DefaultGwenInterpreter extends GwenInterpreter(EvalEngine())

object GwenInterpreter {
  def apply() = new GwenInterpreter(EvalEngine())
}

/**
  * Main Gwen application superclass.
  *
  * @param engine the evaluation engine
  */
class GwenInterpreter[T <: EvalContext](engine: EvalEngine[T]) extends GwenLauncher(engine) with LazyLogging with NoopProjectInitialiser with GwenInfo {

  Settings.addEnvOverrides(
    "gwen.launch.options.dryRun" -> "GWEN_DRY_RUN", 
    "gwen.launch.options.parallel" ->"GWEN_PARALLEL",
    "gwen.parallel.maxThreads" ->"GWEN_THREADS"
  )

  def main(args: Array[String]): Unit = {
    printBanner("Welcome to ", args)
    val start = System.nanoTime
    try {
      val options = init(GwenOptions(args, GwenSettings.`gwen.baseDir`))
      GwenSettings.check()
      initLogging(options)
      Dialect.instance
      System.exit(run(options))
    } catch {
      case _: Errors.GwenInterruptException =>
        System.exit(1) // user cntl-c initiated exit
      case e: Throwable =>
        val failure = Failed(System.nanoTime - start, e)
        val consoleReporter = new ConsoleReporter(GwenOptions())
        logger.error(s"${e.getClass.getSimpleName}\n\n" + consoleReporter.printError(failure), e)
        println()
        System.exit(1)
    }
  }

  def init(options: GwenOptions): GwenOptions = {
    System.out.println(s"Launching: ${options.commandString}")
    System.out.println()
    val profile = options.profile
    logger.info("Initialising settings")
    Settings.init(profile.settingsFile.toList ++ options.settingsFiles)
    (if (options.repl && options.features.nonEmpty) {
      val featureStream = new FeatureStream(options.metas, options.tagFilter)
      options.copy(
        metas = featureStream.readAll(options.features, options.dataFile).flatten.toList.flatMap(_.metaFiles),
        features = Nil
      )
    } else {
      options
    }) tap { opts => 
      List(
        ("Feature file or directory", options.features),
        ("Meta file or directory", options.metas),
        ("Input data file", options.dataFile.toList),
        ("Settings file", options.settingsFiles)
       ) foreach { (category, files) =>
        files foreach { file =>
          if (!file.exists) Errors.missingFileError(category, file)
        }
      }
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
    val ctxOpt = if (options.batch || options.init || options.pretty) {
      None 
    } else {
      Some(engine.init(options, EnvState()))
    }
    try {
      val evalStatus = run(options, ctxOpt)
      if (!options.init) {
        ctxOpt foreach { ctx =>
          if (options.verbose || (evalStatus.isEvaluated && options.features.nonEmpty)) {
            printBanner("", options.args.getOrElse(Array[String]()))
          }
          createRepl(ctx).run()
        }
      }
      evalStatus.exitCode
    } finally {
      ctxOpt foreach { ctx =>
        try {
          ctx.close()
        } catch {
          case e: Throwable => 
            logger.warn(s"Could not close context: $e")
        }
      }
    }
  }

  private def initLogging(options: GwenOptions): Unit = {

    if (ConsoleColors.isEnabled) {
      AnsiConsole.systemInstall()
    }

    if (options.verbose) {
      val context = LoggerFactory.getILoggerFactory().asInstanceOf[LoggerContext]
      val configurator = new JoranConfigurator()
      val config = getClass.getResource("/logback-verbose.xml")
      configurator.setContext(context)
      context.reset()
      configurator.doConfigure(config)
    } else {
      
      // suppress error stream
      System.setErr(new PrintStream(
        new OutputStream() {
          override def write(b: Int): Unit = { }
        }
      ))

      // send all j.u.l logs to slf4j
      SLF4JBridgeHandler.removeHandlersForRootLogger()
      SLF4JBridgeHandler.install()

    }

  }

  /**
    * Returns the console REPL.
    *
    * @param ctx the evaluation context
    */
  private [gwen] def createRepl(ctx: T): GwenREPL[T] = new GwenREPL[T](engine, ctx)

  private def printBanner(intro: String, args: Array[String]): Unit = {
    println(
      """|
         |   __ ___      _____ _ __     _    
         |  / _` \ \ /\ / / _ \ '_ \   { \," 
         | | (_| |\ V  V /  __/ | | | {_`/   
         |  \__, | \_/\_/ \___|_| |_|   `    
         |  |___/                            
         |
         |""".stripMargin + intro + engine.implName + " v" + engine.implVersion + engine.noticeMsg.map(msg => s"${System.lineSeparator}$msg").getOrElse("") + """|
         |gweninterpreter.org
         |""".stripMargin
      )

  }

}
