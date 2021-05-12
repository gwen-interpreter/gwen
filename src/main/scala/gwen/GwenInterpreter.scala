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
import gwen.core.engine.EvalContext
import gwen.core.engine.EvalEngine

import gwen.core.model.EvalStatus
import gwen.core.model.FeatureUnit
import gwen.core.model.Loaded
import gwen.core.model.Root
import gwen.core.model.SpecResult
import gwen.core.model.SpecType
import gwen.core.model.gherkin.Step
import gwen.core.model.state.EnvState

import scala.util.Try

import com.typesafe.scalalogging.LazyLogging
import org.apache.log4j.PropertyConfigurator

import java.net.URL

/**
  * Default Gwen interpreter application.
  */
object DefaultGwenInterpreter extends GwenInterpreter(EvalEngine.DefaultInstance)

/**
  * Main Gwen application superclass.
  * 
  * @param engine the evaluation engine
  */
class GwenInterpreter[T <: EvalContext](engine: EvalEngine[T]) extends App with GwenInfo with LazyLogging {

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
    * Initialises the interpreter
    * 
    * @param options the command line options
    */
  def init(options: GwenOptions): T = {
    Settings.getOpt("log4j.configuration").orElse(Settings.getOpt("log4j.configurationFile")).foreach { config => 
      if (config.toLowerCase.trim startsWith "file:") {
        PropertyConfigurator.configure(new URL(config));
      } else {
        PropertyConfigurator.configure(config); 
      }
    }
    engine.init(options, EnvState()) tap { env =>
      logger.info(s"Evaluation context initialised")
    }
  }

  def lifecycle = engine

  /**
    * Interprets a single step expression.
    *
    * @param stepExpression the input step expression
    * @param ctx the evaluation context
    * @return the evaluated step (or an exception if a runtime error occurs)
    */
  def interpretStep(stepExpression: String, ctx: T): Try[Step] = {
    engine.parseStep(stepExpression).map { step =>
      engine.evaluateStep(Root, step, ctx)
    }
  }

  /**
    * Interprets a features unit (feaature + meta).
    *
    * @param unit the feature unit
    * @param ctx the evaluation context
    * @return the evaluated result
    */
  def interpretUnit(unit: FeatureUnit, ctx: T): Option[SpecResult] = {
    logger.info(("""|        
                    |   _    
                    |  { \," Evaluating """ + SpecType.ofFile(unit.featureFile).toString.toLowerCase + """..
                    | {_`/   """ + unit.featureFile.toString + """
                    |    `   """).stripMargin)
    engine.evaluateUnit(unit, ctx)
  }
  
  /** 
    * Runs the interpreter with the given options
    * 
    * @param options the command line options
    * @param launcher implicit Gwen launcher
    * @return 0 if successful; 1 otherwise
    */  
  private [gwen] def run(options: GwenOptions, launcher: GwenLauncher[T] = new GwenLauncher(this)): Int = {
    val ctxOpt = if (options.batch) None else Some(init(options))
    try {
      val evalStatus = launcher.run(options, ctxOpt)
      ctxOpt foreach { ctx =>
        if (EvalStatus.isEvaluated(evalStatus.status) || evalStatus == Loaded) {
          printBanner("")
        }
        createRepl(ctx).run()
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
  private [gwen] def createRepl(ctx: T): GwenREPL[T] = new GwenREPL[T](this, ctx)
  
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
