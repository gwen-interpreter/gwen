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

import java.io.File
import scala.Option.option2Iterable
import com.typesafe.scalalogging.slf4j.LazyLogging
import gwen.ConsoleWriter
import gwen.Predefs.Kestrel
import gwen.UserOverrides
import gwen.dsl.EvalStatus
import gwen.dsl.Failed
import gwen.report.ReportGenerator
import gwen.report.html.HtmlReportGenerator
import gwen.GwenInfo
import gwen.dsl.FeatureSpec

/**
  * Executes user provided options on the given interpreter.
  * 
  * @param interpreter the gwen interpreter to execute on
  */
class GwenExecutor[T <: EnvContext](interpreter: GwenInterpreter[T]) extends LazyLogging with ConsoleWriter {
  
  /**
    * Executes the given options.
    * 
    * @param options the command line options
    * @param optEnv optional environment context (None to have Gwen create an env context for each feature unit, 
    *               Some(env) to reuse an environment context for all, default is None)
    */
  def execute(options: GwenOptions, optEnv: Option[T] = None): EvalStatus = {
    if (options.args.isDefined) {
    	logger.info(options.commandString(interpreter))
    }
    val start = System.nanoTime
    try {
      FeatureStream.readAll(options.paths) match {
        case featureStream @ _ #:: _ =>
          val summary = executeFeatureUnits(options, featureStream.flatten, optEnv)
          EvalStatus(summary.featureResults.map(_.evalStatus)) tap { status =>
            printStatus(summary, status)
          }
        case _ =>
          EvalStatus { 
            optEnv.toList flatMap { env =>
             interpreter.loadMeta(options.metaFiles, Nil, env).map(_.evalStatus)
            }
          } tap { status =>
            if (!options.paths.isEmpty) {
              logger.info("No features found in specified files and/or directories!")
            }
          }
      }
    } catch {
      case e: Throwable =>
        if (options.batch) {
          logger.error(e.getMessage, e)
          Failed(System.nanoTime - start, e)
        } else {
          throw e
        }
    }
  }
  
  /**
    * Executes all feature units in the given stream.
    * 
    * @param options the command line options
    * @param featureStream the feature stream to execute
    * @param envOpt optional environment context (reused across all feature units if provided, 
    *               otherwise a new context is created for each unit)
    */
  private def executeFeatureUnits(options: GwenOptions, featureStream: Stream[FeatureUnit], envOpt: Option[T]): FeatureSummary = {
    val reportGenerator = options.reportDir map { reportDir =>
      new HtmlReportGenerator(options)
    }
    if (options.parallel) {
      val results = featureStream.par.flatMap { unit =>
        evaluateUnit(options, envOpt, reportGenerator, unit) { specs => 
          specs match { 
            case Nil => None
            case _ => Some(toFeatureResult(reportGenerator, specs))
          }
        }
      }
      results.foldLeft(FeatureSummary()) (_+_) tap { summary => 
        reportGenerator foreach { _.reportSummary(interpreter, summary) }
      }
    } else {
      featureStream.foldLeft(FeatureSummary()) { (summary, unit) =>
        evaluateUnit(options, envOpt, reportGenerator, unit) { specs =>
          specs match { 
            case Nil => summary
            case specs => 
              val result = toFeatureResult(reportGenerator, specs)
              summary + result tap { accSummary =>
                if (!options.parallel) {
                  reportGenerator foreach { _.reportSummary(interpreter, accSummary) }
                }
              }
          }
        }
      }
    }
  }
  
  private def evaluateUnit[U](options: GwenOptions, envOpt: Option[T], reportGenerator: Option[ReportGenerator], unit: FeatureUnit)(f: (List[FeatureSpec] => U)): U = {
    logger.info(("""|       
                    |  _    
                    | { \," Evaluating feature..
                    |{_`/   """ + unit.featureFile.toString + """
                    |   `   """).stripMargin)
    val env = envOpt.getOrElse(interpreter.initialise(options))
    try {
      if (envOpt.isDefined) { interpreter.reset(env) }
      f(interpreter.interpretFeature(unit.featureFile, UserOverrides.mergeMetaFiles(unit.metaFiles, options.metaFiles), options.tags, env))
    } finally {
      if (!envOpt.isDefined) { interpreter.close(env) }
    }
  }
    
  private def toFeatureResult(reportGenerator: Option[ReportGenerator], specs: List[FeatureSpec]): FeatureResult = 
    reportGenerator.map(_.reportDetail(interpreter, specs)).getOrElse(new FeatureResult(specs.head, specs.map(new FeatureResult(_, Nil, None)), None)) tap { result =>
      val status = result.spec.evalStatus
      logger.info("");
      logger.info(s"${status} ${status.emoticon} ${result.timestamp}", status)
      logger.info("");
    }
  
  private def printStatus(summary: FeatureSummary, status: EvalStatus) {
    println
    println(summary.toString)
    println
    println(s"${status} ${status.emoticon} ${summary.timestamp}")
    println
  }
}