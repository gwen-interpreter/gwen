/*
 * Copyright 2014-2018 Branko Juric, Brady Wood
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

import scala.Option.option2Iterable
import com.typesafe.scalalogging.LazyLogging
import gwen.Predefs.Kestrel
import gwen.Predefs.FileIO
import gwen.dsl.EvalStatus
import gwen.dsl.Failed
import gwen.report.ReportGenerator
import gwen.GwenSettings
import gwen.dsl.StatusKeyword
import java.util.concurrent.atomic.AtomicInteger

import gwen.Settings

import scala.concurrent._
import scala.concurrent.duration.Duration
import gwen.dsl.StateLevel
import scala.concurrent.ExecutionContext

/**
  * Launches the gwen interpreter.
  * 
  * @param interpreter the interpreter to launch
  */
class GwenLauncher[T <: EnvContext](interpreter: GwenInterpreter[T]) extends LazyLogging {
  
  /**
    * Runs the interpreter with the given options.
    * 
    * @param options the command line options
    * @param optEnv optional environment context (None to have Gwen create an env context for each feature unit, 
    *               Some(env) to reuse an environment context for all, default is None)
    */
  def run(options: GwenOptions, optEnv: Option[T] = None): EvalStatus = {
    if (options.args.isDefined) {
      logger.info(options.commandString(interpreter))
    }
    val startNanos = System.nanoTime
    try {
      val metaFiles = options.metas.flatMap(m => if (m.isFile) List(m) else FileIO.recursiveScan(m, "meta"))
      val featureStream = new FeatureStream(metaFiles)
      featureStream.readAll(options.features, options.dataFile) match {
        case featureStream @ _ #:: _ =>
          val summary = executeFeatureUnits(options, featureStream.flatten, optEnv)
          printSummaryStatus(summary)
          summary.evalStatus
        case _ =>
          EvalStatus { 
            optEnv.toList flatMap { env =>
             interpreter.loadMeta(metaFiles, Nil, env).map(_.spec.evalStatus)
            }
          } tap { _ =>
            if (options.features.nonEmpty) {
              logger.warn("No features found in specified files and/or directories!")
            }
          }
      }
    } catch {
      case e: Throwable =>
        if (options.batch) {
          logger.error(e.getMessage, e)
          Failed(System.nanoTime - startNanos, e)
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
    val reportGenerators = ReportGenerator.generatorsFor(options)
    if (options.parallel) {
      executeFeatureUnitsParallel(options, featureStream, envOpt, reportGenerators)
    } else {
      executeFeatureUnitsSequential(options, featureStream, envOpt, reportGenerators)
    }
  }

  private def executeFeatureUnitsParallel(options: GwenOptions, featureStream: Stream[FeatureUnit], envOpt: Option[T], reportGenerators: List[ReportGenerator]) = {
    val counter = new AtomicInteger(0)
    val started = new ThreadLocal[Boolean]()
    started.set(false)
    val executor = ParallelExecutors.featureInstance
    implicit val ec = ExecutionContext.fromExecutorService(executor)
    val featureUnits = featureStream.map { unit =>
      Future {
        if (!started.get) {
          started.set(true)
          GwenSettings.`gwen.rampup.interval.seconds` foreach { interval =>
            if (interval > 0) {
              val partition = counter.incrementAndGet()
              val period = (partition - 1) * interval
              logger.info(s"Ramp up period for parallel partition $partition is $period second${if (period == 1) "" else "s"}")
              Thread.sleep(period * 1000)
            }
          }
        }
        evaluateUnit(options, envOpt, unit) { result =>
          result.map(bindReportFiles(reportGenerators, unit, _)) tap { _ => result.foreach(logFeatureStatus) }
        }
      }
    }
    val results = Await.result(
      Future.sequence(featureUnits.force),
      Duration.Inf
    )
    results.flatten.sortBy(_.finished).foldLeft(FeatureSummary())(_ + _) tap { summary =>
      reportGenerators foreach {
        _.reportSummary(interpreter, summary)
      }
    }
  }

  private def executeFeatureUnitsSequential(options: GwenOptions, featureStream: Stream[FeatureUnit], envOpt: Option[T], reportGenerators: List[ReportGenerator]) = {
    val exitOnFail = GwenSettings.`gwen.feature.failfast.exit` && !options.dryRun
    featureStream.foldLeft(FeatureSummary()) { (summary, unit) =>
      if (exitOnFail && summary.evalStatus.status == StatusKeyword.Failed) {
        summary
      } else {
        evaluateUnit(options, envOpt, unit) { result =>
          (result match {
            case None => summary
            case _ =>
              (summary + result.map(bindReportFiles(reportGenerators, unit, _)).get) tap { accSummary =>
                reportGenerators foreach {
                  _.reportSummary(interpreter, accSummary)
                }
              }
          }) tap { _ => result.foreach(logFeatureStatus) }
        }
      }
    }
  }

  private def evaluateUnit[U](options: GwenOptions, envOpt: Option[T], unit: FeatureUnit)(f: (Option[FeatureResult] => U)): U = {
    logger.info(("""|       
                    |  _    
                    | { \," Evaluating feature..
                    |{_`/   """ + unit.featureFile.toString + """
                    |   `   """).stripMargin)
    Settings.clearLocal()
    val env = envOpt.getOrElse(interpreter.initialise(options))
    try {
      if (envOpt.nonEmpty) { env.reset(StateLevel.feature) }
      val targetUnit = FeatureUnit(unit.featureFile, unit.metaFiles, unit.dataRecord)
      unit.dataRecord foreach { record =>
        record.data foreach { case (name, value) =>
          env.topScope.set(name, value)
        }
      }
      f(interpreter.interpretFeature(targetUnit, options.tags, env) map { res =>
        new FeatureResult(res.spec, res.reports, flattenResults(res.metaResults), res.started, res.finished)
      })
    } finally {
      if (envOpt.isEmpty) { 
        env.close() 
        logger.info("Environment context closed")
      }
    }
  }
  
  private def flattenResults(results: List[FeatureResult]): List[FeatureResult] = {
    def flattenResults(results: List[FeatureResult]): List[FeatureResult] = results.flatMap { 
      r => r::flattenResults(r.metaResults)
    }
    flattenResults(results).sortBy(_.finished)
  }
    
  private def bindReportFiles(reportGenerators: List[ReportGenerator], unit: FeatureUnit, result: FeatureResult): FeatureResult = {
    val reportFiles = reportGenerators.map { generator => 
      (generator.reportFormat, generator.reportDetail(interpreter, unit, result)) 
    }.filter(_._2.nonEmpty).toMap
    if (reportFiles.nonEmpty) 
      new FeatureResult(result.spec, Some(reportFiles), result.metaResults, result.started, result.finished)
    else 
      result
  }
  
  private def logFeatureStatus(result: FeatureResult) {
    logger.info("")
    logger.info(result.toString)
    logger.info("")
  }
      
  private def printSummaryStatus(summary: FeatureSummary) {
    println
    println(summary.toString)
    println
  }
}