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

package gwen.core.eval

import gwen.core._
import gwen.core.Settings
import gwen.core.node.FeatureStream
import gwen.core.node.FeatureUnit
import gwen.core.node.Root
import gwen.core.node.gherkin.SpecType
import gwen.core.report.console.ConsoleReporter
import gwen.core.report.ReportGenerator
import gwen.core.result.SpecResult
import gwen.core.result.ResultsSummary
import gwen.core.state.EnvState
import gwen.core.state.StateLevel
import gwen.core.status._

import com.typesafe.scalalogging.LazyLogging

import scala.concurrent._
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import scala.util.chaining._

import java.io.File
import java.nio.file.Files
import java.util.concurrent.atomic.AtomicInteger

/**
  * Launches a gwen engine.
  *
  * @param engine the engine to launch
  */
class GwenLauncher[T <: EvalContext](engine: EvalEngine[T]) extends LazyLogging {

  /**
    * Initialises a new Gwen project.
    *
    * @param dir the directory to initialise
    */
  def initProject(dir: File): Unit = {
    if (dir.exists) {
      if (dir.isFile) {
        Errors.initProjectError(s"Cannot initialise ${dir} because it is a file that already exists")
      } else if (Files.newDirectoryStream(dir.toPath).iterator.hasNext) {
        if (dir.isSame(new File("."))) {
          Errors.initProjectError("Cannot initialise current directory because it is not empty")
        } else {
          Errors.initProjectError(s"Cannot initialise ${dir} directory because it exists and is not empty")
        }              
      }
    }
    logger.info(("""|
                    |   _
                    |  { \," Initialising project directory: """ + dir.getPath + """
                    | {_`/   
                    |    `   """).stripMargin)
    dir.mkdirs()
  }

  /**
    * Interprets a features unit (feaature + meta).
    *
    * @param unit the feature unit
    * @param ctx the evaluation context
    * @return the evaluated result
    */
  private def interpretUnit(unit: FeatureUnit, ctx: T): Option[SpecResult] = {
    engine.evaluateUnit(unit, ctx)
  }

  /**
    * Runs the interpreter with the given options.
    *
    * @param options the command line options
    * @param ctxOpt optional evaluation context (None to have Gwen create an env context for each feature unit,
    *               Some(ctx) to reuse a context for all, default is None)
    */
  def run(options: GwenOptions, ctxOpt: Option[T] = None): EvalStatus = {
    if (options.args.isDefined) {
      logger.info(options.commandString)
    }
    val startNanos = System.nanoTime
    try {
      if (options.init) {
        initProject(options.initDir)
        logger.info(s"Project directory initialised")
        Passed(System.nanoTime - startNanos)
      } else {
        val metaFiles = options.metas.filter(_.exists).flatMap { m => 
          if (m.isFile) List(m) else FileIO.recursiveScan(m, "meta") 
        }
        val featureStream = new FeatureStream(metaFiles, options.tagFilter)
        featureStream.readAll(options.features, options.dataFile) match {
          case stream @ _ #:: _ =>
            executeFeatureUnits(options, stream.flatten, ctxOpt)
          case _ =>
            (EvalStatus {
              if (metaFiles.nonEmpty) {
                val unit = FeatureUnit(Root, metaFiles.head, metaFiles.tail, None, options.tagFilter)
                ctxOpt flatMap { ctx =>
                  interpretUnit(unit, ctx) map { result =>
                    result.evalStatus
                  }
                } toList
              } else {
                Nil
              }
            }) tap { _ =>
              if (options.features.nonEmpty) {
                logger.warn("No features found in specified files and/or directories!")
              }
            }
        }
      }
    } catch {
      case e: Throwable =>
        val failed = Failed(System.nanoTime - startNanos, e)
        if (options.batch) {
          logger.error(failed.message, e)
          failed
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
    * @param ctxOpt optional evaluation context (None to have Gwen create an env context for each feature unit,
    *               Some(ctx) to reuse a context for all, default is None)
    */
  private def executeFeatureUnits(options: GwenOptions, featureStream: LazyList[FeatureUnit], ctxOpt: Option[T]): EvalStatus = {
    val start = System.nanoTime
    val consoleReporter = Option(options.verbose).filter(!_).map(_ => new ConsoleReporter(options))
    consoleReporter.foreach(engine.addListener)
    try {
      val reportGenerators = ReportGenerator.generatorsFor(options, engine)
      reportGenerators.foreach(_.init(engine))
      Try {
        if (options.parallel) {
          executeFeatureUnitsParallel(options, featureStream, ctxOpt, reportGenerators)
        } else {
          executeFeatureUnitsSequential(options, featureStream, ctxOpt, reportGenerators)
        }
      } match {
        case Success(s) =>
          val reports = reportGenerators flatMap { reportGenerator => 
            reportGenerator.close(engine, s.evalStatus) flatMap { report =>
              Some((reportGenerator.format, report))
            }
          }
          consoleReporter foreach { _.printSummary(s.withReports(reports)) }
          printSummaryStatus(options, s)
          s.evalStatus
        case Failure(f) =>
          reportGenerators.foreach(_.close(engine, Failed(System.nanoTime - start, f)))
          throw f
      }
    } finally {
      consoleReporter.foreach(engine.removeListener)
    }
  }

  private def executeFeatureUnitsParallel(options: GwenOptions, featureStream: LazyList[FeatureUnit], ctxOpt: Option[T], reportGenerators: List[ReportGenerator]): ResultsSummary = {
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
        evaluateUnit(options, ctxOpt, unit) { result =>
          result.map(bindReportFiles(reportGenerators, unit, _)) tap { _ => result.foreach(r => logSpecStatus(options, r)) }
        }
      }
    }
    val results = Await.result(
      Future.sequence(featureUnits.force),
      Duration.Inf
    )
    results.flatten.sortBy(_.finished).foldLeft(ResultsSummary())(_ + _) tap { summary =>
      reportGenerators foreach {
        _.reportSummary(summary)
      }
    }
  }

  private def executeFeatureUnitsSequential(options: GwenOptions, featureStream: LazyList[FeatureUnit], ctxOpt: Option[T], reportGenerators: List[ReportGenerator]): ResultsSummary = {
    val exitOnFail = GwenSettings.`gwen.feature.failfast.exit` && !options.dryRun
    featureStream.foldLeft(ResultsSummary()) { (summary, unit) =>
      if (exitOnFail && summary.evalStatus.isFailed) {
        summary
      } else {
        evaluateUnit(options, ctxOpt, unit) { result =>
          (result match {
            case None => summary
            case _ =>
              (summary + result.map(bindReportFiles(reportGenerators, unit, _)).get) tap { accSummary =>
                reportGenerators foreach {
                  _.reportSummary(accSummary)
                }
              }
          }) tap { _ => result.foreach(r => logSpecStatus(options, r)) }
        }
      }
    }
  }

  private def evaluateUnit[U](options: GwenOptions, ctxOpt: Option[T], unit: FeatureUnit)(f: (Option[SpecResult] => U)): U = {
    Settings.clearLocal()
    val ctx = ctxOpt getOrElse { 
      engine.init(options, EnvState())
    }
    if (ctxOpt.nonEmpty) { ctx.reset(StateLevel.feature) }
    unit.dataRecord foreach { record =>
      record.data foreach { case (name, value) =>
        ctx.topScope.set(name, value)
      }
    }
    val result = try {
      interpretUnit(unit, ctx)
    } finally {
      if (ctxOpt.isEmpty) { 
        ctx.close()
        logger.info("Evaluation context closed")
      } 
    }
    f(result map { res =>
      new SpecResult(res.spec, res.reports, res.videos, (res.metaResults), res.started, res.finished)
    })
  }

  private def flattenResults(results: List[SpecResult]): List[SpecResult] = {
    def flattenResults(results: List[SpecResult]): List[SpecResult] = results.flatMap {
      r => r::flattenResults(r.metaResults)
    }
    flattenResults(results).sortBy(_.finished)
  }

  private def bindReportFiles(reportGenerators: List[ReportGenerator], unit: FeatureUnit, result: SpecResult): SpecResult = {
    val reportFiles = reportGenerators.map { generator =>
      (generator.format, generator.reportDetail(unit, result))
    }.filter(_._2.nonEmpty).toMap
    if (reportFiles.nonEmpty)
      new SpecResult(result.spec, Some(reportFiles), result.videos, result.metaResults, result.started, result.finished)
    else
      result
  }

  private def logSpecStatus(options: GwenOptions, result: SpecResult): Unit = {
    logger.info("")
    StatusLogger.log(options, logger, result.evalStatus, result.toString)
    logger.info("")
  }

  private def printSummaryStatus(options: GwenOptions, summary: ResultsSummary): Unit = {
    logger.info(s"\n${summary.statsString}")
    logger.info("")
    StatusLogger.log(options, logger, summary.evalStatus, summary.statusString)
    logger.info("")
  }
  
}
