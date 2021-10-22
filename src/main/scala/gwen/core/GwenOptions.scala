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

package gwen.core

import gwen.core.Deprecation
import gwen.core.FileIO
import gwen.core.node.gherkin.Tag
import gwen.core.node.gherkin.TagFilter
import gwen.core.report.ReportFormat
import gwen.core.state.StateLevel

import scopt.OptionParser

import scala.util.chaining._

import java.io.File

/**
  * Captures gwen command line options.
  *
  * @param batch true to run in batch mode, false for interactive REPL (default is false)
  * @param parallel true to run features or scenarios in parallel depending on state level (default is false)
  * @param parallelFeatures true to run features in parallel regardless of state level (default is false)
  * @param reportDir optional directory to generate evaluation report into
  * @param settingsFiles list of settings files to load
  * @param tags list of tags to include and exclude, list of (tag, true=include|false=exclude)
  * @param dryRun true to not evaluate steps on engine (and validate for correctness only)
  * @param dataFile optional CSV file for data driven testing (must include column headers in 1st line)
  * @param metas optional list of meta file and/or directories
  * @param features optional list of feature file and/or directories
  * @param init true to initialise a working directory
  * @param initDir working diretory to initialise
  *
  * @author Branko Juric
  */
case class GwenOptions(
    batch: Boolean = GwenOptions.Defaults.batch,
    parallel: Boolean = GwenOptions.Defaults.parallel,
    parallelFeatures: Boolean = GwenOptions.Defaults.parallelFeatures,
    reportDir: Option[File] = GwenOptions.Defaults.report,
    reportFormats: List[ReportFormat] = GwenOptions.Defaults.format,
    settingsFiles: List[File] = GwenOptions.Defaults.conf,
    tags: List[(Tag, Boolean)] = GwenOptions.Defaults.tags,
    dryRun: Boolean = GwenOptions.Defaults.dryRun,
    dataFile: Option[File] = GwenOptions.Defaults.inputData,
    metas: List[File] = GwenOptions.Defaults.meta,
    features: List[File] = GwenOptions.Defaults.features,
    args: Option[Array[String]] = None,
    init: Boolean = false,
    initDir: File = GwenOptions.Defaults.initDir) extends GwenInfo {

  def isParallelScenarios(stateLevel: StateLevel) = stateLevel == StateLevel.scenario && parallel && !parallelFeatures

  def tagFilter = new TagFilter(tags)

  /**
    * Gets the command string used to invoke gwen.
    */
  def commandString: String = args match {
    case (Some(params)) => s"$implName.${if(sys.props("os.name").startsWith("Windows")) "bat" else "sh"} ${params.mkString(" ")}"
    case _ => ""
  }

}

object GwenOptions {

  object Defaults {
    val batch = CLISettings.`gwen.cli.options.batch`
    val format = CLISettings.`gwen.cli.options.format`
    val conf = CLISettings.`gwen.cli.options.conf`
    val dryRun = CLISettings.`gwen.cli.options.dryRun`
    val features = CLISettings.`gwen.cli.options.features`
    val initDir = CLISettings.`gwen.cli.options.initDir`
    val inputData = CLISettings.`gwen.cli.options.inputData`
    val parallel = CLISettings.`gwen.cli.options.parallel`
    val parallelFeatures = CLISettings.`gwen.cli.options.parallelFeatures`
    val meta = CLISettings.`gwen.cli.options.meta`
    val report = CLISettings.`gwen.cli.options.report`
    val tags = CLISettings.`gwen.cli.options.tags`
  }

  /**
    * Creates a new options object from the given command line arguments.
    *
    * @param args the command line arguments
    * @throws gwen.Errors.InvocationException if the given arguments fail to parse
    */
  def apply(args: Array[String]): GwenOptions = {

    val parser = new OptionParser[GwenOptions]("gwen") {

      version("version") text "Print Gwen version"

      help("help") text "Print CLI usage"

      opt[Unit]("parallel") action {
        (_, c) => {
          c.copy(parallel = true, batch = true)
        }
      } text """|Execute features or scenarios in parallel
                |- depending on gwen.state.level (default is feature)""".stripMargin

      opt[Unit]("parallel-features") action {
        (_, c) => {
          c.copy(parallelFeatures = true, batch = true)
        }
      } text "Execute features in parallel (unconditionally)"

      opt[Unit]('b', "batch") action {
        (_, c) => c.copy(batch = true)
      } text "Exit when execution completes (omit to open REPL)"

      opt[Unit]('n', "dry-run") action {
        (_, c) => c.copy(dryRun = true)
      } text "Check all syntax and bindings and report errors"

      opt[String]('c', "conf") action {
        (cs, c) =>
          c.copy(settingsFiles = c.settingsFiles ++ cs.split(",").toList.map(new File(_)))
      } validate { cs =>
        ((cs.split(",") flatMap { f =>
          val file = new File(f)
          if (!FileIO.hasFileExtension("conf", file) && !FileIO.hasFileExtension("json", file) && !FileIO.hasFileExtension("properties", file)) {
            Some("-c|--conf option only accepts *.conf, *.json or *.properties files")
          }
          else if (file.exists()) None
          else Some(s"Provided settings file not found: $f")
        }) collectFirst {
          case error => failure(error)
        }).getOrElse(success)
      } valueName "files" text "Settings files: conf, json or properties (comma separated)"

      opt[String]('p', "properties") action {
        (ps, c) =>
          Deprecation.warn("CLI option", "-p|--properties", "-c|--conf")
          c.copy(settingsFiles = c.settingsFiles ++ ps.split(",").toList.map(new File(_)))
      } validate { ps =>
        ((ps.split(",") flatMap { f =>
          val file = new File(f)
          if (!FileIO.hasFileExtension("properties", file)) Some("-p|--properties option only accepts *.properties files")
          else if (file.exists()) None
          else Some(s"Provided properties file not found: $f")
        }) collectFirst {
          case error => failure(error)
        }).getOrElse(success)
      } valueName "files" text "Properties files (deprecated, use -c|--conf instead)"

      opt[File]('r', "report") action {
        (f, c) => c.copy(reportDir = Some(f))
      } valueName "dir" text "Directory to output generated report(s) to"

      opt[String]('f', "formats") action {
        (fs, c) =>
          c.copy(reportFormats = fs.split(",").toList.map(f => ReportFormat.valueOf(f)))
      } valueName "reports" text s"""|Report formats to include in output (comma separated)
                                     |- ${ReportFormat.values.filter(_.isCliOption).mkString(",")} (default is ${ReportFormat.html})""".stripMargin

      opt[String]('t', "tags") action {
        (ts, c) =>
          c.copy(tags = ts.split(",").toList.map(t => (Tag(t), t.startsWith("@"))))
      } validate { ts =>
        ((ts.split(",") flatMap { t =>
          if (t.matches("""^(~?@\w+,?)+$""")) None
          else Some(s"Invalid tag $t: tags must start with @ or ~@")
        }) collectFirst {
          case error => failure(error)
        }).getOrElse(success)
      } valueName "filter" text "Tags to @include or ~@exclude (comma separated)"

      opt[File]('i', "input-data") action {
        (d, c) => c.copy(dataFile = Some(d))
      } validate { d =>
        if (!d.exists) failure(s"Specified data file not found: $d")
        else success
      } valueName "file" text "Input data feed (CSV file with column headers)"

      opt[String]('m', "meta") action {
        (ms, c) =>
          c.copy(metas = ms.split(",").toList.map(new File(_)))
      } validate { ms =>
        ((ms.split(",") flatMap { m =>
          if (new File(m).exists()) None
          else Some(s"Specified meta entry not found: $m")
        }) collectFirst {
          case error => failure(error)
        }).getOrElse(success)
      } valueName "files|dirs" text "Meta files or directories to load (comma separated)"

      arg[File]("files|dirs").unbounded().optional().action {
        (f, c) =>
          c.copy(features = c.features :+ f)
      } validate {
        f => if (f.exists) success else failure(s"Specified feature(s) not found: $f")
      } text "Feature files or directories to execute (space separated)"

      cmd("init").action { 
        (_, c) => 
          c.copy(init = true) 
      } children {
          arg[File]("dir").optional().action {
            (d, c) =>
              c.copy(initDir = d)
          } text s"Project directory to initialise (default is ${GwenOptions.Defaults.initDir.getPath()})"
      } 

    }

    (parser.parse(args, GwenOptions()).map { options =>
      new GwenOptions(
        options.batch,
        options.parallel,
        options.parallelFeatures,
        options.reportDir,
        if (options.reportFormats.nonEmpty) options.reportFormats else { if (options.reportDir.nonEmpty) GwenOptions.Defaults.format else Nil },
        options.settingsFiles,
        options.tags,
        options.dryRun,
        options.dataFile,
        FileIO.appendFile(options.metas, Settings.UserMeta),
        options.features,
        Some(args),
        options.init,
        options.initDir)
      }tap { options =>
        options foreach { opt =>
          if (opt.batch && opt.features.isEmpty) {
            Errors.invocationError("No feature files or directories provided")
          }
          if (opt.reportFormats.nonEmpty && opt.reportDir.isEmpty) {
            val reportables = opt.reportFormats.filter(_.isFileSystemReport)
            Errors.invocationError(s"Required -r|--report option not provided for -f|--formats option${if (reportables.size > 1) "s" else ""}: ${reportables.mkString(",")}")
          }
        }
      }).getOrElse(Errors.invocationError("Gwen invocaation failed (see log for details)"))

  }

}
