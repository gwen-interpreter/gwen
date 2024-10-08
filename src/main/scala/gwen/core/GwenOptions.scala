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

import gwen.core._
import gwen.core.init.InitOption
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
  * @param verbose true for verbose log output, false for pretty log output
  * @param debug true to enable debug mode, false otherwise
  * @param reportDir optional directory to generate evaluation report into
  * @param settingsFiles list of settings files to load
  * @param tags list of tags to include and exclude, list of (tag, true=include|false=exclude)
  * @param dryRun true to not evaluate steps on engine (and validate for correctness only)
  * @param dataFile optional CSV or JSON file for data driven testing (CSV must include column headers in 1st line)
  * @param metas optional list of meta file and/or directories
  * @param features optional list of feature file and/or directories
  * @param init true to initialise a working directory
  * @param docker true to init docker files, false to not
  * @param jenkins true to init jenkins files, false to not
  * @param initDir working diretory to initialise
  * @param pretty option boolean to pretty format given feature and meta files
  * @param formatFiles list of files/directories to format
  *
  * @author Branko Juric
  */
case class GwenOptions(
    batch: Boolean = GwenOptions.Defaults.batch,
    parallel: Boolean = GwenOptions.Defaults.parallel,
    verbose: Boolean = GwenOptions.Defaults.verbose,
    debug: Boolean = GwenOptions.Defaults.debug,
    reportDir: Option[File] = GwenOptions.Defaults.report,
    reportFormats: List[ReportFormat] = GwenOptions.Defaults.format,
    settingsFiles: List[File] = Nil,
    tags: List[(Tag, Boolean)] = GwenOptions.Defaults.tags,
    dryRun: Boolean = GwenOptions.Defaults.dryRun,
    dataFile: Option[File] = GwenOptions.Defaults.inputData,
    metas: List[File] = GwenOptions.Defaults.meta,
    features: List[File] = GwenOptions.Defaults.features,
    args: Option[Array[String]] = None,
    init: Boolean = false,
    initOptions: List[InitOption] = Nil,
    initDir: File = GwenOptions.Defaults.initDir,
    pretty: Boolean = GwenOptions.Defaults.pretty,
    formatFiles: List[File] = Nil) {

  def interpolate(source: String): String = {
    val options = GwenOptions.this
    val interpolator = new Interpolator(name => {
        if (name.startsWith("<gwen.options.")) {
          val n = name.substring("gwen.options.".length + 1, name.length - 1)
          (n match {
            case "batch" => Some(options.batch)
            case "parallel" => Some(options.parallel)
            case "verbose" => Some(options.verbose)
            case "debug" => Some(options.debug)
            case "reportDir" => Some(options.reportDir.getOrElse(""))
            case "reportFormats" => Some(reportFormats.mkString(","))
            case "settingsFiles" => Some(options.settingsFiles.mkString(","))
            case "tags" => Some(options.tags.map((t, include) => s"${if (include) "" else "~"}$t").mkString(","))
            case "dryRun" => Some(options.dryRun)
            case "dataFile" => Some(options.dataFile.getOrElse(""))
            case "metas" => Some(FileIO.appendFile(options.metas, Settings.UserMeta).mkString(","))
            case "features" => Some(options.features.mkString(" "))
            case "args" => Some(options.args.mkString(" "))
            case "init" => Some(options.init)
            case "initOptions" => Some(options.initOptions.map(opt => s"--$opt").sorted.mkString(" "))
            case "initDir" => Option(initDir)
            case "pretty" => Some(options.pretty)
            case "formatFiles" => Some(options.formatFiles.mkString(" "))
            case _ => None
          }).map(_.toString)
        } else {
          Option(s"$$!$name")
        }
      })
      val result = interpolator.interpolate(source)
      interpolator.restoreUnresolved(result)
  }

  def parallelScenarios(stateLevel: StateLevel) = parallel && stateLevel == StateLevel.scenario

  def tagFilter = new TagFilter(tags)

  /**
    * Gets the command string used to invoke gwen.
    */
  def commandString: String = args match {
    case (Some(params)) => s"gwen ${params.mkString(" ")}"
    case _ => ""
  }

}

object GwenOptions {

  object SettingsKey {
    val dataFile = "gwen.options.dataFile"
  }

  object Defaults {
    val batch = CLISettings.`gwen.cli.options.batch`
    val format = CLISettings.`gwen.cli.options.format` match {
      case Nil => List(ReportFormat.html)
      case fs => fs
    }
    val conf = Nil
    val dryRun = CLISettings.`gwen.cli.options.dryRun`
    val features = CLISettings.`gwen.cli.options.features`
    val inputData = CLISettings.`gwen.cli.options.inputData`
    val parallel = CLISettings.`gwen.cli.options.parallel`
    val meta = CLISettings.`gwen.cli.options.meta`
    val report = CLISettings.`gwen.cli.options.report`
    val tags = CLISettings.`gwen.cli.options.tags`
    val verbose = CLISettings.`gwen.cli.options.verbose`
    val debug = false
    val docker = false
    val jenkins = false
    val initDir = new File("gwen")
    val pretty = false
  }

  /**
    * Creates a new options object from the given command line arguments.
    *
    * @param args the command line arguments
    */
  def apply(args: Array[String]): GwenOptions = {

    val parser = new OptionParser[GwenOptions]("gwen") {

      version("version") text "Print Gwen version"

      help("help") text "Print CLI usage"

      opt[Unit]("parallel") action {
        (_, c) => {
          c.copy(parallel = true)
        }
      } text """|Execute features or scenarios in parallel
                |- features if gwen.state.level = feature (default)
                |- scenarios if gwen.state.level = scenario""".stripMargin

      opt[Unit]('b', "batch") action {
        (_, c) => c.copy(batch = true)
      } text "Exit when execution completes (omit to open REPL)"

      opt[Unit]('v', "verbose") action {
        (_, c) => c.copy(verbose = true)
      } text "Enable verbose log output"

      opt[Unit]('n', "dry-run") action {
        (_, c) => c.copy(dryRun = true)
      } text "Check all syntax and bindings and report errors"

      opt[Unit]('d', "debug") action {
        (_, c) => c.copy(debug = true)
      } text "Enable breakpoints and debugging"

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
          Deprecation.log("CLI option", "-p|--properties", Some("-c|--conf"))
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
          val formats = fs.split(",").toList.map(f => ReportFormat.valueOf(f))
          formats.find(_ == ReportFormat.rp) foreach { format =>
            Deprecation.log("Report Portal option", s"-f|--format = $format", None)
          }
          c.copy(reportFormats = formats)
      } valueName "reports" text s"""|Report formats to include in output (comma separated)
                                                                                               |- ${ReportFormat.values.filter(_.isCliOption).mkString(",")} (default = ${ReportFormat.html}, ${ReportFormat.rp} deprecated)""".stripMargin

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
      } valueName "file" text "Input data feed (csv or json file)"

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
          
          opt[Unit]("docker") action {
            (_, c) => {
              c.copy(initOptions = InitOption.docker :: c.initOptions)
            }
          } text "Generate files in project for running Gwen in Docker"

          opt[Unit]("jenkins") action {
            (_, c) => {
              c.copy(initOptions = InitOption.jenkins :: c.initOptions)
            }
          } text "Generate files in project for running Gwen on Jenkins"

          opt[Unit]("force") action {
            (_, c) => {
              c.copy(initOptions = InitOption.force :: c.initOptions)
            }
          } text "Regenerate and overwrite previously initialised files"

          arg[File]("dir").optional().action {
            (d, c) =>
              c.copy(initDir = d)
          } text s"Project directory to initialise (default is ${GwenOptions.Defaults.initDir.getPath()})"

      } text "Initialise project directory"

      cmd("format").action { 
        (_, c) => 
          c.copy(pretty = true) 

      } children {
          
        opt[Unit]("pretty") action {
            (_, c) => {
              c.copy(pretty = true)
            }
          } text "Pretty Gherkin format (default)"

        arg[File]("files|dirs").unbounded().required().action {
          (f, c) =>
            c.copy(formatFiles = c.formatFiles :+ f)
        } validate {
          f => if (f.exists) success else failure(s"File/dir not found: $f")
        } text s"Feature/meta files or dirs to format (space separated)"

      } text "Format feature and meta files"

    }

    (parser.parse(args, GwenOptions()).map { options =>
      new GwenOptions(
        options.batch,
        options.parallel,
        options.verbose,
        options.debug,
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
        options.initOptions,
        options.initDir,
        options.pretty,
        options.formatFiles)
      } map { options =>
        if (options.parallel) options.copy(batch = true)
        else options
      } tap { options =>
        options foreach { opt =>
          if ((opt.batch) && opt.features.isEmpty) {
            Errors.invocationError("No feature files or directories provided")
          }
          if (opt.reportFormats.nonEmpty && opt.reportDir.isEmpty) {
            val reportables = opt.reportFormats.filter(_.isFileSystemReport)
            Errors.invocationError(s"Required -r|--report option not provided for -f|--formats option${if (reportables.size > 1) "s" else ""}: ${reportables.mkString(",")}")
          }
          if (opt.debug && opt.parallel) {
            Errors.invocationError("Debug mode not supported for parallel executions")
          }
        }
      }).getOrElse(Errors.invocationError("Gwen invocation failed (see log for details)"))

  }

}
