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

package gwen.core

import gwen.core._
import gwen.core.behavior.BehaviorMode
import gwen.core.behavior.FeatureMode
import gwen.core.node.gherkin.Tag
import gwen.core.state.StateLevel
import gwen.core.report.ReportFormat
import gwen.core.result.ResultFile

import scala.util.chaining._
import scala.util.Try

import java.io.File
import java.util.logging.Level

import com.typesafe.scalalogging.LazyLogging

/**
  * Provides access to all Gwen settings.
  *
  * @author Branko Juric
  */
object GwenSettings extends LazyLogging {

  /**
    * Checks and validates all settings.
    */
  def check(): Unit = {
    `gwen.baseDir`
    `gwen.launch.options.batch`
    `gwen.launch.options.format`
    `gwen.launch.options.dryRun`
    `gwen.launch.options.features`
    `gwen.launch.options.inputData`
    `gwen.launch.options.parallel`
    `gwen.launch.options.meta`
    `gwen.launch.options.report`
    `gwen.launch.options.tags`
    `gwen.assertion.mode`
    `gwen.auto.trim.data.csv`
    `gwen.auto.trim.data.json`
    `gwen.behavior.rules`
    `gwen.feature.dialect`
    `gwen.feature.failfast.enabled`
    `gwen.feature.failfast.exit`
    `gwen.feature.mode`
    `gwen.mask.char`
    `gwen.parallel.maxThreads`
    `gwen.rampup.interval.seconds`
    `gwen.report.attach.functions`
    `gwen.report.overwrite`
    `gwen.report.suppress.meta`
    `gwen.report.slideshow.framespersecond`
    `gwen.state.level`
    `gwen.video.dir`
    `gwen.video.timeoutSecs`
    `gwen.logLevel.deprecations`
    `gwen.input.data.readOnly`
  }

  /**
   * Cap max threads to number of avilable processors.
   */
  val availableProcessors = Runtime.getRuntime().availableProcessors()

  /**
   * Provides access to the `gwen.baseDir` setting used to set the Gwen base directory.
   */
  def `gwen.baseDir`: File = Settings.getFile("gwen.baseDir")

  /**
    * Provides access to the `gwen.launch.options.batch` setting used to set the default
    * -b/--batch switch.
    */
  def `gwen.launch.options.batch`: Boolean = {
    Settings.getBoolean("gwen.launch.options.batch", Some("gwen.cli.options.batch")) tap { enabled => 
      if (enabled) logger.warn("Setting gwen.launch.options.batch=true will enable batch mode only and disable REPL")
    }
  }

  /**
    * Provides access to the `gwen.launch.options.format` setting used to set the default
    * -f/--format option.
    */
  def `gwen.launch.options.format`: List[ReportFormat] = {
    Settings.getList("gwen.launch.options.format", Some("gwen.cli.options.format")) map { value => 
      Settings.convert("gwen.launch.options.format", value, ReportFormat.values.mkString(", ")) { format =>
        ReportFormat.valueOf(format)
      }
    }
  }

  /**
    * Provides access to the `gwen.launch.options.dryRun` setting used to set the default
    * --n/--dry-run switch.
    */
  def `gwen.launch.options.dryRun`: Boolean = {
    Settings.getBoolean("gwen.launch.options.dryRun", Some("gwen.cli.options.dryRun")) tap { enabled => 
      if (enabled) logger.warn("Setting gwen.launch.options.dryRun=true will enable validation only and disable execution")
    }
  }
  
  /**
    * Provides access to the `gwen.launch.options.features` setting used to set the default
    * features argument.
    */
  def `gwen.launch.options.features`: List[File] = {
    Settings.getList("gwen.launch.options.features", Some("gwen.cli.options.features")) map { filepath => 
      Settings.toFile(filepath)
    }
  }

  /**
    * Provides access to the `gwen.launch.options.inputData` setting used to set the default
    * -i/--input-data option.
    */
  def `gwen.launch.options.inputData`: Option[File] = {
    Settings.getOpt("gwen.launch.options.inputData", Some("gwen.cli.options.inputData")).headOption.filter(_.trim().size > 0) map { filepath => 
      Settings.toFile(filepath)
    }
  }

  /**
    * Provides access to the `gwen.launch.options.meta` setting used to set the default
    * -m/--meta argument.
    */
  def `gwen.launch.options.meta`: List[File] = {
    Settings.getList("gwen.launch.options.meta", Some("gwen.cli.options.meta")) map { filepath => 
      Settings.toFile(filepath)
    }
  }

  /**
    * Provides access to the `gwen.launch.options.parallel` setting used to set the default
    * --parallel switch.
    */
  def `gwen.launch.options.parallel`: Boolean = {
    Settings.getBoolean("gwen.launch.options.parallel", Some("gwen.cli.options.parallel"))
  }

  /**
    * Provides access to the `gwen.launch.options.report` setting used to set the default
    * -r/--report option.
    */
  def `gwen.launch.options.report`: Option[File] = {
    Settings.getOpt("gwen.launch.options.report", Some("gwen.cli.options.report")) map { filepath => 
      Settings.toFile(filepath)
    }
  }

  /**
    * Provides access to the `gwen.launch.options.tags` setting used to set the default
    * -t/--tags option.
    */
  def `gwen.launch.options.tags`: List[(Tag, Boolean)] = {
    Settings.getList("gwen.launch.options.tags", Some("gwen.cli.options.tags")) map { tag => 
      (Tag(tag), tag.startsWith("@"))
    }
  }

  /**
    * Provides access to the `gwen.launch.options.verbose` setting used to set the default
    * -v/--verbose option.
    */
  def `gwen.launch.options.verbose`: Boolean = {
    Settings.getBoolean("gwen.launch.options.verbose", Some("gwen.cli.options.verbose"))
  }
  
  /**
   * Provides access to the name of the currently running gwen profile.
   */
  def `gwen.profile.name`: Option[String] = {
    Settings.getOpt(ImplicitValueKeys.`gwen.profile.name`)
  }

  /**
    * Provides access to the `gwen.feature.failfast.enabled` setting used to enable
    * or disable fail fast mode (default value is `true`).
    * Enabling this feature will fail a feature as soon as the first scenario in that
    * feature fails.  Other features (if provided) will resume.
    */
  def `gwen.feature.failfast.enabled`: Boolean = {
    Settings.getBoolean("gwen.feature.failfast.enabled")
  }

  /**
    * Provides access to the `gwen.feature.failfast.exit` setting used to exit all execution
    * on first failure (default value is `false`).
    * Enabling this feature will exit execution when the first failure is detected.
    */
  def `gwen.feature.failfast.exit`: Boolean = {
    `gwen.feature.failfast.enabled` && Settings.getBoolean("gwen.feature.failfast.exit")
  }

  /**
   * Provides access to the `gwen.report.slideshow.framespersecond` setting
   * used to set the default frame per second (speed) of the slideshow (default value is 4).
   */
  def `gwen.report.slideshow.framespersecond`: Int = {
    Settings.getInt("gwen.report.slideshow.framespersecond")
  }

  /**
    * Provides access to the `gwen.report.attach.functions` setting used to control whether 
    * or not evaluted functions are attached to the HTML report (default value is `true`).
    */
  def `gwen.report.attach.functions`: Boolean = {
    Settings.getBoolean("gwen.report.attach.functions")
  }

  /**
    * Provides access to the `gwen.report.overwrite` setting used to overwrite
    * or create backups of previously generated reports (default value is `false`).
    * Enabling this feature will create timestamped backups of previous reports.
    */
  def `gwen.report.overwrite`: Boolean = {
    Settings.getBoolean("gwen.report.overwrite")
  }

  /**
    * Provides access to the `gwen.report.stepDef.indent.pixels` setting used to the 
    * indent size of StepDefs in HTML reports.
    */
  def `gwen.report.stepDef.indent.pixels`: Int = {
    Settings.getInt("gwen.report.stepDef.indent.pixels")
  }

  /**
   * Provides access to the `gwen.rampup.interval.seconds` setting used
   * to set the ramp up interval (in seconds) for staggering parallel feature executions
   * (this setting is optional and only used in for parallel execution options).
   */
  def `gwen.rampup.interval.seconds`: Option[Long] = {
    Settings.getLongOpt("gwen.rampup.interval.seconds").filter(_ > 0)
  }

    /**
    * Controls whether or not slideshows should be generated in HTML reports. This property is implicitly set to true
    * in the web engine only if screenshot capturing is enabled (gwen.web.capture.screenshots.enabled=true in web engine settings).
    * Users should not explicitly set this value.
    */
  def `gwen.report.slideshow.create` = {
    Settings.getBooleanOpt("gwen.report.slideshow.create").getOrElse(false)
  }


  /**
    * Provides access to the `gwen.report.suppress.meta` setting used to control whether
    * or not meta report generation will be suppressed (default value is `true`).
    */
  def `gwen.report.suppress.meta`: Boolean = {
    Settings.getBoolean("gwen.report.suppress.meta")
  }

  /**
  * Provides access to the `gwen.auto.bind.tableData.outline.examples` setting used to control whether or
  * not to bind row data in outline examples tables to same named attributes at runitme (default value is true).
  */
  def `gwen.auto.bind.tableData.outline.examples`: Boolean = {
    Settings.getBoolean("gwen.auto.bind.tableData.outline.examples")
  }

  /**
    * Provides access to the `gwen.auto.trim.data.csv` setting used to enable
    * or disable automatic trimming of CSV data (default value is `false` for no triming).
    */
  def `gwen.auto.trim.data.csv`: Boolean = {
    Settings.getBoolean("gwen.auto.trim.data.csv")
  }

  /**
    * Provides access to the `gwen.auto.trim.data.json` setting used to enable
    * or disable automatic trimming of JSON data (default value is `false` for no triming).
    */
  def `gwen.auto.trim.data.json`: Boolean = {
    Settings.getBoolean("gwen.auto.trim.data.json")
  }

  /**
    * Provides access to the `gwen.assertion.mode` setting used to enable hard, soft, or sustained
    * assertions (default value is `hard`).
    *   - `hard` - Halts processing on first assertion failure
    *   - `soft` - Collects all assertion failures and continues processing
    *   - `sustained` - Collects all assertion failures and continues processing without raising failure
    */
  def `gwen.assertion.mode`: AssertionMode = {
    AssertionMode.valueOf(Settings.get("gwen.assertion.mode"))
  }

  /**
    * Provides access to the `gwen.state.level` setting used to control if state is maintained
    * at the scenario or feature level during execution.
    *   - `feature` - Feature level state is shared across scenarios (default)
    *   - `scenario` - Each scenario gets a new state which is not shared across scenarios
    */
  def `gwen.state.level`: StateLevel = {
    StateLevel.valueOf(Settings.get("gwen.state.level"))
  }

  /**
    * Provides access to the `gwen.feature.mode` setting used to determine whether the
    * feature mode is declarative or imperative (default value is `imperative`). When declarative,
    * the DSL steps defined in the Gwen engine cannot be used directly in features and must be
    * bound to step definitions defined in meta instead. This forces the user to write features
    * that are clean and free of automation concerns. When imperative, then DSL steps can be used
    * directly in features.
    */
  def `gwen.feature.mode`: FeatureMode = {
    FeatureMode.valueOf(Settings.get("gwen.feature.mode"))
  }

  /**
    * Provides access to the `gwen.behavior.rules` setting used to determine whether strict,
    * or lenient rules around Given-When-Then usage should be enforced in features (default value is
    * `strict`). When strict, scenarios and backgrounds must contain Given-When-Then ordered steps
    * and Given steps set context, When steps must perform actions, and Then or But steps must perform
    * assertions. When `leneient` no behavioral rules are enforced. Note that `gwen.behavior.rules` is
    * an alias for this setting.
    */
  def `gwen.behavior.rules`: BehaviorMode = {
    BehaviorMode.valueOf(Settings.getOpt("gwen.behavior.rules").getOrElse(Settings.get("gwen.behavior.rules")))
  }

  /**
    * Provides access to the `gwen.feature.dialect` setting used to set the default
    * dialect for Gherkin keywords in features.
    */
  def `gwen.feature.dialect`: String = {
    Settings.get("gwen.feature.dialect")
  }

  /**
    * Provides access to the `gwen.parallel.maxThreads` setting used to set the maximum number
    * of threads to use in parallel execution mode. The value will default to the
    * number of avialable processors in the host environment if it is not specified or exceeds
    * that value.
    */
  def `gwen.parallel.maxThreads`: Int = {
    val maxThreads = Settings.getOpt("gwen.parallel.maxThreads") map { n => 
      if (n == "auto") 0 else Settings.getInt("gwen.parallel.maxThreads")
    } getOrElse 0
    if (maxThreads < 0) {
      Errors.propertyLoadError("gwen.parallel.maxThreads", "cannot be less than 0")
    } else if (maxThreads == 0) {
      availableProcessors
    } else {
      maxThreads
    }
  }

  /**
    * Provides access to the character used to mask settings defined with the `:masked` suffix.
    * Default value is `*`.
    */
  def `gwen.mask.char`: Char = {
    val maskChar = Settings.get("gwen.mask.char")
    if (maskChar.length != 1) {
      Errors.invalidSettingError("gwen.mask.char", maskChar, "Mask character length must be 1")
    } else {
      maskChar(0)
    }
  }

  /**
    * Provides access to the `gwen.console.log.colors` setting used to control whether or not to use ANSI 
    * colors to highlight Gherkin keywords and evaulation results in all console output.
    */
  def `gwen.console.log.colors`: Boolean = Settings.getBoolean("gwen.console.log.colors")

  /**
    * Provides access to the `gwen.console.log.depth` setting used to control the depth level of steps to log
    (default is 1 for top level steps only). Can be set to 'infinity' to log all levels.
    */
  def `gwen.console.log.depth`: Int = { 
    if (Settings.getOpt("gwen.console.log.depth").map(_ == "infinity").getOrElse(false)) Int.MaxValue
    else Settings.getInt("gwen.console.log.depth")
  }

  /**
    * Provides access to the `gwen.console.repl.autoSuggestions` setting used to control whether or not auto
    * suggestions are enabled in the repl.
    */
  def `gwen.console.repl.autoSuggestions`: Boolean = Settings.getBoolean("gwen.console.repl.autoSuggestions")

  /**
    * Provides access to the `gwen.console.repl.tabCompletion` setting used to control whether or not tab
    * completion is enabled in the repl.
    */
  def `gwen.console.repl.tabCompletion`: Boolean = Settings.getBoolean("gwen.console.repl.tabCompletion")

  /**
    * Provides access to the `gwen.dryRun.limit.tableData.outline.examples.records` setting used to control the number of 
    * records in examples tables to include in dry runs (default is infinity for all steps).
    */
  def `gwen.dryRun.limit.tableData.outline.examples.records`: Int = {
    if (Settings.getOpt("gwen.dryRun.limit.tableData.outline.examples.records").map(_ == "infinity").getOrElse(false)) Int.MaxValue
    else Settings.getInt("gwen.dryRun.limit.tableData.outline.examples.records")
  }

  /**
    * Provides access to the `gwen.console.log.stepDefs` setting used to control whether or to log StepDefs
    * to all console outputs.
    */
  def `gwen.console.log.stepDefs`: Boolean = {
    Settings.getBoolean("gwen.console.log.stepDefs")
  }

  /**
    * Provides access to the `gwen.outDir` setting used to set the Gwen output directory.
    */
  def `gwen.outDir`: File = {
    Settings.getFile("gwen.outDir")
  }

  /**
   * Gets the video output directory (default is ${gwen.outDir}/.video).
   */
  def `gwen.video.dir`: File = {
    Settings.getFileOpt("gwen.video.dir").getOrElse(new File(`gwen.outDir`, ".video"))
  }

  /**
   * Gets the number of seconds to wait for video file to be ready when generating report (default is 10 secs).
   */
  def `gwen.video.timeoutSecs`: Int = {
    Settings.getIntOpt("gwen.video.timeoutSecs").getOrElse(10)
  }

  /**
    * Provides access to the `gwen.error.messages.inline.locators` setting used to control whether or to inline 
    * locator binding info in reported error messages (default is false).
    */
  def `gwen.error.messages.inline.locators`: Boolean = {
      Settings.getBoolean("gwen.error.messages.inline.locators")
  }

  /**
    * Provides access to the `gwen.behavior.rules` setting used to determine whether strict,
    * or lenient rules around Given-When-Then usage should be enforced in features (default value is
    * `strict`). When strict, scenarios and backgrounds must contain Given-When-Then ordered steps
    * and Given steps set context, When steps must perform actions, and Then or But steps must perform
    * assertions. When `leneient` no behavioral rules are enforced. Note that `gwen.behavior.rules` is
    * an alias for this setting.
    */
  def `gwen.logLevel.deprecations`: Level = {
    Settings.getOpt("gwen.logLevel.deprecations") map { level => 
      level match {
        case "warn" => Level.WARNING
        case "error" => Level.SEVERE
        case "none" => Level.OFF
        case _ => Errors.invalidSettingError("gwen.logLevel.deprecations", level, "Valid value are warn|error|none")
      }
    } getOrElse {
      Level.WARNING
    }
  }

  /**
    * Provides access to the `gwen.input.data.readOnly` setting used to control whether or not
    * input data should be read only.
    */
  def `gwen.input.data.readOnly`: Boolean = {
    Settings.getBoolean("gwen.input.data.readOnly")
  }

  def `gwen.report.results.files`(options: GwenOptions): List[ResultFile] = {
    val fileSettings = Settings.findAll(_.startsWith(ResultFile.SettingsKey))
    fileSettings.filter(_._1.endsWith(".file")).keys.map(_.drop(ResultFile.SettingsKey.length + 1).dropRight(".file".length)).toList.sorted map { id => 
      ResultFile(id, fileSettings, options)
    }
  }

}
