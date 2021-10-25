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

import gwen.core.behavior.BehaviorMode
import gwen.core.behavior.FeatureMode
import gwen.core.state.StateLevel

import scala.util.Try

import java.io.File

/**
  * Provides access to all Gwen settings.
  *
  * @author Branko Juric
  */
object GwenSettings {

  /**
    * Checks and validates all settings.
    */
  def check(): Unit = {
    `gwen.assertion.mode`
    `gwen.associative.meta`
    `gwen.auto.discover.data.csv`
    `gwen.auto.discover.meta`
    `gwen.behavior.rules`
    `gwen.feature.dialect`
    `gwen.feature.failfast.enabled`
    `gwen.feature.failfast.exit`
    `gwen.feature.mode`
    `gwen.mask.char`
    `gwen.parallel.maxThreads`
    `gwen.rampup.interval.seconds`
    `gwen.report.overwrite`
    `gwen.report.suppress.meta`
    `gwen.report.slideshow.framespersecond`
    `gwen.state.level`
  }

  /**
   * Cap max threads to number of avilable processors.
   */
  val availableProcessors = Runtime.getRuntime().availableProcessors()

  /**
    * Provides access to the `gwen.feature.failfast.enabled` setting used to enable
    * or disable fail fast mode (default value is `true`).
    * Enabling this feature will fail a feature as soon as the first scenario in that
    * feature fails.  Other features (if provided) will resume.
    */
  def `gwen.feature.failfast.enabled`: Boolean = {
    Settings.getBoolean("gwen.feature.failfast.enabled", Some("gwen.feature.failfast"))
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
    * Provides access to the `gwen.report.overwrite` setting used to overwrite
    * or create backups of previously generated reports (default value is `false`).
    * Enabling this feature will create timestamped backups of previous reports.
    */
  def `gwen.report.overwrite`: Boolean = {
    Settings.getBoolean("gwen.report.overwrite")
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
    * Provides access to the `gwen.auto.discover.meta` setting used to enable
    * or disable automatic discovery of meta files (default value is `true` for enabled).
    * Disabling this will prevent Gwen from automatically discovering and loading meta files in the path of an
    * executing feature, forcing the user to control explicitly through the -m/--meta command line option which meta
    * files to load.
    */
  def `gwen.auto.discover.meta`: Boolean = {
    Settings.getBoolean("gwen.auto.discover.meta")
  }

  /**
    * Provides access to the `gwen.auto.discover.data.csv` setting used to enable
    * or disable automatic discovery of CSV data files (default value is `false` for enabled).
    * Disabling this will prevent Gwen from automatically discovering and loading CSV files in the path of an executing
    * feature, forcing the user to control explicitly through the -i/--input command line option which CSV files to load.
    */
  def `gwen.auto.discover.data.csv`: Boolean = {
    Settings.getBoolean("gwen.auto.discover.data.csv")
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
    * Provides access to the `gwen.associative.meta` setting used to control whether or
    * not meta files having the same name (excluding file extension) and same location as feature
    * files are only loaded for that feature and loaded last (default value is true).
    * This setting is only honoured if `gwen.auto.discover.meta` is also enabled.
    */
  def `gwen.associative.meta`: Boolean = {
    `gwen.auto.discover.meta` && Settings.getBoolean("gwen.associative.meta")
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
    val maxThreads = Settings.getInt("gwen.parallel.maxThreads")
    if (maxThreads < 0) {
      Errors.propertyLoadError("gwen.parallel.maxThreads", "cannot be less than 0")
    } else if (maxThreads == 0 || maxThreads > availableProcessors) {
      availableProcessors
    } else {
      maxThreads
    }
  }

  /**
    * Provides access to the character used to mask settings defined with the `:masked` suffix.
    * Default value is `●`.
    */
  def `gwen.mask.char`: Char = {
    val maskChar = sys.props("gwen.mask.char")
    if (maskChar.length != 1) {
      Errors.invalidSettingError("gwen.mask.char", maskChar, "Mask character length must be 1")
    } else {
      maskChar(0)
    }
  }

  /**
    * Provides access to the `gwen.output.dir` used to set the Gwen output directory.
    */
  def `gwen.output.dir`: File = {
    Settings.getFile("gwen.output.dir")
  }

}
