/*
 * Copyright 2021 Branko Juric, Brady Wood
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

import gwen.core.report.ReportFormat
import gwen.core.node.gherkin.Tag

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging

import scala.util.chaining._

import java.io.File

/**
  * Provides access to the default Gwen settings.
  */
object BootstrapSettings extends LazyLogging {

  private var conf = Settings.BootstrapConf

  def mergeProcessSettings(process: Process): Unit = {
    this.conf = Settings.processConf(process)
  }

  check()

  /**
    * Checks and validates all launch settings.
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
  }

  /**
   * Provides access to the `gwen.baseDir` setting used to set the Gwen base directory.
   */
  def `gwen.baseDir`: File = {
    Settings.toFile(Settings.get("gwen.baseDir", None, Some(conf)))
  }

  /**
    * Provides access to the `gwen.launch.options.batch` setting used to set the default
    * -b/--batch switch.
    */
  def `gwen.launch.options.batch`: Boolean = {
    Settings.getBoolean("gwen.launch.options.batch", Some("gwen.cli.options.batch"), Some(conf)) tap { enabled => 
      if (enabled) logger.warn("Setting gwen.launch.options.batch=true will enable batch mode only and disable REPL")
    }
  }

  /**
    * Provides access to the `gwen.launch.options.format` setting used to set the default
    * -f/--format option.
    */
  def `gwen.launch.options.format`: List[ReportFormat] = {
    Settings.getList("gwen.launch.options.format", Some("gwen.cli.options.format"), Some(conf)) map { value => 
      Settings.convert("gwen.launch.options.format", value, ReportFormat.values.mkString(", ")) { format =>
        ReportFormat.valueOf(format)
      }
    } distinct
  }

  /**
    * Provides access to the `gwen.launch.options.dryRun` setting used to set the default
    * --n/--dry-run switch.
    */
  def `gwen.launch.options.dryRun`: Boolean = {
    Settings.getBoolean("gwen.launch.options.dryRun", Some("gwen.cli.options.dryRun"), Some(conf)) tap { enabled => 
      if (enabled) logger.warn("Setting gwen.launch.options.dryRun=true will enable validation only and disable execution")
    }
  }
  
  /**
    * Provides access to the `gwen.launch.options.features` setting used to set the default
    * features argument.
    */
  def `gwen.launch.options.features`: List[File] = {
    Settings.getList("gwen.launch.options.features", Some("gwen.cli.options.features"), Some(conf)) map { filepath => 
      Settings.toFile(filepath)
    }
  }

  /**
    * Provides access to the `gwen.launch.options.inputData` setting used to set the default
    * -i/--input-data option.
    */
  def `gwen.launch.options.inputData`: Option[File] = {
    Settings.getOpt("gwen.launch.options.inputData", Some("gwen.cli.options.inputData"), Some(conf)).headOption.filter(_.trim().size > 0) map { filepath => 
      Settings.toFile(filepath)
    }
  }

  /**
    * Provides access to the `gwen.launch.options.meta` setting used to set the default
    * -m/--meta argument.
    */
  def `gwen.launch.options.meta`: List[File] = {
    Settings.getList("gwen.launch.options.meta", Some("gwen.cli.options.meta"), Some(conf)) map { filepath => 
      Settings.toFile(filepath)
    }
  }

  /**
    * Provides access to the `gwen.launch.options.parallel` setting used to set the default
    * --parallel switch.
    */
  def `gwen.launch.options.parallel`: Boolean = {
    Settings.getBoolean("gwen.launch.options.parallel", Some("gwen.cli.options.parallel"), Some(conf))
  }

  /**
    * Provides access to the `gwen.launch.options.report` setting used to set the default
    * -r/--report option.
    */
  def `gwen.launch.options.report`: Option[File] = {
    Settings.getOpt("gwen.launch.options.report", Some("gwen.cli.options.report"), Some(conf)) map { filepath => 
      Settings.toFile(filepath)
    }
  }

  /**
    * Provides access to the `gwen.launch.options.tags` setting used to set the default
    * -t/--tags option.
    */
  def `gwen.launch.options.tags`: List[(Tag, Boolean)] = {
    Settings.getList("gwen.launch.options.tags", Some("gwen.cli.options.tags"), Some(conf)) map { tag => 
      (Tag(tag), tag.startsWith("@"))
    }
  }

  /**
    * Provides access to the `gwen.launch.options.verbose` setting used to set the default
    * -v/--verbose option.
    */
  def `gwen.launch.options.verbose`: Boolean = {
    Settings.getBoolean("gwen.launch.options.verbose", Some("gwen.cli.options.verbose"), Some(conf))
  }

}
