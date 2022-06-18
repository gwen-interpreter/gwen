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

import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging

import scala.util.chaining._

import java.io.File

/**
  * Provides access to the default Gwen CLI settings.
  */
object CLISettings extends LazyLogging {

  private val (conf, _) =  Settings.load()

  check()

  /**
    * Checks and validates all CLI settings.
    */
  def check(): Unit = {
    `gwen.cli.options.batch`
    `gwen.cli.options.format`
    `gwen.cli.options.conf`
    `gwen.cli.options.dryRun`
    `gwen.cli.options.features`
    `gwen.cli.options.inputData`
    `gwen.cli.options.parallel`
    `gwen.cli.options.meta`
    `gwen.cli.options.report`
    `gwen.cli.options.tags`
  }

  /**
    * Provides access to the `gwen.cli.options.batch` setting used to set the default
    * -b/--batch CLI switch.
    */
  def `gwen.cli.options.batch`: Boolean = {
    Settings.getBoolean("gwen.cli.options.batch", None, Some(conf)) tap { enabled => 
      if (enabled) logger.warn("Setting gwen.cli.options.batch=true will enable batch mode only and disable REPL")
    }
  }

  /**
    * Provides access to the `gwen.cli.options.format` setting used to set the default
    * -f/--format CLI option.
    */
  def `gwen.cli.options.format`: List[ReportFormat] = {
    Settings.getList("gwen.cli.options.format", None, Some(conf)) map { value => 
      Settings.convert("gwen.cli.options.format", value, ReportFormat.values.mkString(", ")) { format =>
        ReportFormat.valueOf(format)
      }
    }
  }

  /**
    * Provides access to the `gwen.cli.options.conf` setting used to set the default
    * -c/--conf CLI option.
    */
  def `gwen.cli.options.conf`: List[File] = {
    Settings.getList("gwen.cli.options.conf", None, Some(conf)) map { filepath => 
      Settings.toFile(filepath)
    }
  }

  /**
    * Provides access to the `gwen.cli.options.dryRun` setting used to set the default
    * --n/--dry-run CLI switch.
    */
  def `gwen.cli.options.dryRun`: Boolean = {
    Settings.getBoolean("gwen.cli.options.dryRun", None, Some(conf)) tap { enabled => 
      if (enabled) logger.warn("Setting gwen.cli.options.dryRun=true will enable validation only and disable execution")
    }
  }
  
  /**
    * Provides access to the `gwen.cli.options.conf` setting used to set the default
    * features CLI argument.
    */
  def `gwen.cli.options.features`: List[File] = {
    Settings.getList("gwen.cli.options.features", None, Some(conf)) map { filepath => 
      Settings.toFile(filepath)
    }
  }

  /**
    * Provides access to the `gwen.cli.options.inputData` setting used to set the default
    * -i/--input-data CLI option.
    */
  def `gwen.cli.options.inputData`: Option[File] = {
    Settings.getList("gwen.cli.options.inputData", None, Some(conf)).headOption map { filepath => 
      Settings.toFile(filepath)
    }
  }

  /**
    * Provides access to the `gwen.cli.options.meta` setting used to set the default
    * -m/--meta CLI option.
    */
  def `gwen.cli.options.meta`: List[File] = {
    Settings.getList("gwen.cli.options.meta", None, Some(conf)) map { filepath => 
      Settings.toFile(filepath)
    }
  }

  /**
    * Provides access to the `gwen.cli.options.parallel` setting used to set the default
    * --parallel CLI switch.
    */
  def `gwen.cli.options.parallel`: Boolean = {
    Settings.getBoolean("gwen.cli.options.parallel", None, Some(conf))
  }

  /**
    * Provides access to the `gwen.cli.options.report` setting used to set the default
    * -r/--report CLI option.
    */
  def `gwen.cli.options.report`: Option[File] = {
    Settings.getOpt("gwen.cli.options.report", None, Some(conf)) map { filepath => 
      Settings.toFile(filepath)
    }
  }

  /**
    * Provides access to the `gwen.cli.options.tags` setting used to set the default
    * -t/--tags CLI option.
    */
  def `gwen.cli.options.tags`: List[(Tag, Boolean)] = {
    Settings.getList("gwen.cli.options.tags", None, Some(conf)) map { tag => 
      (Tag(tag), tag.startsWith("@"))
    }
  }

  /**
    * Provides access to the `gwen.cli.options.verbose` setting used to set the default
    * -v/--verbose CLI option.
    */
  def `gwen.cli.options.verbose`: Boolean = {
    Settings.getBoolean("gwen.cli.options.verbose", None, Some(conf))
  }

}
