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

import org.scalatest.matchers.should.Matchers

class CLISettingsTest extends BaseTest with Matchers {

  "Default Gwen CLI settings" should "load" in {
    Settings.exclusively {
      CLISettings.`gwen.cli.options.batch` should be (false)
      CLISettings.`gwen.cli.options.format` should be (List(ReportFormat.html))
      CLISettings.`gwen.cli.options.conf` should be (Nil)
      CLISettings.`gwen.cli.options.dryRun` should be (false)
      CLISettings.`gwen.cli.options.features` should be (Nil)
      CLISettings.`gwen.cli.options.inputData` should be (None)
      CLISettings.`gwen.cli.options.meta` should be (Nil)
      CLISettings.`gwen.cli.options.parallel` should be (false)
      CLISettings.`gwen.cli.options.parallelFeatures` should be (false)
      CLISettings.`gwen.cli.options.report`.map(_.getPath) should be (Some("output/reports"))
      CLISettings.`gwen.cli.options.tags` should be (Nil)
      CLISettings.`gwen.cli.options.verbose` should be (false)
    }
  }

}
