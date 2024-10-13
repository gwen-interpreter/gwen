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

class LaunchSettingsTest extends BaseTest with Matchers {

  "Default Gwen launch settings" should "load" in {
    Settings.exclusively {
      LaunchSettings.`gwen.launch.options.batch` should be (false)
      LaunchSettings.`gwen.launch.options.format` should be (Nil)
      LaunchSettings.`gwen.launch.options.dryRun` should be (false)
      LaunchSettings.`gwen.launch.options.features` should be (Nil)
      LaunchSettings.`gwen.launch.options.inputData` should be (None)
      LaunchSettings.`gwen.launch.options.meta` should be (Nil)
      LaunchSettings.`gwen.launch.options.parallel` should be (false)
      LaunchSettings.`gwen.launch.options.report`.map(_.getPath) should be (Some("output/reports"))
      LaunchSettings.`gwen.launch.options.tags` should be (Nil)
      LaunchSettings.`gwen.launch.options.verbose` should be (false)
    }
  }

}
