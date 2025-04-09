/*
 * Copyright 2021-2024 Branko Juric, Brady Wood
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

import gwen.core.AssertionMode
import gwen.core.behavior.BehaviorMode
import gwen.core.behavior.FeatureMode
import gwen.core.state.StateLevel
import gwen.core.status.StatusKeyword

import org.scalatest.matchers.should.Matchers

import java.io.File
import java.util.logging.Level

class GwenSettingsTest extends BaseTest with Matchers {

  "Default Gwen core settings" should "load" in {
    GwenSettings.`gwen.launch.options.batch` should be (false)
    GwenSettings.`gwen.launch.options.format` should be (Nil)
    GwenSettings.`gwen.launch.options.dryRun` should be (false)
    GwenSettings.`gwen.launch.options.features` should be (Nil)
    GwenSettings.`gwen.launch.options.inputData` should be (None)
    GwenSettings.`gwen.launch.options.meta` should be (Nil)
    GwenSettings.`gwen.launch.options.parallel` should be (false)
    GwenSettings.`gwen.launch.options.report`.map(_.getPath) should be (Some("output/reports"))
    GwenSettings.`gwen.launch.options.tags` should be (Nil)
    GwenSettings.`gwen.launch.options.verbose` should be (false)
    GwenSettings.`gwen.assertion.mode` should be (AssertionMode.hard)
    GwenSettings.`gwen.auto.bind.tableData.outline.examples` should be (true)
    GwenSettings.`gwen.auto.trim.data.csv` should be (false)
    GwenSettings.`gwen.auto.trim.data.json` should be (false)
    GwenSettings.`gwen.behavior.rules` should be (BehaviorMode.lenient)
    GwenSettings.`gwen.feature.dialect` should be ("en")
    GwenSettings.`gwen.feature.failfast.enabled` should be (true)
    GwenSettings.`gwen.feature.failfast.exit` should be (false)
    GwenSettings.`gwen.feature.mode` should be (FeatureMode.imperative)
    GwenSettings.`gwen.mask.char` should be ('*')
    GwenSettings.`gwen.baseDir`.getPath should be (".")
    GwenSettings.`gwen.outDir`.getPath should be ("output")
    GwenSettings.`gwen.parallel.maxThreads` should be (GwenSettings.availableProcessors)
    GwenSettings.`gwen.rampup.interval.seconds` should be (None)
    GwenSettings.`gwen.report.stepDef.indent.pixels` should be (20)
    GwenSettings.`gwen.report.attach.functions` should be (true)
    GwenSettings.`gwen.report.overwrite` should be (false)
    GwenSettings.`gwen.report.suppress.meta` should be (true)
    GwenSettings.`gwen.report.slideshow.create` should be (false)
    GwenSettings.`gwen.report.slideshow.framespersecond` should be (4)
    GwenSettings.`gwen.state.level` should be (StateLevel.feature)
    GwenSettings.`gwen.console.log.colors` should be (true)
    GwenSettings.`gwen.console.log.depth` should be (1)
    GwenSettings.`gwen.console.log.stepDefs` should be (true)
    GwenSettings.`gwen.console.repl.autoSuggestions` should be (true)
    GwenSettings.`gwen.console.repl.tabCompletion` should be (true)
    GwenSettings.`gwen.video.dir`.getPath should be ("output/.video")
    GwenSettings.`gwen.video.timeoutSecs` should be (10)
    GwenSettings.`gwen.dryRun.limit.tableData.outline.examples.records` should be (Integer.MAX_VALUE)
    GwenSettings.`gwen.error.messages.inline.locators` should be (false)
    GwenSettings.`gwen.logLevel.deprecations` should be (Level.WARNING)
    GwenSettings.`gwen.report.results.files`(GwenOptions()).length should be (0)
    GwenSettings.`gwen.input.data.readOnly` should be (true)
    GwenSettings.`gwen.input.data.maskFields` should be (Nil)
  }
}
