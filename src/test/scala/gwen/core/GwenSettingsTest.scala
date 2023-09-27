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

import gwen.core.AssertionMode
import gwen.core.behavior.BehaviorMode
import gwen.core.behavior.FeatureMode
import gwen.core.report.rp.RPConfig.ErrorBlocks
import gwen.core.report.rp.RPConfig.ErrorReportingMode
import gwen.core.report.rp.RPConfig.StepDefFormat
import gwen.core.report.rp.RPConfig.TestCaseIdKeys
import gwen.core.report.rp.RPSettings
import gwen.core.state.StateLevel

import org.scalatest.matchers.should.Matchers

class GwenSettingsTest extends BaseTest with Matchers {

  "Default Gwen core settings" should "load" in {
    Settings.exclusively {
      GwenSettings.`gwen.assertion.mode` should be (AssertionMode.hard)
      GwenSettings.`gwen.associative.meta` should be (true)
      GwenSettings.`gwen.auto.bind.tableData.outline.examples` should be (true)
      GwenSettings.`gwen.auto.discover.data.csv` should be (true)
      GwenSettings.`gwen.auto.discover.data.json` should be (false)
      GwenSettings.`gwen.auto.discover.meta` should be (true)
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
    }
  }

  "Default Gwen RP settings" should "load" in {
    Settings.exclusively {
      RPSettings.`gwen.rp.debug` should be (false)
      RPSettings.`gwen.rp.heartbeat.enabled` should be (true)
      RPSettings.`gwen.rp.heartbeat.timeoutSecs` should be (3)
      RPSettings.`gwen.rp.send.annotations` should be (false)
      RPSettings.`gwen.rp.send.breadcrumbs` should be (false)
      RPSettings.`gwen.rp.send.failed.envTrace` should be (ErrorReportingMode.none)
      RPSettings.`gwen.rp.send.failed.errorBlocks` should be (ErrorBlocks.none)
      RPSettings.`gwen.rp.send.failed.errorTrace` should be (ErrorReportingMode.none)
      RPSettings.`gwen.rp.send.failed.hierarchy` should be (ErrorReportingMode.inlined)
      RPSettings.`gwen.rp.send.failed.stepDefs` should be (StepDefFormat.inlined)
      RPSettings.`gwen.rp.send.markdownBlocks` should be (true)
      RPSettings.`gwen.rp.send.meta` should be (false)
      RPSettings.`gwen.rp.send.stepDefs` should be (StepDefFormat.none)
      RPSettings.`gwen.rp.send.tags` should be (true)
      RPSettings.`gwen.rp.testCaseId.keys` should be (TestCaseIdKeys.`nodePath+params`)
    }
  }

}
