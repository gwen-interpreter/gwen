/*
 * Copyright 2018 Branko Juric, Brady Wood
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
package gwen.sample.sync

import gwen.dsl.Failed
import gwen.dsl.Passed
import gwen.eval.GwenOptions
import java.io.File

import org.scalatest.prop.TableDrivenPropertyChecks.forAll
import gwen.eval.GwenLauncher
import gwen.report.ReportFormat
import gwen.eval.EnvContext
import gwen.eval.EvalEngine
import gwen.eval.GwenInterpreter
import gwen.eval.support.DefaultEngineSupport
import gwen.BaseTest

class SyncEnvContext(val options: GwenOptions)
  extends EnvContext(options) {
  override def dsl: List[String] = Nil
}

trait SyncEvalEngine extends EvalEngine[SyncEnvContext] with DefaultEngineSupport[SyncEnvContext] {
  override def init(options: GwenOptions): SyncEnvContext = new SyncEnvContext(options)
}

class SyncInterpreter
  extends GwenInterpreter[SyncEnvContext]
  with SyncEvalEngine

class SyncInterpreterTest extends BaseTest {
  
  forAll (levels) { level =>
    s"Synced StepDef using $level level state" should "evaluate one feature at time in parallel execution mode" in { 
      withSetting("gwen.state.level", level) {
        val options = GwenOptions(
          batch = true,
          parallel = true,
          reportDir = Some(new File(s"target/report/sync/$level-level")),
          reportFormats = List(ReportFormat.html, ReportFormat.junit, ReportFormat.json),
          features = List(new File("features/sample/sync"))
        )
          
        val launcher = new GwenLauncher(new SyncInterpreter())
        launcher.run(options, None) match {
          case Passed(_) => // excellent :)
          case Failed(_, error) => error.printStackTrace(); fail(error.getMessage)
          case _ => fail("evaluation expected but got noop")
        }
      }
    }
  }
  
  forAll (levels) { level =>
    s"Synced StepDef using $level level state" should "pass --dry-run test" in {  
      withSetting("gwen.state.level", level) {
        val options = GwenOptions(
          batch = true,
          parallel = true,
          reportDir = Some(new File(s"target/report/sync-dry-run/$level-level")),
          reportFormats = List(ReportFormat.html, ReportFormat.junit, ReportFormat.json),
          features = List(new File("features/sample/sync")),
          dryRun = true
        )
          
        val launcher = new GwenLauncher(new SyncInterpreter())
        launcher.run(options, None) match {
          case Passed(_) => // excellent :)
          case Failed(_, error) => error.printStackTrace(); fail(error.getMessage)
          case _ => fail("evaluation expected but got noop")
        }
      }
    }
  }
  
}
