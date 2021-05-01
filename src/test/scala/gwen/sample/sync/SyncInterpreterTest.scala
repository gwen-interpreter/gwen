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

import gwen.BaseTest
import gwen.GwenOptions
import gwen.eval.EvalContext
import gwen.eval.EvalEnvironment
import gwen.eval.EvalEngine
import gwen.eval.GwenInterpreter
import gwen.eval.GwenLauncher
import gwen.model.Failed
import gwen.model.Passed
import gwen.report.ReportFormat

import java.io.File

import org.scalatest.prop.TableDrivenPropertyChecks.forAll

class SyncEvalContext
  extends EvalContext(GwenOptions(), new EvalEnvironment()) {
  override def dsl: List[String] = Nil
}

trait SyncEvalEngine extends EvalEngine[SyncEvalContext] {
  override def init(options: GwenOptions, envOpt: Option[EvalEnvironment] = None): SyncEvalContext = new SyncEvalContext()
}

class SyncInterpreter
  extends GwenInterpreter[SyncEvalContext]
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
