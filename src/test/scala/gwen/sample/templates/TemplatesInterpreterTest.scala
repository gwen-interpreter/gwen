/*
 * Copyright 2017 Branko Juric, Brady Wood
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
package gwen.sample.templates

import gwen.BaseTest
import gwen.dsl.Failed
import gwen.dsl.Passed
import gwen.eval.EnvContext
import gwen.eval.GwenLauncher
import gwen.eval.GwenInterpreter
import gwen.eval.GwenOptions
import gwen.eval.support.DefaultEngineSupport
import gwen.report.ReportFormat

import org.scalatest.prop.TableDrivenPropertyChecks.forAll

import java.io.File

class TemplatesEnvContext(val options: GwenOptions)
  extends EnvContext(options) {
  override def dsl: List[String] = Nil
}

trait TemplatesEvalEngine extends DefaultEngineSupport[TemplatesEnvContext] {
  override def init(options: GwenOptions): TemplatesEnvContext = new TemplatesEnvContext(options)
}

class TemplatesInterpreter
  extends GwenInterpreter[TemplatesEnvContext]
  with TemplatesEvalEngine

class TemplatesInterpreterTest extends BaseTest {
  
  forAll (levels) { level =>
    s"Templates using $level level state" should "evaluate without error" in {  
      withSetting("gwen.state.level", level) {
        val options = GwenOptions(
          batch = true,
          reportDir = Some(new File(s"target/report/templates/$level-level")),
          reportFormats = List(ReportFormat.html, ReportFormat.junit, ReportFormat.json),
          features = List(new File("features/sample/templates"))
        )
          
        val launcher = new GwenLauncher(new TemplatesInterpreter())
        launcher.run(options, None) match {
          case Passed(_) => // excellent :)
          case Failed(_, error) => error.printStackTrace(); fail(error.getMessage)
          case _ => fail("evaluation expected but got noop")
        }
      }
    }
  }
  
  forAll (levels) { level =>
    s"Templates using $level level state" should "pass --dry-run test" in {  
      withSetting("gwen.state.level", level) {
        val options = GwenOptions(
          batch = true,
          reportDir = Some(new File(s"target/report/templates-dry-run/$level-level")),
          reportFormats = List(ReportFormat.html, ReportFormat.junit, ReportFormat.json),
          features = List(new File("features/sample/templates")),
          dryRun = true
        )
          
        val launcher = new GwenLauncher(new TemplatesInterpreter())
        launcher.run(options, None) match {
          case Passed(_) => // excellent :)
          case Failed(_, error) => error.printStackTrace(); fail(error.getMessage)
          case _ => fail("evaluation expected but got noop")
        }
      }
    }
  }
  
}
