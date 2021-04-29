/*
 * Copyright 2016-2017 Branko Juric, Brady Wood
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
package gwen.sample.bindings

import gwen.BaseTest
import gwen.Gwen
import gwen.GwenOptions
import gwen.eval.DefaultEngine
import gwen.eval.EvalContext
import gwen.eval.EvalEnvironment
import gwen.eval.GwenInterpreter
import gwen.eval.GwenLauncher
import gwen.model.Failed
import gwen.model.Passed
import gwen.report.ReportFormat

import org.scalatest.prop.TableDrivenPropertyChecks.forAll

import java.io.File

class BindingsEvalContext 
  extends EvalContext(GwenOptions(), new EvalEnvironment()) {
  override def dsl: List[String] = Nil
}

trait BindingsEvalEngine extends DefaultEngine[BindingsEvalContext] {
  override def init(options: GwenOptions, envOpt: Option[EvalEnvironment] = None): BindingsEvalContext = new BindingsEvalContext()
}

class BindingsInterpreter 
  extends GwenInterpreter[BindingsEvalContext]
  with BindingsEvalEngine

object BindingsInterpreter 
  extends Gwen(new BindingsInterpreter)

class BindingsInterpreterTest extends BaseTest {
  
  forAll (levels) { level =>
    s"binding features using $level level state" should "evaluate without error" in {
      withSetting("gwen.state.level", level) {
        val options = GwenOptions(
          batch = true,
          reportDir = Some(new File(s"target/report/bindings/$level-level")), 
          reportFormats = List(ReportFormat.html, ReportFormat.junit, ReportFormat.json),
          features = List(new File("features/sample/bindings")),
          properties = List(new File("src/test/resources/gwen/bindings/bindings.properties"))
        )
          
        val launcher = new GwenLauncher(new BindingsInterpreter())
        launcher.run(options, None) match {
          case Passed(_) => // excellent :)
          case Failed(_, error) => error.printStackTrace(); fail(error.getMessage)
          case _ => fail("evaluation expected but got noop")
        }
      }
    }
  }
  
  forAll (levels) { level =>
    s"binding features using $level level state" should "pass --dry-run test" in {  
      withSetting("gwen.state.level", level) {
        val options = GwenOptions(
          batch = true,
          reportDir = Some(new File(s"target/report/bindings-dry-run/$level-level")), 
          reportFormats = List(ReportFormat.html, ReportFormat.junit, ReportFormat.json),
          features = List(new File("features/sample/bindings")),
          dryRun = true,
          properties = List(new File("src/test/resources/gwen/bindings/bindings.properties"))
        )
          
        val launcher = new GwenLauncher(new BindingsInterpreter())
        launcher.run(options, None) match {
          case Passed(_) => // excellent :)
          case Failed(_, error) => error.printStackTrace(); fail(error.getMessage)
          case _ => fail("evaluation expected but got noop")
        }
      }
    }
  }
  
}
