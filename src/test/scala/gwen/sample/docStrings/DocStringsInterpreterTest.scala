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
package gwen.sample.docStrings

import gwen.dsl.Failed
import gwen.dsl.Passed
import gwen.eval.GwenOptions
import java.io.File

import org.scalatest.FlatSpec
import gwen.eval.GwenLauncher
import gwen.report.ReportFormat
import gwen.eval.EnvContext
import gwen.eval.GwenInterpreter
import gwen.eval.GwenApp
import gwen.eval.support.DefaultEngineSupport

class DocStringsEnvContext(val options: GwenOptions)
  extends EnvContext(options) {
  override def dsl: List[String] = Nil
}

trait DocStringsEvalEngine extends DefaultEngineSupport[DocStringsEnvContext] {
  override def init(options: GwenOptions): DocStringsEnvContext = new DocStringsEnvContext(options)
}

class DocStringsInterpreter
  extends GwenInterpreter[DocStringsEnvContext]
  with DocStringsEvalEngine

object DocStringsInterpreter
  extends GwenApp(new DocStringsInterpreter)

class DocStringsInterpreterTest extends FlatSpec {
  
  "DDocStrings" should "evaluate without error" in {
    
    val options = GwenOptions(
      batch = true,
      reportDir = Some(new File("target/report/docStrings")),
      reportFormats = List(ReportFormat.html, ReportFormat.junit, ReportFormat.json),
      features = List(new File("features/sample/docStrings"))
    )
      
    val launcher = new GwenLauncher(new DocStringsInterpreter())
    launcher.run(options, None) match {
      case Passed(_) => // excellent :)
      case Failed(_, error) => error.printStackTrace(); fail(error.getMessage)
      case _ => fail("evaluation expected but got noop")
    }
  }
  
  "DocStrings" should "pass --dry-run test" in {
    
    val options = GwenOptions(
      batch = true,
      reportDir = Some(new File("target/report/docStrings-dry-run")),
      reportFormats = List(ReportFormat.html, ReportFormat.junit, ReportFormat.json),
      features = List(new File("features/sample/docStrings")),
      dryRun = true
    )
      
    val launcher = new GwenLauncher(new DocStringsInterpreter())
    launcher.run(options, None) match {
      case Passed(_) => // excellent :)
      case Failed(_, error) => error.printStackTrace(); fail(error.getMessage)
      case _ => fail("evaluation expected but got noop")
    }
  }
  
}
