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
package gwen.sample.outlines

import gwen.dsl.Failed
import gwen.dsl.Passed
import gwen.eval.EnvContext
import gwen.eval.GwenOptions
import gwen.eval.GwenInterpreter
import gwen.eval.GwenLauncher
import gwen.eval.support.DefaultEngineSupport
import gwen.report.ReportFormat

import org.scalatest.FlatSpec

import java.io.File

class OutlinesEnvContext(val options: GwenOptions)
  extends EnvContext(options) {
  override def dsl: List[String] = Nil
}

trait OutlinesEvalEngine extends DefaultEngineSupport[OutlinesEnvContext] {
  override def init(options: GwenOptions): OutlinesEnvContext = new OutlinesEnvContext(options)
}

class OutlinesInterpreter
  extends GwenInterpreter[OutlinesEnvContext]
  with OutlinesEvalEngine

class OutlinesInterpreterTest extends FlatSpec {
  
  "Scenario outlines" should "evaluate without error" in {
    
    val options = GwenOptions(
      batch = true,
      reportDir = Some(new File("target/report/outlines")),
      reportFormats = List(ReportFormat.html, ReportFormat.junit, ReportFormat.json),
      features = List(new File("features/sample/outlines"))
    )
      
    val launcher = new GwenLauncher(new OutlinesInterpreter())
    launcher.run(options, None) match {
      case Passed(_) => // excellent :)
      case Failed(_, error) => error.printStackTrace(); fail(error.getMessage)
      case _ => fail("evaluation expected but got noop")
    }
  }
  
  "Scenario outlines" should "pass --dry-run test" in {
    
    val options = GwenOptions(
      batch = true,
      reportDir = Some(new File("target/report/outlines-dry-run")),
      reportFormats = List(ReportFormat.html, ReportFormat.junit, ReportFormat.json),
      features = List(new File("features/sample/outlines")),
      dryRun = true
    )
      
    val launcher = new GwenLauncher(new OutlinesInterpreter())
    launcher.run(options, None) match {
      case Passed(_) => // excellent :)
      case Failed(_, error) => error.printStackTrace(); fail(error.getMessage)
      case _ => fail("evaluation expected but got noop")
    }
  }
  
}
