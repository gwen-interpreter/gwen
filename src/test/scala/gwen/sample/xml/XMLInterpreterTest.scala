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
package gwen.sample.xml

import gwen.GwenOptions
import gwen.eval.DefaultEngine
import gwen.eval.EvalContext
import gwen.eval.EvalEnvironment
import gwen.eval.GwenInterpreter
import gwen.eval.GwenLauncher
import gwen.model.Failed
import gwen.model.Passed
import gwen.report.ReportFormat

import org.scalatest.FlatSpec

import java.io.File

class XMLEvalContext
  extends EvalContext(GwenOptions(), new EvalEnvironment()) {
  override def dsl: List[String] = Nil
}

trait XMLEvalEngine extends DefaultEngine[XMLEvalContext] {
  override def init(options: GwenOptions, envOpt: Option[EvalEnvironment] = None): XMLEvalContext = new XMLEvalContext()
}

class XMLInterpreter
  extends GwenInterpreter[XMLEvalContext]
  with XMLEvalEngine

class XmlInterpreterTest extends FlatSpec {
  
  "XML tests" should "pass" in {
    
    val options = GwenOptions(
      batch = true,
      reportDir = Some(new File("target/report/xml")),
      reportFormats = List(ReportFormat.html, ReportFormat.junit, ReportFormat.json),
      features = List(new File("features/sample/xml"))
    )
      
    val launcher = new GwenLauncher(new XMLInterpreter())
    launcher.run(options, None) match {
      case Passed(_) => // excellent :)
      case Failed(_, error) => error.printStackTrace(); fail(error.getMessage)
      case _ => fail("evaluation expected but got noop")
    }
  }
  
  "XML tests" should "pass --dry-run" in {
    
    val options = GwenOptions(
      batch = true,
      reportDir = Some(new File("target/report/xml-dry-run")),
      reportFormats = List(ReportFormat.html, ReportFormat.junit, ReportFormat.json),
      features = List(new File("features/sample/xml")),
      dryRun = true
    )
      
    val launcher = new GwenLauncher(new XMLInterpreter())
    launcher.run(options, None) match {
      case Passed(_) => // excellent :)
      case Failed(_, error) => error.printStackTrace(); fail(error.getMessage)
      case _ => fail("evaluation expected but got noop")
    }
  }
  
}
