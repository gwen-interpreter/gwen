/*
 * Copyright 2018-2021 Branko Juric, Brady Wood
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
package gwen.core.sample.xml

import gwen.DefaultGwenInterpreter
import gwen.core.GwenOptions
import gwen.core.report.ReportFormat
import gwen.core.status._


import java.io.File
import org.scalatest.flatspec.AnyFlatSpec

class XmlTest extends AnyFlatSpec {

  val interpreter = DefaultGwenInterpreter
  
  "XML tests" should "pass" in {
    
    val options = GwenOptions(
      batch = true,
      reportDir = Some(new File("target/report/xml")),
      reportFormats = List(ReportFormat.html, ReportFormat.junit, ReportFormat.json),
      features = List(new File("features/sample/xml"))
    )
      
    interpreter.run(options, None) match {
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
      
    interpreter.run(options, None) match {
      case Passed(_) => // excellent :)
      case Failed(_, error) => error.printStackTrace(); fail(error.getMessage)
      case _ => fail("evaluation expected but got noop")
    }
  }
  
}
