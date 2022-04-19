/*
 * Copyright 2016-2021 Branko Juric, Brady Wood
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
package gwen.core.features

import gwen.GwenInterpreter
import gwen.core.BaseTest
import gwen.core.GwenOptions
import gwen.core.Settings
import gwen.core.report.ReportFormat
import gwen.core.status._

import java.io.File

class AdhocFeatureTest extends BaseTest {

  val feature = "src/test/features/similarity"

  val interpreter = GwenInterpreter()

  s"Feature should" should "execute" in {
    val options = GwenOptions(
      batch = true,
      reportDir = Some(new File(s"target/reports")), 
      reportFormats = List(ReportFormat.html, ReportFormat.junit, ReportFormat.json),
      features = List(new File(feature)),
      settingsFiles = List(new File("src/test/resources/gwen.conf"))
    )
        
    Settings.init(options.settingsFiles*)
    interpreter.run(options, None) match {
      case _: Passed => // excellent :)
      case Failed(_, error) => error.printStackTrace(); fail(error.getMessage)
      case _ => fail("evaluation expected but got noop")
    }
  }
  
}
