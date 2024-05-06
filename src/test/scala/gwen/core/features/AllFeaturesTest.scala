/*
 * Copyright 2017-2021 Branko Juric, Brady Wood
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
import gwen.core.Settings
import gwen.core.BaseTest
import gwen.core.GwenOptions
import gwen.core.report.ReportFormat
import gwen.core.status._

import org.scalatest.prop.TableDrivenPropertyChecks.forAll

import java.io.File

class AllFeaturesTest extends BaseTest {

  val interpreter = GwenInterpreter()
  
  forAll (levels) { level =>

    s"All features using $level level state" should "evaluate without error" in {
      
      val options = GwenOptions(
        batch = true,
        reportDir = Some(new File(s"target/reports/all-features/$level-level")),
        reportFormats = List(ReportFormat.html, ReportFormat.junit, ReportFormat.json),
        features = List(new File("src/test/features")),
        settingsFiles = List(new File("src/test/resources/gwen/bindings/bindings.conf"))
      )
        
      Settings.init(options.settingsFiles*)
      interpreter.run(options, None) match {
        case _: Passed => // excellent :)
        case Failed(_, error) => error.printStackTrace(); fail(error.getMessage)
        case _ => fail("evaluation expected but got No-op")
      }
    }

  }

  forAll (levels) { level =>
  
    s"All features using $level level state" should "pass parallel --dry-run test" in {
      
      val options = GwenOptions(
        batch = true,
        reportDir = Some(new File(s"target/reports/all-features-dry-run/$level-level")),
        reportFormats = List(ReportFormat.html, ReportFormat.junit, ReportFormat.json),
        features = List(new File("src/test/features")),
        dryRun = true,
        parallel = true,
        settingsFiles = List(new File("src/test/resources/gwen/bindings/bindings.conf"))
      )
        
      Settings.init(options.settingsFiles*)
      interpreter.run(options, None) match {
        case _: Passed => // excellent :)
        case Failed(_, error) => error.printStackTrace(); fail(error.getMessage)
        case _ => fail("evaluation expected but got No-op")
      }
    }

  }
  
}
