/*
 * Copyright 2014-2017 Branko Juric, Brady Wood
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

package gwen.sample.math

import gwen.GwenInterpreter
import gwen.GwenOptions
import gwen.TestModel
import gwen.eval.GwenLauncher
import gwen.model.Failed
import gwen.model.Passed
import gwen.model.StepKeyword
import gwen.report.ReportFormat

import org.scalatest.FlatSpec

import java.io.File

class MathInterpreterTest extends FlatSpec with TestModel {
  
  val engine = new MathEngine()
  val interpreter = new GwenInterpreter(engine)
  val launcher = new GwenLauncher(interpreter)

  "math features" should "evaluate" in {
    
    val options = GwenOptions(
      batch = true,
      reportDir = Some(new File("target/report/math")), 
      reportFormats = List(ReportFormat.html, ReportFormat.junit, ReportFormat.json),
      features = List(new File("features/sample/math"))
    )
      
    launcher.run(options, None) match {
      case Passed(_) => // excellent :)
      case Failed(_, error) => error.printStackTrace(); fail(error.getMessage)
      case _ => fail("evaluation expected but got noop")
    }
  }
  
  "math features" should "pass --dry-run test" in {
    
    val options = GwenOptions(
      batch = true,
      reportDir = Some(new File("target/report/math-dry-run")), 
      reportFormats = List(ReportFormat.html, ReportFormat.junit, ReportFormat.json),
      features = List(new File("features/sample/math")),
      dryRun = true
    )
      
    launcher.run(options, None) match {
      case Passed(_) => // excellent :)
      case Failed(_, error) => error.printStackTrace(); fail(error.getMessage)
      case _ => fail("evaluation expected but got noop")
    }
  }
  
  "math.dsl" should "pass --dry-run test" in {
    
    val options = new GwenOptions(dryRun = true)
    
    val ctx = engine.init(options)

    ctx.withEnv { env =>

      env.scopes.addScope("vars").set("y", "1")
          
      env.dsl map { dsl =>
        dsl.replace("<integer>", "1")
      } foreach { dsl => 
        StepKeyword.values.map(_.toString) foreach { keyword =>
          engine.evaluate(Step(keyword, dsl), ctx)
        }
      }

    }
    
  }
}
