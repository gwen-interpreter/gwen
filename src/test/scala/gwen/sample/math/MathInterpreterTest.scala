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

import gwen.dsl.Failed
import gwen.dsl.Passed
import gwen.eval.GwenOptions
import java.io.File
import org.scalatest.FlatSpec
import gwen.eval.GwenLauncher
import gwen.dsl.Step
import gwen.dsl.StepKeyword
import gwen.report.ReportFormat

class MathInterpreterTest extends FlatSpec {
  
  "math features" should "evaluate" in {
    
    val options = GwenOptions(
      batch = true,
      reportDir = Some(new File("target/report/math")), 
      reportFormats = List(ReportFormat.html, ReportFormat.junit, ReportFormat.json),
      features = List(new File("features/sample/math"))
    )
      
    val launcher = new GwenLauncher(new MathInterpreter())
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
      
    val launcher = new GwenLauncher(new MathInterpreter())
    launcher.run(options, None) match {
      case Passed(_) => // excellent :)
      case Failed(_, error) => error.printStackTrace(); fail(error.getMessage)
      case _ => fail("evaluation expected but got noop")
    }
  }
  
  "math.dsl" should "pass --dry-run test" in {
    
    val options = new GwenOptions(dryRun = true)
    
    val env = new MathEnvContext(new MathService(), options)
    env.scopes.addScope("vars").set("y", "1")
        
    val interpreter = new MathInterpreter
    env.dsl map { dsl =>
      dsl.replace("<integer>", "1")
    } foreach { dsl => 
      StepKeyword.values.map(_.toString) foreach { keyword =>
        interpreter.evaluate(Step(keyword, dsl), env)
      }
    }
  }
}
