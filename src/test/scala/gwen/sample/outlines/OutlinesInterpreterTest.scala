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
import gwen.eval.GwenOptions
import java.io.File
import org.scalatest.FlatSpec
import gwen.eval.GwenLauncher
import gwen.report.ReportFormat
import gwen.eval.ScopedDataStack
import gwen.eval.EnvContext
import gwen.eval.EvalEngine
import gwen.dsl.Step
import gwen.Predefs.RegexContext
import gwen.eval.GwenInterpreter
import gwen.eval.GwenApp

class OutlinesEnvContext(val options: GwenOptions, val scopes: ScopedDataStack)
  extends EnvContext(options, scopes) {
  override def dsl: List[String] = Nil
}

trait OutlinesEvalEngine extends EvalEngine[OutlinesEnvContext] {
  override def init(options: GwenOptions, scopes: ScopedDataStack): OutlinesEnvContext = new OutlinesEnvContext(options, scopes)
  override def evaluate(step: Step, env: OutlinesEnvContext) {
    step.expression match {
      case r"""(.+?)$name is "(.+?)"$$$value""" =>
        env.scopes.set(name, value)
      case r"""(.+?)$name should be "(.+?)"$$$value""" =>
        val actual = env.scopes.get(name)
        env.execute(assert(actual == value, s"Expected '$value' but got '$actual'"))
      case _ =>
        super.evaluate(step, env)
    }
  }
}

class OutlinesInterpreter
  extends GwenInterpreter[OutlinesEnvContext]
  with OutlinesEvalEngine

object OutlinesInterpreter
  extends GwenApp(new OutlinesInterpreter)

class OutlinesInterpreterTest extends FlatSpec {
  
  "Scenario outlines" should "evaluate without error" in {
    
    val options = GwenOptions(
      batch = true,
      reportDir = Some(new File("target/report/outlines")),
      reportFormats = List(ReportFormat.html, ReportFormat.junit),
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
      reportFormats = List(ReportFormat.html, ReportFormat.junit),
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
