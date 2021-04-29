/*
 * Copyright 2015 Branko Juric, Brady Wood
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

package gwen.eval

import gwen.GwenOptions
import gwen.TestModel
import gwen.eval.EvalContext
import gwen.eval.EvalEnvironment
import gwen.model.StepKeyword

import gwen.Settings

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import java.io.IOException

class TestEvalContext() extends EvalContext(GwenOptions(), new EvalEnvironment())
class TestEvalEngine extends DefaultEngine[TestEvalContext] {
  def init(options: GwenOptions, envOpt: Option[EvalEnvironment] = None): TestEvalContext = new TestEvalContext()
}

class DefaultEngineSupportTest extends FlatSpec with Matchers with TestModel {

  val engine = new TestEvalEngine
  val ctx: TestEvalContext = engine.init(new GwenOptions())
  
  "Set attribute binding step" should "be successful" in {
    engine.evaluate(Step(StepKeyword.Given.toString, """my name is "Gwen""""), ctx)
    ctx.withEnv { env =>
      env.topScope.get("my name") should be ("Gwen")
    }
  }
  
  "Set global setting step" should "be successful" in {
    engine.evaluate(Step(StepKeyword.Given.toString, """my gwen.username setting is "Gwen""""), ctx)
    Settings.get("gwen.username") should be ("Gwen")
  }
  
  "Execute system process 'hostname'" should "be successful" in {
    engine.evaluate(Step(StepKeyword.Given.toString, """I execute system process "hostname""""), ctx)
  }
  
  "Execute system process 'undefined'" should "fail with IOException" in {
    intercept[IOException] {
      engine.evaluate(Step(StepKeyword.Given.toString, """I execute system process "undefined""""), ctx)
    }
  }
  
}