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

package gwen.eval.support

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import gwen.eval.EnvContext
import gwen.eval.GwenOptions
import gwen.dsl.Step
import java.io.IOException
import gwen.dsl.StepKeyword
import gwen.Settings

class TestEnvContext(val options: GwenOptions) extends EnvContext(options)
class TestEvalEngine extends DefaultEngineSupport[TestEnvContext] {
  def init(options: GwenOptions): TestEnvContext = new TestEnvContext(options)
}

class DefaultEngineSupportTest extends FlatSpec with Matchers {

  val engine = new TestEvalEngine
  val env: TestEnvContext = engine.init(new GwenOptions())
  
  "Set attribute binding step" should "be successful" in {
    engine.evaluate(Step(StepKeyword.Given.toString, """my name is "Gwen""""), env)
    env.topScope.get("my name") should be ("Gwen")
  }
  
  "Set global setting step" should "be successful" in {
    engine.evaluate(Step(StepKeyword.Given.toString, """my gwen.username setting is "Gwen""""), env)
    Settings.get("gwen.username") should be ("Gwen")
  }
  
  "Execute system process 'hostname'" should "be successful" in {
    engine.evaluate(Step(StepKeyword.Given.toString, """I execute system process "hostname""""), env)
  }
  
  "Execute system process 'undefined'" should "fail with IOException" in {
    intercept[IOException] {
      engine.evaluate(Step(StepKeyword.Given.toString, """I execute system process "undefined""""), env)
    }
  }
  
}