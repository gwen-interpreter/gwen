/*
 * Copyright 2014-2015 Branko Juric, Brady Wood
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

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import gwen.dsl.Step
import gwen.dsl.StepKeyword
import gwen.errors.UndefinedStepException

class TestEnvContext(val options: GwenOptions, val scopes: ScopedDataStack) extends EnvContext(options, scopes)
class TestEvalEngine extends EvalEngine[TestEnvContext] {
  override def init(options: GwenOptions, scopes: ScopedDataStack): TestEnvContext = new TestEnvContext(options, scopes)
}
  
class EvalEngineTest extends FlatSpec with Matchers {

  val engine = new TestEvalEngine
  val env = engine.init(new GwenOptions(), new ScopedDataStack())
  
  "Unsupported step" should "fail with UnsupportedStepException" in {
    intercept[UndefinedStepException] {
      engine.evaluate(Step(StepKeyword.Given, " I am unsupported"), env)
    }
  }
  
}