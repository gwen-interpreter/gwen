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

package gwen.eval

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import gwen.dsl._
import gwen.errors.UndefinedStepException
import gwen.eval.support.DefaultEngineSupport

class TestEnvContext(val options: GwenOptions, val scopes: ScopedDataStack) extends EnvContext(options, scopes)
class TestEvalEngine extends DefaultEngineSupport[TestEnvContext] {
  override def init(options: GwenOptions, scopes: ScopedDataStack): TestEnvContext = new TestEnvContext(options, scopes)
}
  
class EvalEngineTest extends FlatSpec with Matchers {

  object Scenario {
    def apply(tags: List[Tag], name: String, description: List[String], background: Option[Background], steps: List[Step]): Scenario =
      new Scenario(tags.distinct, name, description, background, steps, isOutline = false, Nil, None)
  }

  val engine = new TestEvalEngine
  
  "Unsupported step" should "fail with UnsupportedStepException" in {
    val env = engine.init(new GwenOptions(), new ScopedDataStack())
    intercept[UndefinedStepException] {
      engine.evaluate(Step(StepKeyword.Given, " I am unsupported"), env)
    }
  }
  
  "Step that fails interpolation" should "not be evaluated" in {
    val env = engine.init(new GwenOptions(), new ScopedDataStack())
    var step = Step(StepKeyword.Given, """x is "${y}"""")
    step = engine.evaluateStep(step, env)
    step.evalStatus.status should be (StatusKeyword.Failed)
    env.scopes.getOpt("x") should be (None)
    step.stepDef should be (None)
  }
  
  "Step that passes interpolation" should "should be evaluated" in {
    val env = engine.init(new GwenOptions(), new ScopedDataStack())
    env.scopes.set("y", "1")
    var step = Step(StepKeyword.Given, """x is "${y}"""")
    step = engine.evaluateStep(step, env)
    step.evalStatus.status should be (StatusKeyword.Passed)
    env.scopes.get("x") should be ("1")
    step.stepDef should be (None)
  }
  
  "Step that is a stepdef" should "should be evaluated" in {
    val env = engine.init(new GwenOptions(), new ScopedDataStack())
    val stepDef = Scenario(List[Tag]("@StepDef"), "I assign x, y, and z", Nil, None, List(
        Step(StepKeyword.Given, """x is "1""""),
        Step(StepKeyword.When, """y is "2""""),
        Step(StepKeyword.Then, """z is "3"""")))
    env.addStepDef(stepDef)
    env.scopes.set("y", "1")
    var step = Step(StepKeyword.Given, "I assign x, y, and z")
    step = engine.evaluateStep(step, env)
    step.evalStatus.status should be (StatusKeyword.Passed)
    env.scopes.get("x") should be ("1")
    env.scopes.get("y") should be ("2")
    env.scopes.get("z") should be ("3")
    step.stepDef should not be (None)
  }
  
}