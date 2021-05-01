/*
 * Copyright 2014-2020 Branko Juric, Brady Wood
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

import gwen._
import gwen.model._

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatestplus.mockito.MockitoSugar

import java.io.IOException

class TestEvalContext() extends EvalContext(GwenOptions(), new EvalEnvironment())
class TestEvalEngine extends EvalEngine[TestEvalContext] {
  def init(options: GwenOptions, envOpt: Option[EvalEnvironment] = None): TestEvalContext = new TestEvalContext()
}

class EvalEngineTest extends FlatSpec with Matchers with MockitoSugar with TestModel {

  val engine = new TestEvalEngine
  val ctx = engine.init(new GwenOptions())

  val parent = mock[Identifiable]
  
  "Unsupported step" should "fail with UnsupportedStepException" in {
    intercept[Errors.UndefinedStepException] {
      engine.evaluate(Step(StepKeyword.Given.toString, " I am unsupported"), ctx)
    }
  }
  
  "Step that fails interpolation" should "not be evaluated" in {
    ctx.withEnv { env =>
      var step = Step(StepKeyword.Given.toString, """x is "${y}"""")
      step = engine.evaluateStep(parent, step, ctx)
      step.evalStatus.status should be (StatusKeyword.Failed)
      env.scopes.getOpt("x") should be (None)
      step.stepDef should be (None)
    }
  }
  
  "Step that passes interpolation" should "should be evaluated" in {
    ctx.withEnv { env =>
      env.scopes.set("y", "1")
      var step = Step(StepKeyword.Given.toString, """x is "${y}"""")
      step = engine.evaluateStep(parent, step, ctx)
      step.evalStatus.status should be (StatusKeyword.Passed)
      env.scopes.get("x") should be ("1")
      step.stepDef should be (None)
    }
  }
  
  "Step that is a stepdef" should "be evaluated" in {
    val ctx = engine.init(new GwenOptions())
    ctx.withEnv { env =>
      val stepDef = Scenario(List[Tag](Tag("@StepDef"), Tag("@Action")), "I assign x, y, and z", Nil, None, List(
          Step(StepKeyword.Given.toString, """x is "1""""),
          Step(StepKeyword.And.toString, """y is "2""""),
          Step(StepKeyword.And.toString, """z is "3""""),
          Step(StepKeyword.When.toString, """I capture x as a"""),
          Step(StepKeyword.Then.toString, """a should be "1"""")))
      env.addStepDef(stepDef)
      env.scopes.set("y", "1")
      var step = Step(StepKeyword.When.toString, "I assign x, y, and z")
      step = engine.evaluateStep(parent, step, ctx)
      step.evalStatus.status should be (StatusKeyword.Passed)
      env.scopes.get("x") should be ("1")
      env.scopes.get("y") should be ("2")
      env.scopes.get("z") should be ("3")
      env.scopes.get("a") should be ("1")
      step.stepDef should not be (None)
    }
  }

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