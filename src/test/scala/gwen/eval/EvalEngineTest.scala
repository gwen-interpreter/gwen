package gwen.eval

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import gwen.dsl.Step
import gwen.dsl.StepKeyword
import java.io.IOException

class TestEnvContext(val scopes: ScopedDataStack) extends EnvContext(scopes)
class TestEvalEngine extends EvalEngine[TestEnvContext] {
  override def init(options: GwenOptions, scopes: ScopedDataStack): TestEnvContext = new TestEnvContext(scopes)
}
  
class EvalEngineTest extends FlatSpec with Matchers {

  val engine = new TestEvalEngine
  val env = engine.init(new GwenOptions(), new ScopedDataStack())
  
  "Unsupported step" should "fail with UnsupportedStepException" in {
    intercept[UnsupportedStepException] {
      engine.evaluate(Step(StepKeyword.Given, " I am unsupported"), env)
    }
  }
  
  "Execute system process 'hostname'" should "be successful" in {
    engine.evaluate(Step(StepKeyword.Given, """I execute system process "hostname""""), env)
  }
  
  "Execute system process 'undefined'" should "fail with IOException" in {
    intercept[IOException] {
    	engine.evaluate(Step(StepKeyword.Given, """I execute system process "undefined""""), env)
    }
  }
  
}