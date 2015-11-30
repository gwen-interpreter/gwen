package gwen.sample.math

import gwen.dsl.Failed
import gwen.dsl.Passed
import gwen.eval.GwenOptions
import java.io.File
import org.scalatest.FlatSpec
import gwen.eval.GwenLauncher
import gwen.eval.ScopedDataStack
import gwen.dsl.Step
import gwen.dsl.StepKeyword
import gwen.report.ReportFormat

class MathInterpreterTest extends FlatSpec {
  
  "math features" should "evaluate" in {
    
    val options = GwenOptions(
      batch = true,
      reportDir = Some(new File("target/report")), 
      reportFormats = List(ReportFormat.html, ReportFormat.junit),
      features = List(new File("features/sample/math"))
    )
      
    val launcher = new GwenLauncher(new MathInterpreter())
    launcher.run(options, None) match {
      case Passed(_) => // excellent :)
      case Failed(_, error) => error.printStackTrace(); fail(error.getMessage())
      case _ => fail("evaluation expected but got noop")
    }
  }
  
  "math features" should "pass --dry-run test" in {
    
    val options = GwenOptions(
      batch = true,
      reportDir = Some(new File("target/report-dry-run")), 
      features = List(new File("features/sample/math")),
      dryRun = true
    )
      
    val launcher = new GwenLauncher(new MathInterpreter())
    launcher.run(options, None) match {
      case Passed(_) => // excellent :)
      case Failed(_, error) => error.printStackTrace(); fail(error.getMessage())
      case _ => fail("evaluation expected but got noop")
    }
  }
  
  "math.dsl" should "pass --dry-run test" in {
    
    val options = new GwenOptions(dryRun = true);
    
    val env = new MathEnvContext(new MathService(), options, new ScopedDataStack())
    env.scopes.addScope("vars").set("y", "1")
        
    val interpreter = new MathInterpreter
    env.dsl map { dsl =>
      dsl.replace("<integer>", "1")
    } foreach { dsl => 
      StepKeyword.values foreach { keyword =>
        interpreter.evaluate(Step(keyword, dsl), env)
      }
    }
  }
}
