package gwen.sample.math

import gwen.dsl.Failed
import gwen.dsl.Passed
import gwen.eval.GwenOptions
import java.io.File
import org.scalatest.FlatSpec

class MathInterpreterTest extends FlatSpec {
  
  "math features" should "evaluate" in {
    
    val options = GwenOptions(
      batch = true,
      reportDir = Some(new File("target/report")), 
      paths = List(new File("features/sample/math"))
    )
      
    val intepreter = new MathInterpreter()
    intepreter.execute(options, None) match {
      case Passed(_) => // excellent :)
      case Failed(_, error) => error.printStackTrace(); fail(error.getMessage())
      case _ => fail("evaluation expected but got noop")
    }
  }
}
