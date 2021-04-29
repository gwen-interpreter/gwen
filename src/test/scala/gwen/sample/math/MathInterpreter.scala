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

package gwen.sample.math

import gwen._
import gwen.eval._
import gwen.model.gherkin.Step
import gwen.model.state.ScopedData

import scala.io.Source

class MathService {
  def plus(x: Int, y: Int): Int = x + y
}

class MathEnv extends EvalEnvironment {
  def vars: ScopedData = addScope("vars")
}

class MathEvalContext(options: GwenOptions, val mathService: MathService) 
  extends EvalContext(options, new MathEnv()) {
  override def dsl: List[String] = 
    Source.fromInputStream(getClass.getResourceAsStream("/math.dsl")).getLines().toList ++ super.dsl
}

trait MathEvalEngine extends EvalEngine[MathEvalContext] {
 
  override def init(options: GwenOptions, envOpt: Option[EvalEnvironment] = None): MathEvalContext =
    new MathEvalContext(options, new MathService())
 
  override def evaluate(step: Step, ctx: MathEvalContext): Unit = ctx.withEnv { env =>
    val vars = env.asInstanceOf[MathEnv].vars
    step.expression match {
      case r"""([a-z])$x = (\d+)$value""" =>
        vars.set(x, value)
      case r"([a-z])$x = ([a-z])$y" =>
        vars.set(x, vars.get(y))
      case r"""z = ([a-z])$x \+ ([a-z])$y""" =>
        val xvalue = vars.get(x).toInt
        val yvalue = vars.get(y).toInt
        ctx.evaluate(vars.set("z", "0")) {
          logger.info(s"evaluating z = $xvalue + $yvalue")
          val zresult = ctx.mathService.plus(xvalue, yvalue)
          vars.set("z", zresult.toString)
        }
      case r"""([a-z])$x == (\d+)$value""" =>
        val xvalue = vars.get(x).toInt
        ctx.perform {
          assert (xvalue.toInt == value.toInt)
        }
      case _ =>
        super.evaluate(step, ctx)
    }
  }
}

class MathInterpreter 
  extends GwenInterpreter[MathEvalContext]
  with MathEvalEngine

object MathInterpreter 
  extends Gwen(new MathInterpreter)
