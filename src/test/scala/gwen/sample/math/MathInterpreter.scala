/*
 * Copyright 2014 Branko Juric, Brady Wood
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

import gwen.eval.EnvContext
import gwen.dsl.Step
import gwen.eval.EvalEngine
import gwen.eval.GwenOptions
import gwen.eval.GwenInterpreter
import gwen.eval.GwenApp
import gwen.eval.EnvContext
import gwen.eval.DataScopes

class MathService {
  def plus(x: Int, y: Int) = x + y
}

class MathEnvContext(val mathService: MathService, val dataScopes: DataScopes) 
  extends EnvContext(dataScopes) {
  def vars = dataScope("vars")
}

trait MathEvalEngine extends EvalEngine[MathEnvContext] {
 
  override def init(options: GwenOptions, dataScopes: DataScopes): MathEnvContext =
    new MathEnvContext(new MathService(), dataScopes)
 
  override def evaluate(step: Step, env: MathEnvContext) {
    val vars = env.vars
    step.expression match {
      case r"([a-z])$x = (\d+)$value" =>
        vars.set(x, value)
      case r"([a-z])$x = ([a-z])$y" =>
        vars.set(x, vars.get(y))
      case r"z = ([a-z])$x \+ ([a-z])$y" =>
        val xvalue = vars.get(x).toInt
        val yvalue = vars.get(y).toInt
        logger.info(s"evaluating z = $xvalue + $yvalue")
        val zresult = env.mathService.plus(xvalue, yvalue)
        vars.set("z", zresult.toString)
      case r"([a-z])$x == (\d+)$value" =>
        assert (vars.get(x).toInt == value.toInt)
      case _ =>
        super.evaluate(step, env)
    }
  }
}

class MathInterpreter 
  extends GwenInterpreter[MathEnvContext]
  with MathEvalEngine

object MathInterpreter 
  extends GwenApp(new MathInterpreter)
