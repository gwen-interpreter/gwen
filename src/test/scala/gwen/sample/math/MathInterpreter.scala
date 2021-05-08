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

import scala.io.Source
import gwen.model.Identifiable

class MathService {
  def plus(x: Int, y: Int): Int = x + y
}

class MathContext(options: GwenOptions, val mathService: MathService) 
  extends EvalContext(options, new EvalEnvironment()) {
  override def dsl: List[String] = 
    Source.fromInputStream(getClass.getResourceAsStream("/math.dsl")).getLines().toList ++ super.dsl
}

class MathEngine extends MathDSLTranslator

class MathDSLTranslator extends EvalEngine[MathContext] {
 
  override def init(options: GwenOptions, envOpt: Option[EvalEnvironment] = None): MathContext =
    new MathContext(options, new MathService())
 
  override def translate(parent: Identifiable, step: Step, env: EvalEnvironment, ctx: MathContext): Step => Unit = {
    val vars = env.addScope("vars")
    step.expression match {
      case r"""([a-z])$x = (\d+)$value""" => step =>
        vars.set(x, value)
      case r"([a-z])$x = ([a-z])$y" => step =>
        vars.set(x, vars.get(y))
      case r"""z = ([a-z])$x \+ ([a-z])$y""" => step =>
        val xvalue = vars.get(x).toInt
        val yvalue = vars.get(y).toInt
        ctx.evaluate(vars.set("z", "0")) {
          logger.info(s"evaluating z = $xvalue + $yvalue")
          val zresult = ctx.mathService.plus(xvalue, yvalue)
          vars.set("z", zresult.toString)
        }
      case r"""([a-z])$x == (\d+)$value""" => step =>
        val xvalue = vars.get(x).toInt
        ctx.perform {
          assert (xvalue.toInt == value.toInt)
        }
      case _ =>
        super.translate(parent, step, env, ctx)
    }
  }

}
