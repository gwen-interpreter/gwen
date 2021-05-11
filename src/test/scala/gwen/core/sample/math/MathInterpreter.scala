/*
 * Copyright 2014-2021 Branko Juric, Brady Wood
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

package gwen.core.sample.math

import gwen.core._
import gwen.core.engine._
import gwen.core.engine.lambda.UnitStep
import gwen.core.engine.lambda.unit.BindAttribute
import gwen.core.model.gherkin.Step

import scala.io.Source
import gwen.core.model.Identifiable

class MathService {
  def plus(x: Int, y: Int): Int = x + y
}

class MathContext(options: GwenOptions, val mathService: MathService) 
  extends EvalContext(options, new EvalEnvironment()) {
  override def dsl: List[String] = 
    Source.fromInputStream(getClass.getResourceAsStream("/math.dsl")).getLines().toList ++ super.dsl
}

class Sum(x: Int, y: Int, engine: MathEngine, ctx: MathContext) extends UnitStep[MathContext](engine, ctx) {
  override def apply(parent: Identifiable, step: Step): Unit = {
    ctx.evaluate(env.scopes.set("z", "0")) {
      logger.info(s"evaluating z = $x + $y")
      val zresult = ctx.mathService.plus(x, y)
      env.scopes.set("z", zresult.toString)
    }
  }
}

class Compare(actual: Int, expected: Int, engine: MathEngine, ctx: MathContext) extends UnitStep[MathContext](engine, ctx) {
  override def apply(parent: Identifiable, step: Step): Unit = {
    ctx.perform {
      assert (actual == expected)
    }
  }
}

class MathEngine extends EvalEngine[MathContext] {
 
  override def init(options: GwenOptions, envOpt: Option[EvalEnvironment] = None): MathContext =
    new MathContext(options, new MathService())
 
  override def translate(parent: Identifiable, step: Step, env: EvalEnvironment, ctx: MathContext): UnitStep[MathContext] = {
    step.expression match {
      case r"""([a-z])$x = (\d+)$value""" =>
        new BindAttribute(x, value, this, ctx)
      case r"([a-z])$x = ([a-z])$y" =>
        new BindAttribute(x, env.scopes.get(y), this, ctx)
      case r"""z = ([a-z])$x \+ ([a-z])$y""" =>
        new Sum(env.scopes.get(x).toInt, env.scopes.get(y).toInt, this, ctx)
      case r"""([a-z])$x == (\d+)$value""" =>
        new Compare(env.scopes.get(x).toInt, value.toInt, this, ctx)
      case _ =>
        super.translate(parent, step, env, ctx)
    }
  }

}
