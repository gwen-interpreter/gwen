/*
 * Copyright 2023-2024 Branko Juric, Brady Wood
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

package gwen.core.eval.action.composite

import gwen.core.eval.EvalContext
import gwen.core.eval.EvalEngine
import gwen.core.eval.binding.JSBinding
import gwen.core.eval.support.BooleanCondition
import gwen.core.node.gherkin.Step

class RepeatJS[T <: EvalContext](doStep: String, operation: String, condition: String, engine: EvalEngine[T])
    extends Repeat(doStep, operation, condition, engine) {

  override def evaluteCondition(step: Step, ctx: T): Boolean = {
    val bCond = BooleanCondition(condition, false, step.timeoutOpt.getOrElse(ctx.defaultWait).toSeconds, ctx)
    ctx.evaluate(true) {
      bCond.evaluate()
    }
  }

}


