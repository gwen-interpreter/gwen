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
import gwen.core.node.gherkin.Step

import scala.util.Try

class RepeatIfDefined[T <: EvalContext](doStep: String, operation: String, name: String, negate: Boolean, engine: EvalEngine[T])
    extends Repeat(doStep, operation, s"$name is ${if (negate) "not " else ""}defined", engine) {

  override def evaluteCondition(step: Step, ctx: T): Boolean = {
      val result = Try(ctx.getBinding(name))
      if (!negate) result.isSuccess else result.isFailure
  }

}


