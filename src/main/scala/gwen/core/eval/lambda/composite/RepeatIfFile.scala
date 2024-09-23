
/*
 * Copyright 2024 Branko Juric, Brady Wood
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

package gwen.core.eval.lambda.composite

import gwen.core.eval.EvalContext
import gwen.core.eval.EvalEngine
import gwen.core.eval.FileComparisonOperator
import gwen.core.eval.binding.JSBinding
import gwen.core.eval.support.FileCondition


import scala.concurrent.duration.Duration
import scala.util.chaining._

class RepeatIfFile[T <: EvalContext](doStep: String, operation: String, condition: String, filepath: Option[String], filepathRef: Option[String], operator: FileComparisonOperator, negate: Boolean, delay: Duration, timeout: Duration, engine: EvalEngine[T])
    extends Repeat(doStep, operation, condition, delay, timeout, engine) {

  override def evaluteCondition(ctx: T): Boolean = {
    val fCond = FileCondition(filepath, filepathRef, operator, negate, ctx)
    ctx.evaluate(true) {
      fCond.evaluate()
    }
  }

}


