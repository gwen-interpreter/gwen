/*
 * Copyright 2022 Branko Juric, Brady Wood
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

package gwen.core.eval.lambda.unit

import gwen.core.eval.ComparisonOperator
import gwen.core.eval.SimilarityOperator
import gwen.core.eval.EvalContext
import gwen.core.eval.lambda.UnitStep
import gwen.core.node.GwenNode
import gwen.core.node.gherkin.Step
import gwen.core.behavior.BehaviorType

import scala.util.Failure
import scala.util.Success
import scala.util.chaining._
import gwen.core.Formatting

class CaptureSimilarity[T <: EvalContext](target: String, source1: String, source2: Option[String], sourceValue2: Option[String], trim: Boolean, ignoreCase: Boolean) extends UnitStep[T] {

  override def apply(parent: GwenNode, step: Step, ctx: T): Step = {
    checkStepRules(step, BehaviorType.Assertion, ctx)
    val binding1 = ctx.getBinding(source1)
    val binding2 = source2.map(ctx.getBinding)
    val value1 = binding1.resolve()
    val value2 = sourceValue2.getOrElse(binding2.get.resolve())
    ctx.evaluate(Some(1.0)) {
      ctx.dscSimilarity(Formatting.format(value1, trim, ignoreCase), Formatting.format(value2, trim, ignoreCase))
    } map { score =>
      ctx.topScope.set(target, score.toString)
      step.addAttachment(target, "txt", score.toString)
    } getOrElse {
      if (ctx.topScope.getOpt(target).nonEmpty) {
        ctx.topScope.set(target, null)
      }
      step
    }
  }

}
