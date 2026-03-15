/*
 * Copyright 2026 Branko Juric, Brady Wood
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

package gwen.core.eval.action.unit

import gwen.core.Errors
import gwen.core.eval.binding.BindingType
import gwen.core.eval.binding.DryValueBinding
import gwen.core.eval.EvalContext
import gwen.core.eval.binding.XPathBinding
import gwen.core.eval.action.UnitStepAction
import gwen.core.node.GwenNode
import gwen.core.node.gherkin.Annotations
import gwen.core.node.gherkin.Step
import gwen.core.behavior.BehaviorType

import scala.util.chaining._

class FormatText[T <: EvalContext](source: String, sourceFormat: String, target: String, targetFormat: String) extends UnitStepAction[T] {

  override def apply(parent: GwenNode, step: Step, ctx: T): Step = {
    checkStepRules(step, BehaviorType.Action, ctx)
    val sourceValue = ctx.getBoundValue(source)
    def formatted = if (step.isDateTime) {
      ctx.evaluate(step.dryValue(target).getOrElse(DryValueBinding.unresolved("formatDateTime"))) {
        ctx.formatDateTime(sourceValue, sourceFormat, targetFormat)
      }
    } else if (step.isNumber) {
      ctx.evaluate(step.dryValue(target).getOrElse(DryValueBinding.unresolved("formatNumber"))) {
        ctx.formatNumber(sourceValue, sourceFormat, targetFormat)
      }
    } else {
      Errors.missingFormatAnnotationError(s"Missing format annotation on step: @${Annotations.DateTime} or @${Annotations.Number} expected.")
    }
    ctx.topScope.set(target, formatted, step.isData)
    step.addAttachment(target, "txt", formatted)
  }

}
