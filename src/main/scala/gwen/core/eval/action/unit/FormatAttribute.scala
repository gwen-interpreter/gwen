/*
 * Copyright 2021-2026 Branko Juric, Brady Wood
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
import gwen.core.eval.EvalContext
import gwen.core.eval.binding.XPathBinding
import gwen.core.eval.action.UnitStepAction
import gwen.core.node.GwenNode
import gwen.core.node.gherkin.Annotations
import gwen.core.node.gherkin.Step
import gwen.core.behavior.BehaviorType

import java.time.format.DateTimeFormatter
import java.time.LocalDate
import java.text.DecimalFormat
import java.text.NumberFormat

import scala.util.chaining._

class FormatAttribute[T <: EvalContext](source: String, sourceFormat: String, target: String, targetFormat: String) extends UnitStepAction[T] {

  override def apply(parent: GwenNode, step: Step, ctx: T): Step = {
    step tap { _ =>
      checkStepRules(step, BehaviorType.Action, ctx)
      val sourceValue = ctx.getBoundValue(source)
      def formatted = if (step.isDateTime) {
        ctx.evaluate(sourceValue) {
          LocalDate.parse(sourceValue, DateTimeFormatter.ofPattern(sourceFormat)).format(DateTimeFormatter.ofPattern(targetFormat))
        }
      } else if (step.isNumber) {
        ctx.evaluate(sourceValue) {
          new DecimalFormat(targetFormat).format(DecimalFormat(sourceFormat).parse(sourceValue))
        }
      } else {
        Errors.missingFormatAnnotationError(s"Missing format annotation on step: @${Annotations.DateTime} or @${Annotations.Number} expected.")
      }
      ctx.topScope.set(target, formatted, step.isData)
    }
  }

}
