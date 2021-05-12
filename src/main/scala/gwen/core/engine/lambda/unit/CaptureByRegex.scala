/*
 * Copyright 2021 Branko Juric, Brady Wood
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

package gwen.core.engine.lambda.unit

import gwen.core._
import gwen.core.engine.EvalContext
import gwen.core.engine.binding.BindingType
import gwen.core.engine.lambda.UnitStep
import gwen.core.model.BehaviorType
import gwen.core.model.Identifiable
import gwen.core.model.gherkin.Step

class CaptureByRegex[T <: EvalContext](target: String, regex: String, source: String) extends UnitStep[T] {

  override def apply(parent: Identifiable, step: Step, ctx: T): Unit = {
    checkStepRules(step, BehaviorType.Action, ctx)
    val sourceValue = ctx.getBoundReferenceValue(source)
    val result = ctx.evaluate(s"$$[dryRun:${BindingType.regex}]") {
      ctx.extractByRegex(regex, sourceValue) tap { content =>
        ctx.addAttachment(target, "txt", content)
      }
    }
    ctx.topScope.set(target, result)
  }

}

