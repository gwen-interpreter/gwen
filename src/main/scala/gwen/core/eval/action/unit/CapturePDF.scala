/*
 * Copyright 2021-2024 Branko Juric, Brady Wood
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

import gwen.core.LocationType
import gwen.core.eval.EvalContext
import gwen.core.eval.binding.DryValueBinding
import gwen.core.eval.action.UnitStepAction
import gwen.core.node.GwenNode
import gwen.core.node.gherkin.Step
import gwen.core.behavior.BehaviorType
import gwen.core.state.SensitiveData

class CapturePDF[T <: EvalContext](target: String, sourceType: LocationType, sourceLocation: String, timeoutSecs: Long) extends UnitStepAction[T] {

  override def apply(parent: GwenNode, step: Step, ctx: T): Step = {
    checkStepRules(step, BehaviorType.Action, ctx)
    val content = ctx.evaluate(step.dryValue(target).getOrElse(DryValueBinding.unresolved("pdfText"))) {
      SensitiveData.withValue(sourceLocation) { sourceLoc =>
        ctx.getWithWait(timeoutSecs, s"waiting for PDF at $sourceType: $sourceLocation") { () => 
          ctx.capturePDFText(sourceType, sourceLoc)
        }
      }
    }
    ctx.topScope.set(target, content)
    step.addAttachment(target, "txt", content)
  }

}

