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

package gwen.core.eval.lambda.unit

import gwen.core.LocationType
import gwen.core.Errors
import gwen.core.eval.EvalContext
import gwen.core.eval.binding.DryValueBinding
import gwen.core.eval.lambda.UnitStep
import gwen.core.node.GwenNode
import gwen.core.node.gherkin.Step
import gwen.core.behavior.BehaviorType

class CapturePDF[T <: EvalContext](target: String, sourceType: LocationType, sourceLocation: String, timeoutSecs: Long) extends UnitStep[T] {

  override def apply(parent: GwenNode, step: Step, ctx: T): Step = {
    checkStepRules(step, BehaviorType.Action, ctx)
    val content = ctx.evaluate(step.dryValue(target).getOrElse(DryValueBinding.unresolved("pdfText"))) {
      var result: Option[String] = None
      var error: Option[Throwable] = None
      try {
        ctx.waitUntil(timeoutSecs, s"waiting for PDF at $sourceType: $sourceLocation") {
          try {
            result = Option(ctx.capturePDFText(sourceType, sourceLocation))
          } catch {
            case e: Throwable => 
              error = Some(e)
          }
          result.nonEmpty
        }
      } catch {
        case e: Throwable =>
          result getOrElse {
            error map { err => 
              throw err
            } getOrElse {
              throw e
            }
          }
      }
      result getOrElse {
        Errors.waitTimeoutError(timeoutSecs, s"Timed out waiting for PDF at $sourceType: $sourceLocation")
      }
    }
    ctx.topScope.set(target, content)
    step.addAttachment(target, "txt", content)
  }

}

