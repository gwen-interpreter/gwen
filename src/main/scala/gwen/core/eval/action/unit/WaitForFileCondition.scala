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

import gwen.core.Assert
import gwen.core.behavior.BehaviorType
import gwen.core.eval.EvalContext
import gwen.core.eval.FileComparisonOperator
import gwen.core.eval.support.FileCondition
import gwen.core.eval.action.UnitStepAction
import gwen.core.node.GwenNode
import gwen.core.node.gherkin.Step

import scala.util.chaining._

class WaitForFileCondition[T <: EvalContext](filepath: Option[String], filepathRef: Option[String], operator: FileComparisonOperator, negate: Boolean) extends UnitStepAction[T] {

  override def apply(parent: GwenNode, step: Step, ctx: T): Step = {
    step tap { _ =>
      checkStepRules(step, BehaviorType.Action, ctx)
      val fCond = FileCondition(filepath, filepathRef, operator, negate, ctx)
      val timeoutSecs = step.timeoutOpt.getOrElse(ctx.defaultWait).toSeconds
      Assert(timeoutSecs > 0, "timeout must be greater than zero")
      ctx.evaluate(true) {
        ctx.waitUntil(timeoutSecs, s"waiting for true return from file condition: ${fCond.condition}") {
          fCond.evaluate() tap { result => 
            if (!result) Thread.sleep(200)
          }
        }
      }
    }
  }

}
