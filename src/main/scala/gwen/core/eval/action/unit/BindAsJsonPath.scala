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

import gwen.core._
import gwen.core.eval.EvalContext
import gwen.core.eval.binding.LoadStrategyBinding
import gwen.core.eval.binding.JsonPathBinding
import gwen.core.eval.action.UnitStepAction
import gwen.core.node.GwenNode
import gwen.core.node.gherkin.Step
import gwen.core.behavior.BehaviorType

import scala.util.chaining._

class BindAsJsonPath[T <: EvalContext](target: String, jsonPath: String, source: String) extends UnitStepAction[T] {

  override def apply(parent: GwenNode, step: Step, ctx: T): Step = {
    step tap { _ =>
      checkStepRules(step, BehaviorType.Context, ctx)
      JsonPathBinding.bind(target, jsonPath, source, step.isMasked, ctx)
      step.loadStrategy foreach { strategy =>
        val value = {
          if (strategy == LoadStrategy.Eager) Option(new JsonPathBinding(target, ctx).resolve())
          else None
        }
        LoadStrategyBinding.bind(target, value, strategy, ctx)
      }
    }
  }

}
