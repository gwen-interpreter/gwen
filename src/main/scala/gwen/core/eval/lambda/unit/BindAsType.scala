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

import gwen.core._
import gwen.core.eval.EvalContext
import gwen.core.eval.binding.AttributeBinding
import gwen.core.eval.binding.AttributeBinding
import gwen.core.eval.binding.BindingType
import gwen.core.eval.binding.FileBinding
import gwen.core.eval.binding.JSBinding
import gwen.core.eval.binding.JSFunctionBinding
import gwen.core.eval.binding.LoadStrategyBinding
import gwen.core.eval.binding.SysprocBinding
import gwen.core.eval.lambda.UnitStep
import gwen.core.node.GwenNode
import gwen.core.node.gherkin.Step
import gwen.core.behavior.BehaviorType

import scala.util.chaining._

class BindAsType[T <: EvalContext](target: String, bindingType: BindingType, value: String, argsString: Option[String], delimiter: Option[String]) extends UnitStep[T] {

  override def apply(parent: GwenNode, step: Step, ctx: T): Step = {
    step tap { _ =>
      checkStepRules(step, BehaviorType.Context, ctx)
      bindingType match {
        case BindingType.javascript => JSBinding.bind(target, value, ctx) 
        case BindingType.function => JSFunctionBinding.bind(target, value, argsString.getOrElse(""), delimiter, ctx)
        case BindingType.sysproc => SysprocBinding.bind(target, value, delimiter, ctx)
        case BindingType.file => FileBinding.bind(target, value, ctx)
        case _ => ctx.topScope.set(target, Settings.get(value))
      }
      step.loadStrategy foreach { strategy =>
        val value = {
          if (strategy == LoadStrategy.Eager) Option(
            bindingType match {
              case BindingType.javascript => new JSBinding(target, Nil, ctx).resolve()
              case BindingType.function => new JSFunctionBinding(target, ctx).resolve()
              case BindingType.sysproc => new SysprocBinding(target, ctx).resolve()
              case BindingType.file => new FileBinding(target, ctx).resolve()
              case _ => null
            }
          )
          else None
        }
        LoadStrategyBinding.bind(target, value, strategy, ctx)
      }
    }
  }

}
