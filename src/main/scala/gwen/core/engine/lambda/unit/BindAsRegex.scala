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

import gwen.core.engine.EvalContext
import gwen.core.engine.lambda.UnitStep
import gwen.core.model.BehaviorType
import gwen.core.model.Identifiable
import gwen.core.model.gherkin.Step
import gwen.core.engine.binding.RegexBinding

class BindAsRegex[T <: EvalContext](target: String, regex: String, source: String) extends UnitStep[T] {

  override def apply(parent: Identifiable, step: Step, ctx: T): Unit = {
    ctx.withEnv { env =>
      ctx.checkStepRules(step, BehaviorType.Context, env)
      RegexBinding.bind(target, regex, source, env)
    }
  }

}

