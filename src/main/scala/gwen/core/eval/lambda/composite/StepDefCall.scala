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

package gwen.core.eval.lambda.composite

import gwen.core.eval.EvalContext
import gwen.core.eval.lambda.CompositeStep
import gwen.core.eval.engine.StepDefEngine
import gwen.core.node.GwenNode
import gwen.core.node.gherkin.Scenario
import gwen.core.node.gherkin.Step

class StepDefCall[T <: EvalContext](caller: GwenNode, stepDef: Scenario, engine: StepDefEngine[T]) extends CompositeStep[T] {

  override def apply(parent: GwenNode, step: Step, ctx: T): Step = {
    engine.callStepDef(caller, stepDef, step, ctx)
  }

}
