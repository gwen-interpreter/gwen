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

package gwen.core.eval.step.composite

import gwen.core.eval.EvalContext
import gwen.core.eval.EvalEngine
import gwen.core.eval.step.DSLStep
import gwen.core.model.Identifiable
import gwen.core.model.gherkin.Step

/**
  * A compositie DSL step.
  */
abstract class CompositeStep[T <: EvalContext](engine: EvalEngine[T], ctx: T) extends DSLStep[T, Step](engine, ctx) {

  /**
    * The composite step operation to apply.
    *
    * @param parent the calling node
    * @param step the composite step to apply to operation to
    */
  override def apply(parent: Identifiable, step: Step): Step

}
