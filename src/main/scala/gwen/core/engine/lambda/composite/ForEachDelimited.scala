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

package gwen.core.engine.lambda.composite

import gwen.core.engine.EvalContext
import gwen.core.engine.EvalEngine
import gwen.core.model.Identifiable
import gwen.core.model.gherkin.Step

class ForEachDelimited[T <: EvalContext](doStep: String, entry: String, source: String, delimiter: String, engine: EvalEngine[T]) extends ForEach[T](engine) {

  override def apply(parent: Identifiable, step: Step, ctx: T): Step = {
    val sourceValue = ctx.getBoundReferenceValue(source)
    val values = () => {
      sourceValue.split(delimiter).toSeq
    }
    evaluateForEach(values, entry, parent, step, doStep, ctx)
  }

}