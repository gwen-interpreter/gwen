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

package gwen.core.engine

import gwen.core.engine.lambda.UnitStep
import gwen.core.engine.lambda.CompositeStep
import gwen.core.model.Identifiable
import gwen.core.model.gherkin.Step

/**
  * Base trait for DSL step tranlsators.
  *
  * @author Branko Juric
  */
trait StepTranslator[T <: EvalContext] {
  engine: EvalEngine[T] =>

  /**
    * Must be implemented to translate a composite DSL step into an executable operation.
    *
    * @param parent the parent (calling node)
    * @param step the step to translate
    * @return a function that performs the composite step operation and returns it in evaluated form
    */
  def translateCompositeStep(parent: Identifiable, step: Step): Option[CompositeStep[T]]

  /**
    * Must be implemented to translate a DSL step into an executable operation.
    *
    * @param parent the parent (calling node)
    * @param step the step to translate
    * @return a step operation that throws an exception on failure
    */
  def translateStep(parent: Identifiable, step: Step): UnitStep[T]
  
}
