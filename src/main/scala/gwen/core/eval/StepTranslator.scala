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

package gwen.core.eval

import gwen.core.eval.action.UnitStepAction
import gwen.core.eval.action.CompositeStepAction
import gwen.core.node.gherkin.Step

/**
  * Base trait for DSL step tranlsators.
  *
  * @author Branko Juric
  */
trait StepTranslator[T <: EvalContext] {
  engine: EvalEngine[T] =>

  /**
    * Must be implemented to translate a composite DSL step into an executable action.
    *
    * @param step the step to translate
    * @return a function that performs the composite step action and returns it in evaluated form
    */
  def translateCompositeStep(step: Step): Option[CompositeStepAction[T]]

  /**
    * Must be implemented to translate a DSL step into an executable action.
    *
    * @param step the step to translate
    * @return a step action
    */
  def translateStep(step: Step): UnitStepAction[T]
  
}
