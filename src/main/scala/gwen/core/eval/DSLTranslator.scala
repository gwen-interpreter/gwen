/*
 * Copyright 2014-2021 Branko Juric, Brady Wood
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

import gwen.core.model.Identifiable
import gwen.core.model.gherkin.Step

/**
  * Base trait for DSL step tranlsators.
  *
  * @author Branko Juric
  */
trait DSLTranslator[T <: EvalContext] {
  engine: EvalEngine[T] =>

  /**
    * Must be implemented to translate a composite DSL step into an executable operation.
    *
    * @param parent the parent (calling node)
    * @param step the step to translate
    * @param env the environment state
    * @param ctx the evaluation context
    * @param a function that performs the composite step operation and returns it in evaluated form
    */
  def translateComposite(parent: Identifiable, step: Step, env: EvalEnvironment, ctx: T): Option[Step => Step]

  /**
    * Must be implemented to translate a DSL step into an executable operation.
    *
    * @param parent the parent (calling node)
    * @param step the step to translate
    * @param env the environment state
    * @param ctx the evaluation context
    * @return a step operation that throws an exception on failure
    */
  def translate(parent: Identifiable, step: Step, env: EvalEnvironment, ctx: T): Step => Unit
  
}
