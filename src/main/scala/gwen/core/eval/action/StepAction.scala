/*
 * Copyright 202-2024 Branko Juric, Brady Wood
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

package gwen.core.eval.action

import gwen.core.eval.EvalContext
import gwen.core.node.GwenNode
import gwen.core.node.gherkin.Step
import gwen.core.behavior.BehaviorRules

import com.typesafe.scalalogging.LazyLogging

/**
  * Base class for all step actions.
  */
abstract class StepAction[T <: EvalContext, U]() extends BehaviorRules with LazyLogging {

  /**
    * The operation to apply.
    *
    * @param parent the calling node
    * @param step the step to apply the operation to
    * @param ctx the evaluation context
    */
  def apply(parent: GwenNode, step: Step, ctx: T): U

}

