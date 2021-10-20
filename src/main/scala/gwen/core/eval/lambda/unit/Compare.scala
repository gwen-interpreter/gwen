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

import gwen.core.eval.ComparisonOperator
import gwen.core.eval.EvalContext
import gwen.core.eval.lambda.UnitStep
import gwen.core.node.GwenNode
import gwen.core.node.gherkin.Step
import gwen.core.behaviour.BehaviourType

import scala.util.Failure
import scala.util.Success
import scala.util.chaining._

class Compare[T <: EvalContext](source: String, expression: String, operator: ComparisonOperator, negate: Boolean) extends UnitStep[T] {

  override def apply(parent: GwenNode, step: Step, ctx: T): Step = {
    step tap { _ =>
      checkStepRules(step, BehaviourType.Assertion, ctx)
      val binding = ctx.getBinding(source)
      val expected = ctx.parseExpression(operator, expression)
      val actualValue = binding.resolve()
      ctx.perform {
        val result = ctx.compare(source, expected, actualValue, operator, negate)
        val op = {
          if (operator == ComparisonOperator.`match template file`) {
            ComparisonOperator.`match template`
          } else {
            operator
          }
        }
        result match {
          case Success(assertion) =>
            assert(assertion, s"Expected $binding to ${if(negate) "not " else ""}$op '$expected' but got '$actualValue'")
          case Failure(error) =>
            assert(assertion = false, error.getMessage)
        }
      }
    }
  }

}
