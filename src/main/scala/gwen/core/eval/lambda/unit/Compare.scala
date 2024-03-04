/*
 * Copyright 2021-2023 Branko Juric, Brady Wood
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

import gwen.core.Assert
import gwen.core.ValueLiteral
import gwen.core.eval.ComparisonOperator
import gwen.core.eval.EvalContext
import gwen.core.eval.lambda.UnitStep
import gwen.core.node.GwenNode
import gwen.core.node.gherkin.Step
import gwen.core.behavior.BehaviorType

import scala.util.Failure
import scala.util.Success
import scala.util.chaining._
import scala.util.Try

class Compare[T <: EvalContext](source: String, expression: String, operator: ComparisonOperator, negate: Boolean, message: Option[String]) extends UnitStep[T] {

  override def apply(parent: GwenNode, step: Step, ctx: T): Step = {
    step tap { _ =>
      checkStepRules(step, BehaviorType.Assertion, ctx)
      val expected = ctx.parseExpression(operator, expression)
      val actualValue = ctx.getBoundValue(source)
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
            val displayName = Try(ctx.getBinding(source).displayName).getOrElse(source)
            ctx.assertWithError(
              assertion, 
              message, 
              Assert.formatFailed(displayName, expected, actualValue, negate, op),
              step.assertionMode)
          case Failure(error) =>
            throw error;
        }
      }
    }
  }

}
