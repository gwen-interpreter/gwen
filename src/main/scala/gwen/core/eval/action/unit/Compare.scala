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

package gwen.core.eval.action.unit

import gwen.core.Formatting
import gwen.core.AssertionMode
import gwen.core.ImplicitValueKeys
import gwen.core.Assert
import gwen.core.Errors
import gwen.core.ValueLiteral
import gwen.core.eval.ComparisonOperator
import gwen.core.eval.EvalContext
import gwen.core.eval.action.UnitStepAction
import gwen.core.node.GwenNode
import gwen.core.node.gherkin.Step
import gwen.core.behavior.BehaviorType

import scala.util.Failure
import scala.util.Success
import scala.util.chaining._
import scala.util.Try

class Compare[T <: EvalContext](source: String, expression: String, operator: ComparisonOperator, negate: Boolean, message: Option[String], trim: Boolean, ignoreCase: Boolean) extends UnitStepAction[T] with ImplicitValueKeys {

  
  override def apply(parent: GwenNode, step: Step, ctx: T): Step = {
    step tap { _ =>
      checkStepRules(step, BehaviorType.Assertion, ctx)
      apply(ctx, step.assertionMode)
    }
  }

  def apply(ctx: T, assertionMode: AssertionMode = AssertionMode.hard): Unit = {
    val expected = ctx.parseExpression(operator, expression)
    val actualValue = ctx.getBoundValue(source)
    ctx.perform {
      val result = ctx.compare(source, Formatting.format(expected, trim, ignoreCase), Formatting.format(actualValue, trim, ignoreCase), operator, negate)
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
          try {
            ctx.assertWithError(
              assertion, 
              message, 
              Assert.formatFailed(displayName, expected, actualValue, negate, op),
              assertionMode)
          } catch {
            case gae: Errors.GwenAssertionError if source == `gwen.accumulated.errors` =>
              Errors.accumulatedAssertionError(gae)
          }
        case Failure(error) =>
          throw error;
      }
    }
  }

}
