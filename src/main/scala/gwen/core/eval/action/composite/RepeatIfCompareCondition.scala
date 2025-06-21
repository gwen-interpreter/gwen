/*
 * Copyright 2024 Branko Juric, Brady Wood
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

package gwen.core.eval.action.composite

import gwen.core.Errors
import gwen.core.eval.action.unit.Compare
import gwen.core.eval.ComparisonOperator
import gwen.core.eval.EvalContext
import gwen.core.eval.EvalEngine
import gwen.core.node.gherkin.Step

import scala.util.Failure
import scala.util.Success
import scala.util.Try

class RepeatIfCompareCondition[T <: EvalContext](doStep: String, operation: String, name: String, operator: ComparisonOperator, negate: Boolean, expression: String, engine: EvalEngine[T])
    extends Repeat(
      doStep, 
      operation, 
      s"$name " + (
        if (negate) {
          if (operator == ComparisonOperator.be) "is not" else s"does not $operator"
        } else {
          operator match {
            case ComparisonOperator.be => "is"
            case ComparisonOperator.contain => "contains"
            case ComparisonOperator.`start with` => "starts with"
            case ComparisonOperator.`end with` => "ends with"
            case ComparisonOperator.`match regex` => "matches regex"
            case ComparisonOperator.`match xpath` => "matches xpath"
            case ComparisonOperator.`match json path` => "matches json path"
            case ComparisonOperator.`match template` => "matches template"
            case ComparisonOperator.`match template file` => "matches template file"
          }
        }
      ) + s""" "${if (expression == "") "blank" else expression}"""",
      engine) {

  override def evaluteCondition(step: Step, ctx: T): Boolean = {
    if (ctx.options.dryRun) {
      ctx.parseExpression(operator, expression)
      try {
        ctx.getBoundValue(name)
      } catch {
        case _: Errors.UnboundReferenceException =>
          ctx.getBinding(name)
      }
    }
    val compare = new Compare[T](name, expression, operator, negate)
    Try(compare.apply(step, ctx)) match {
      case Success(_) => true
      case Failure(e) =>
        if (e.isInstanceOf[AssertionError]) false
        else throw e
    }
  }

}


