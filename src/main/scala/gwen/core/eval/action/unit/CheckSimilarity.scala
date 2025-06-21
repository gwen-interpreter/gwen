/*
 * Copyright 2022-2024 Branko Juric, Brady Wood
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
import gwen.core.eval.ComparisonOperator
import gwen.core.eval.SimilarityOperator
import gwen.core.eval.EvalContext
import gwen.core.eval.action.UnitStepAction
import gwen.core.node.GwenNode
import gwen.core.node.gherkin.Step
import gwen.core.behavior.BehaviorType

import scala.util.Failure
import scala.util.Success
import scala.util.chaining._

class CheckSimilarity[T <: EvalContext](source1: String, source2: Option[String], sourceValue2: Option[String], operator: SimilarityOperator, percentage: Double, negate: Boolean) extends UnitStepAction[T] {

  override def apply(parent: GwenNode, step: Step, ctx: T): Step = {
    checkStepRules(step, BehaviorType.Assertion, ctx)
    val binding1 = ctx.getBinding(source1)
    val binding2 = source2.map(ctx.getBinding)
    val value1 = binding1.resolve()
    val value2 = sourceValue2.getOrElse(binding2.get.resolve())
    var similarityScore: Option[Double] = None
    ctx.perform {
      val message = step.message
      val trim = step.isTrim
      val ignoreCase = step.isIgnoreCase
      ctx.checkSimilarity(Formatting.format(value1, trim, ignoreCase), Formatting.format(value2, trim, ignoreCase), operator, percentage, negate) match {
        case Success((passed, score)) =>
          similarityScore = score
          score match {
            case Some (s) =>
              ctx.topScope.set("similarity score", s.toString)
            case _ =>
              if (ctx.topScope.getOpt("similarity score").nonEmpty) {
                ctx.topScope.set("similarity score", null)
              }    
          }
          ctx.assertWithError(
            passed, 
            message, 
            s"Expected ${binding1.displayName} '$value1' to${if (negate) " not" else ""} $operator ${Formatting.upTo2DecimalPlaces(percentage)}% similar to${binding2.map(b => s" ${b.displayName}").getOrElse("")} '$value2' but was${score.map(s => s" ${Formatting.upTo2DecimalPlaces(s * 100)}%").getOrElse(if (!negate) " not" else "")}",
            step.assertionMode)
        case Failure(error) =>
          if (ctx.topScope.getOpt("similarity score").nonEmpty) {
            ctx.topScope.set("similarity score", null)
          }
          throw error
      }
    }
    similarityScore map { score =>
      step.addAttachment("similarity score", "txt", score.toString)
    } getOrElse step
  }

}
