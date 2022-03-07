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

package gwen.core.eval.lambda.composite

import gwen.core.Errors
import gwen.core.eval.EvalContext
import gwen.core.eval.binding.JavaScriptBinding
import gwen.core.eval.lambda.CompositeStep
import gwen.core.eval.engine.StepDefEngine
import gwen.core.node.GwenNode
import gwen.core.node.gherkin.ReservedTags
import gwen.core.node.gherkin.Scenario
import gwen.core.node.gherkin.Step
import gwen.core.node.gherkin.Tag
import gwen.core.status._
import gwen.core.eval.binding.Binding

import scala.util.Try
import scala.util.Success
import scala.util.Failure

class IfCondition[T <: EvalContext](doStep: String, condition: String, negate: Boolean, engine: StepDefEngine[T]) extends CompositeStep[T](doStep) {

  override def apply(parent: GwenNode, step: Step, ctx: T): Step = {
    if (condition.matches(""".*( until | while | for each | if ).*""") && !condition.matches(""".*".*((until|while|for each|if)).*".*""")) {
      Errors.illegalStepError("Nested 'if' condition found in illegal step position (only trailing position supported)")
    }
    val (cond, javascript, isNegated) = {
      if (negate) {
        Try(getBinding(s"not $condition", ctx).resolve()) match { 
          case Success(js) => (s"not $condition", js, false)
          case Failure(_) => (condition, getBinding(condition, ctx).resolve(), true)
        }
      } else {
        (condition, getBinding(condition, ctx).resolve(), false)
      }
    }
    ctx.getStepDef(doStep, None) foreach { stepDef =>
      checkStepDefRules(step.copy(withName = doStep, withStepDef = Some(stepDef)), ctx)
    }
    val iStep = step.copy(withEvalStatus = Pending)
    val ifTag = Tag(ReservedTags.If)
    val tags = List(Tag(ReservedTags.Synthetic), ifTag, Tag(ReservedTags.StepDef))
    val iStepDef = Scenario(None, tags, ifTag.toString, cond, Nil, None, List(step.copy(withName = doStep)), Nil, Nil, Nil)
    val sdCall = () => engine.callStepDef(step, iStepDef, iStep, ctx)
    ctx.evaluate(sdCall()) {
      val boolResult = ctx.evaluateJSPredicate(ctx.interpolate(javascript))
      val satisfied = if (isNegated) !boolResult else boolResult
      if (satisfied) {
        logger.info(s"Processing conditional step (${if (isNegated) "not " else ""}$cond = true): ${step.keyword} $doStep")
        sdCall()
      } else {
        logger.info(s"Skipping conditional step (${if (isNegated) "not " else ""}$cond = false): ${step.keyword} $doStep")
        step.copy(withEvalStatus = Passed(0, abstained = true))
      }
    }
  }

  private def getBinding(cond: String, ctx: T): JavaScriptBinding[T] = {
    JavaScriptBinding(cond, ctx)
  }

}
