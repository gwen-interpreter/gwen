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
import gwen.core.eval.binding.JSBinding
import gwen.core.eval.binding.LoadStrategyBinding
import gwen.core.eval.lambda.CompositeStep
import gwen.core.eval.engine.StepDefEngine
import gwen.core.node.GwenNode
import gwen.core.node.gherkin.Annotations
import gwen.core.node.gherkin.Scenario
import gwen.core.node.gherkin.Step
import gwen.core.node.gherkin.Tag
import gwen.core.status._
import gwen.core.eval.binding.Binding

import scala.util.Try
import scala.util.Success
import scala.util.Failure

import util.chaining.scalaUtilChainingOps
import gwen.core.eval.binding.JSFunctionBinding

class IfCondition[T <: EvalContext](doStep: String, condition: String, negate: Boolean, engine: StepDefEngine[T]) extends CompositeStep[T](doStep) {

  override def apply(parent: GwenNode, step: Step, ctx: T): Step = {
    if (condition.matches(""".*( until | while | for each | if ).*""") && !condition.matches(""".*".*((until|while|for each|if)).*".*""")) {
      Errors.illegalStepError("Nested 'if' condition found in illegal step position (only trailing position supported)")
    }
    val (cond, javascript, isNegated) = {
      if (negate) {
        Try(resolve(getBinding(s"not $condition", ctx), ctx)) match { 
          case Success(js) => (s"not $condition", js, false)
          case Failure(_) => (condition, resolve(getBinding(condition, ctx), ctx), true)
        }
      } else {
        (condition, resolve(getBinding(condition, ctx), ctx), false)
      }
    }
    ctx.getStepDef(doStep, None) foreach { stepDef =>
      checkStepDefRules(step.copy(withName = doStep, withStepDef = Some(stepDef)), ctx)
    }
    val iStep = step.copy(withEvalStatus = Pending)
    val ifTag = Tag(Annotations.If)
    val tags = List(Tag(Annotations.Synthetic), ifTag, Tag(Annotations.StepDef))
    val iStepDef = Scenario(None, tags, ifTag.toString, s"${if (negate) "not " else ""}$condition", Nil, None, List(step.copy(withName = doStep)), Nil, Nil, Nil)
    val sdCall = () => engine.callStepDef(step, iStepDef, iStep, ctx)
    ctx.evaluate(sdCall()) {
      val boolResult = ctx.evaluateJSPredicate(ctx.interpolate(javascript))
      LoadStrategyBinding.bindIfLazy(cond, boolResult.toString, ctx)
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

  private def getBinding(cond: String, ctx: T): Binding[T, String] = {
    JSBinding(cond, Nil, ctx)
  }

  private def resolve(binding: Binding[T, String], ctx: T) = {
    Try {
      binding.resolve()
    } getOrElse {
      JSFunctionBinding(binding.name, ctx).resolve()
    }
  }

}
