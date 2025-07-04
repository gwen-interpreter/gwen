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

package gwen.core.eval.action.composite

import gwen.core.Errors
import gwen.core.eval.EvalContext
import gwen.core.eval.binding.Binding
import gwen.core.eval.binding.JSBinding
import gwen.core.eval.binding.JSFunctionBinding
import gwen.core.eval.binding.LoadStrategyBinding
import gwen.core.eval.action.CompositeStepAction
import gwen.core.eval.engine.StepDefEngine
import gwen.core.eval.support.BooleanCondition
import gwen.core.node.GwenNode
import gwen.core.node.gherkin.Annotations
import gwen.core.node.gherkin.Scenario
import gwen.core.node.gherkin.Step
import gwen.core.node.gherkin.Tag
import gwen.core.status._

import util.chaining.scalaUtilChainingOps

class IfCondition[T <: EvalContext](doStep: String, condition: String, negate: Boolean, engine: StepDefEngine[T]) extends CompositeStepAction[T](doStep) {

  if (condition.matches("(not )?(true|false)")) Errors.illegalConditionError(condition)
  
  override def apply(parent: GwenNode, step: Step, ctx: T): Step = {
    if (condition.matches(""".*( until | while | for each | if ).*""") && !condition.matches(""".*".*((until|while|for each|if)).*".*""")) {
      Errors.illegalStepError("Nested 'if' condition found in illegal step position (only trailing position supported)")
    }
    val bCondition = new BooleanCondition(condition, negate, step.timeoutOpt.getOrElse(ctx.defaultWait).toSeconds, ctx)
    ctx.getStepDef(doStep, None) foreach { stepDef =>
      checkStepDefRules(step.copy(withName = doStep, withStepDef = Some(stepDef)), ctx)
    }
    val iStep = step.copy(withEvalStatus = Pending)
    val ifTag = Tag(Annotations.If)
    val tags = List(Tag(Annotations.Synthetic), ifTag, Tag(Annotations.StepDef))
    val iStepDef = Scenario(None, tags, ifTag.toString, s"${if (negate) "not " else ""}$condition", None, Nil, None, List(step.copy(withName = doStep)), Nil, Nil, Nil)
    val sdCall = () => engine.callStepDef(step, iStepDef, iStep, ctx)
    val attachments = ctx.popAttachments()
    ctx.evaluate(sdCall()) {
      val satisfied = bCondition.evaluate()
      LoadStrategyBinding.bindIfLazy(bCondition.name, satisfied.toString, ctx)
      val result = if (satisfied) {
        logger.info(s"Processing conditional step (${if (bCondition.negated) "not " else ""}${bCondition.name} = true): ${step.keyword} $doStep")
        sdCall()
      } else {
        logger.info(s"Skipping conditional step (${if (bCondition.negated) "not " else ""}${bCondition.name} = false): ${step.keyword} $doStep")
        step.copy(withEvalStatus = Passed(0, abstained = !ctx.options.dryRun))
      }
      result.addAttachments(attachments)
    }
  }

}
