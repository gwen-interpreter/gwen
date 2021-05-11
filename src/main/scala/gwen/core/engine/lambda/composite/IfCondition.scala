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

package gwen.core.engine.lambda.composite

import gwen.core.Errors
import gwen.core.engine.EvalContext
import gwen.core.engine.EvalEngine
import gwen.core.engine.binding.JavaScriptBinding
import gwen.core.engine.lambda.CompositeStep
import gwen.core.model._
import gwen.core.model.gherkin.Scenario
import gwen.core.model.gherkin.Step

import gwen.core.model.Passed

class IfCondition[T <: EvalContext](doStep: String, condition: String, engine: EvalEngine[T], ctx: T) extends CompositeStep[T](engine, ctx) {

  override def apply(parent: Identifiable, step: Step): Step = {
    if (condition.matches(""".*( until | while | for each | if ).*""") && !condition.matches(""".*".*((until|while|for each|if)).*".*""")) {
      Errors.illegalStepError("Nested 'if' condition found in illegal step position (only trailing position supported)")
    }
    val binding = new JavaScriptBinding(condition, ctx)
    val javascript = binding.resolve()
    env.getStepDef(doStep) foreach { stepDef =>
      engine.checkStepDefRules(step.copy(withName = doStep, withStepDef = Some(stepDef)), env)
    }
    val iStep = step.copy(withEvalStatus = Pending)
    val tags = List(Tag(ReservedTags.Synthetic), Tag(ReservedTags.If), Tag(ReservedTags.StepDef))
    val iStepDef = Scenario(None, tags, ReservedTags.If.toString, condition, Nil, None, List(step.copy(withName = doStep)), Nil)
    val stepDefCall = new StepDefCall(step, iStepDef, Nil, engine, ctx)
    ctx.evaluate(stepDefCall(step, iStep)) {
      val satisfied = ctx.evaluateJSPredicate(ctx.interpolate(javascript))
      if (satisfied) {
        logger.info(s"Processing conditional step ($condition = true): ${step.keyword} $doStep")
        stepDefCall(step, iStep)
      } else {
        logger.info(s"Skipping conditional step ($condition = false): ${step.keyword} $doStep")
        step.copy(withEvalStatus = Passed(0))
      }
    }
  }

}
