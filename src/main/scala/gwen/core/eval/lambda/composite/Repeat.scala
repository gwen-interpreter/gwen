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

import gwen.core._
import gwen.core.Formatting.DurationFormatter
import gwen.core.eval.EvalContext
import gwen.core.eval.EvalEngine
import gwen.core.eval.binding.JSBinding
import gwen.core.eval.lambda.CompositeStep
import gwen.core.node.GwenNode
import gwen.core.node.gherkin.Annotations
import gwen.core.node.gherkin.Scenario
import gwen.core.node.gherkin.Step
import gwen.core.node.gherkin.StepKeyword
import gwen.core.node.gherkin.Tag
import gwen.core.state.ReservedParam
import gwen.core.status._

import scala.concurrent.duration.Duration
import scala.util.chaining._

class Repeat[T <: EvalContext](doStep: String, operation: String, condition: String, delay: Duration, timeout: Duration, engine: EvalEngine[T]) extends CompositeStep[T](doStep) {

  override def apply(parent: GwenNode, step: Step, ctx: T): Step = {
    assert(delay.gteq(Duration.Zero), "delay cannot be less than zero")
    assert(timeout.gt(Duration.Zero), "timeout must be greater than zero")
    assert(timeout.gteq(delay), "timeout cannot be less than or equal to delay")
    val operationTag = Tag(if (operation == "until") Annotations.Until else Annotations.While)
    val tags = List(Tag(Annotations.Synthetic), operationTag, Tag(Annotations.StepDef))
    val preCondStepDef = Scenario(None, tags, operationTag.name, condition, Nil, None, Nil, Nil, step.params, step.cumulativeParams)
    var condSteps: List[Step] = Nil
    var evaluatedStep = step
    val start = System.nanoTime()
    var iteration = 0
    ctx.perform {
      try {
        ctx.waitUntil(timeout.toSeconds.toInt, s"trying to repeat: ${step.name}") {
          iteration = iteration + 1
          val preStep = step.copy(
            withKeyword = if(iteration == 1) step.keyword else StepKeyword.And.toString,
            withName = doStep,
            withParams = List((ReservedParam.`iteration.number`.toString, iteration.toString)) ++ step.params
          )
          operation match {
            case "until" =>
              logger.info(s"repeat-until $condition: iteration $iteration")
              if (condSteps.isEmpty) {
                engine.beforeStepDef(preCondStepDef, ctx)
              }
              val iterationStep = engine.evaluateStep(preCondStepDef, preStep, ctx)
              condSteps = iterationStep :: condSteps
              iterationStep.evalStatus match {
                case Failed(_, e) => throw e
                case _ =>
                  val javascript = ctx.interpolate(ctx.scopes.get(JSBinding.key(condition)))
                  ctx.evaluateJSPredicate(javascript) tap { result =>
                    if (!result) {
                      logger.info(s"repeat-until $condition: not complete, will repeat ${if (delay.gt(Duration.Zero)) s"in ${DurationFormatter.format(delay)}" else "now"}")
                      if (delay.gt(Duration.Zero)) Thread.sleep(delay.toMillis)
                    } else {
                      logger.info(s"repeat-until $condition: completed")
                    }
                  }
              }
            case "while" =>
              val javascript = ctx.interpolate(ctx.scopes.get(JSBinding.key(condition)))
              val result = ctx.evaluateJSPredicate(javascript)
              if (result) {
                logger.info(s"repeat-while $condition: iteration $iteration")
                if (condSteps.isEmpty) {
                  engine.beforeStepDef(preCondStepDef, ctx)
                }
                val iterationStep = engine.evaluateStep(preCondStepDef, preStep, ctx)
                condSteps = iterationStep :: condSteps
                iterationStep.evalStatus match {
                  case Failed(_, e) => throw e
                  case _ =>
                    logger.info(s"repeat-while $condition: not complete, will repeat ${if (delay.gt(Duration.Zero)) s"in ${DurationFormatter.format(delay)}" else "now"}")
                    if (delay.gt(Duration.Zero)) Thread.sleep(delay.toMillis)
                }
              } else {
                logger.info(s"repeat-while $condition: completed")
              }
              !result
          }
        }
      } catch {
        case e: Throwable =>
          if (ctx.options.verbose) {
            logger.error(e.getMessage)
          }
          val nanos = System.nanoTime() - start
          val durationNanos = {
            if (nanos > timeout.toNanos) timeout.toNanos
            else nanos
          }
          evaluatedStep = step.copy(
            withEvalStatus = Failed(durationNanos, new Errors.StepException(step, e.getMessage, e))
          )
      }
    } getOrElse {
      try {
        operation match {
          case "until" =>
            evaluatedStep = engine.evaluateStep(step, step.copy(withName = doStep), ctx)
            ctx.scopes.get(JSBinding.key(condition))
          case _ =>
            ctx.scopes.get(JSBinding.key(condition))
            evaluatedStep = engine.evaluateStep(step, step.copy(withName = doStep), ctx)
        }
      } catch {
        case _: Throwable =>
          // ignore in dry run mode
      }
    }
    if (condSteps.nonEmpty) {
      val steps = evaluatedStep.evalStatus match {
        case Failed(nanos, error) if (EvalStatus(condSteps.map(_.evalStatus)).isPassed) =>
          val preStep = condSteps.head.copy(
            withKeyword = StepKeyword.And.toString,
            withName = doStep,
            withParams = List((ReservedParam.`iteration.number`.toString, (iteration + 1).toString)) ++ step.params
          )
          engine.beforeStep(preStep, ctx)
          val fStep = engine.finaliseStep(
            preStep.copy(
              withEvalStatus = Failed(nanos - condSteps.map(_.evalStatus.nanos).sum, error),
              withStepDef = None
            ),
            ctx
          )
          engine.afterStep(fStep, ctx)
          fStep :: condSteps
        case _ =>
          condSteps

      }
      val condStepDef = preCondStepDef.copy(withSteps = steps.reverse)
      engine.afterStepDef(condStepDef, ctx)
      evaluatedStep.copy(
        withEvalStatus = condStepDef.evalStatus,
        withStepDef = Some(condStepDef)
      )
    } else {
      evaluatedStep
    }
  }

}
