/*
 * Copyright 2021-2022 Branko Juric, Brady Wood
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
import gwen.core.eval.lambda.CompositeStep
import gwen.core.node.GwenNode
import gwen.core.node.gherkin.Annotations
import gwen.core.node.gherkin.Scenario
import gwen.core.node.gherkin.Step
import gwen.core.node.gherkin.StepKeyword
import gwen.core.node.gherkin.Tag
import gwen.core.status._

import scala.concurrent.duration.Duration
import scala.util.chaining._

import java.util.Date

abstract class Repeat[T <: EvalContext](doStep: String, operation: String, condition: String, delay: Duration, timeout: Duration, engine: EvalEngine[T]) extends CompositeStep[T](doStep) with ImplicitValueKeys {

  if (condition.matches("(not )?(true|false)")) Errors.illegalConditionError(condition)

  def evaluteCondition(ctx: T): Boolean

  override def apply(parent: GwenNode, step: Step, ctx: T): Step = {
    Assert(delay.gteq(Duration.Zero), "delay cannot be less than zero")
    Assert(timeout.gt(Duration.Zero), "timeout must be greater than zero")
    Assert(timeout.gteq(delay), "timeout cannot be less than or equal to delay")
    val operationTag = Tag(if (operation == "until") Annotations.Until else Annotations.While)
    val tags = List(Tag(Annotations.Synthetic), operationTag, Tag(Annotations.StepDef))
    val preCondStepDef = Scenario(None, tags, operationTag.name, condition, None, Nil, None, Nil, Nil, step.params, step.cumulativeParams)
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
            withParams = List((`iteration.number`, iteration.toString)) ++ step.params
          )
          operation match {
            case "until" =>
              logger.info(s"repeat-until $condition: iteration $iteration")
              if (condSteps.isEmpty) {
                ctx.topScope.stepDefScope.set(`gwen.stepDef.eval.started`, new Date().toString)
                engine.beforeStepDef(preCondStepDef, ctx)
              }
              val iterationStep = engine.evaluateStep(preCondStepDef, preStep, ctx)
              condSteps = iterationStep :: condSteps
              iterationStep.evalStatus match {
                case Failed(_, e) => throw e
                case _ =>
                  evaluteCondition(ctx) tap { result =>
                    if (!result) {
                      logger.info(s"repeat-until $condition: not complete, will repeat ${if (delay.gt(Duration.Zero)) s"in ${DurationFormatter.format(delay)}" else "now"}")
                      if (delay.gt(Duration.Zero)) {
                        condSteps = delayStep(delay, ctx) :: condSteps
                      }
                    } else {
                      logger.info(s"repeat-until $condition: completed")
                    }
                  }
              }
            case "while" =>
              val result = evaluteCondition(ctx)
              if (result) {
                logger.info(s"repeat-while $condition: iteration $iteration")
                if (condSteps.isEmpty) {
                  ctx.topScope.stepDefScope.set(`gwen.stepDef.eval.started`, new Date().toString)
                  engine.beforeStepDef(preCondStepDef, ctx)
                }
                val iterationStep = engine.evaluateStep(preCondStepDef, preStep, ctx)
                condSteps = iterationStep :: condSteps
                iterationStep.evalStatus match {
                  case Failed(_, e) => throw e
                  case _ =>
                    logger.info(s"repeat-while $condition: not complete, will repeat ${if (delay.gt(Duration.Zero)) s"in ${DurationFormatter.format(delay)}" else "now"}")
                    if (delay.gt(Duration.Zero)) {
                      condSteps = delayStep(delay, ctx) :: condSteps
                    }
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
      operation match {
        case "until" =>
          val iterationStep = engine.evaluateStep(step, step.copy(withName = doStep), ctx)
          evaluteCondition(ctx)
          condSteps = iterationStep :: condSteps
        case _ =>
          evaluteCondition(ctx)
          val iterationStep = engine.evaluateStep(step, step.copy(withName = doStep), ctx)
          condSteps = iterationStep :: condSteps
      }
    }
    if (condSteps.nonEmpty) {
      val steps = ctx.evaluate(condSteps) {
        evaluatedStep.evalStatus match {
          case Failed(nanos, error) if (EvalStatus(condSteps.map(_.evalStatus)).isPassed) =>
            val preStep = condSteps.head.copy(
              withKeyword = StepKeyword.And.toString,
              withName = doStep,
              withParams = List((`iteration.number`, (iteration + 1).toString)) ++ step.params
            )
            engine.beforeStep(preStep, ctx)
            val fStep = engine.finaliseStep(
              preStep.copy(
                withEvalStatus = Failed(nanos - condSteps.map(_.evalStatus.nanos).sum, error),
                withStepDef = None,
                withTags = preStep.tags.filter(_.name != Annotations.Synthetic.toString)
              ),
              None,
              ctx
            )
            engine.afterStep(fStep, ctx)
            fStep :: condSteps
          case _ =>
            condSteps

        }
      }
      val condStepDef = preCondStepDef.copy(
        withSteps = steps.reverse
      )
      ctx.perform {
        ctx.topScope.stepDefScope.set(`gwen.stepDef.eval.finished`, new Date().toString)
        engine.afterStepDef(condStepDef, ctx)
      }
      evaluatedStep.copy(
        withEvalStatus = condStepDef.evalStatus,
        withStepDef = Some(condStepDef)
      )
    } else {
      evaluatedStep
    }
  }

  private def delayStep(delay: Duration, ctx: T): Step = {
    val preWaitStep = Step(None, StepKeyword.And.toString, s"delay", Nil, None, Nil, None, Pending, Nil, Nil, List(Tag(Annotations.Synthetic)), None, Nil)
    engine.beforeStep(preWaitStep, ctx)
    val start = System.nanoTime()
    Thread.sleep(delay.toMillis)
    preWaitStep.copy(withEvalStatus = Passed(System.nanoTime() - start)) tap { postWaitStep =>
      engine.afterStep(postWaitStep, ctx)
    }
  }

}
