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

package gwen.core.eval.engine

import gwen.core._
import gwen.core.Errors
import gwen.core.eval.EvalContext
import gwen.core.eval.EvalEngine
import gwen.core.eval.GwenREPL
import gwen.core.behavior.BehaviorType
import gwen.core.eval.lambda.CompositeStep
import gwen.core.eval.lambda.StepLambda
import gwen.core.eval.lambda.composite.ForEachTableRecord
import gwen.core.eval.lambda.composite.ForEachTableRecordAnnotated
import gwen.core.eval.lambda.composite.StepDefCall
import gwen.core.node.GwenNode
import gwen.core.node.Root
import gwen.core.node.gherkin.ReservedTags
import gwen.core.node.gherkin.Scenario
import gwen.core.node.gherkin.Step
import gwen.core.node.gherkin.StepKeyword
import gwen.core.node.gherkin.table.DataTable
import gwen.core.status._

import scala.io.StdIn.readBoolean
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import scala.util.chaining._

/**
  * Step evaluation engine.
  */
trait StepEngine[T <: EvalContext] {
  engine: EvalEngine[T] =>

  /**
   * Interprets and evaluates single step expression.
   *
   * @param stepExpression the input step expression
   * @param ctx the evaluation context
   * @return the evaluated step (or an exception if a runtime error occurs)
   */
  def interpretStep(stepExpression: String, ctx: T): Try[Step] = {
    parseStep(stepExpression).map { step =>
      evaluateStep(Root, step, ctx)
    }
  }

    /**
    * Evaluates a list of steps.
    */
  def evaluateSteps(parent: GwenNode, steps: List[Step], ctx: T): List[Step] = {
    var behaviorCount = 0
    try {
      steps.foldLeft(List[Step]()) {
        (acc: List[Step], step: Step) =>
          if (!StepKeyword.isAnd(step.keyword)) {
            ctx.addBehavior(BehaviorType.of(step.keyword))
            behaviorCount = behaviorCount + 1
          }
          evaluateOrTransitionStep(parent, step, acc, ctx) :: acc
      } reverse
    } finally {
      0 until behaviorCount foreach { _ =>
        ctx.popBehavior()
      }
    }
  }

  private def evaluateOrTransitionStep(parent: GwenNode, step: Step, acc: List[Step], ctx: T): Step = {
    EvalStatus(acc.map(_.evalStatus)) match {
      case status @ Failed(_, error) if !step.isFinally =>
        ctx.evaluate(evaluateStep(parent, step, ctx)) {
          val isAssertionError = status.isAssertionError
          val isHardAssert = ctx.evaluate(false) { AssertionMode.isHard }
          if (!isAssertionError || isHardAssert) {
            transitionStep(step, Skipped, ctx)
          } else {
            evaluateStep(parent, step, ctx)
          }
        }
      case _ => evaluateStep(parent, step, ctx)
    }
  }

  /**
    * Evaluates a step.
    */
  def evaluateStep(parent: GwenNode, step: Step, ctx: T): Step = {
    val continue = if (step.isBreakpoint && ctx.options.debug) {
       new GwenREPL(engine, ctx).debug(parent, step)
    } else {
      true
    }
    if (continue) {
      val pStep = ctx.withStep(step) { ctx.interpolateParams }
      val eStep = pStep.evalStatus match {
        case Failed(_, e) if e.isInstanceOf[Errors.MultilineParamException] => pStep
        case _ =>
          val iStep = ctx.withStep(pStep) { ctx.interpolate }
          logger.info(s"Evaluating Step: $iStep")
          beforeStep(iStep.copy(withEvalStatus = Pending), ctx)
          ctx.withStep(iStep) { s =>
            Try(healthCheck(parent, s, ctx)) match {
              case Failure(e) => throw e
              case _ => translateAndEvaluate(parent, s, ctx)
            }
          }
      }
      finaliseStep(eStep, ctx) tap { fStep =>
        logStatus(ctx.options, fStep)
        afterStep(fStep, ctx)
      }
    } else {
      ctx.close()
      System.exit(0)
      step
    }
  }

  private def translateAndEvaluate(parent: GwenNode, step: Step, ctx: T): Step = {
    translateStepDef(step, ctx) map { sdLambda =>
      if (sdLambda.isInstanceOf[StepDefCall[T]]) {
        translateCompositeStep(step) match {
          case Some(cLambda) =>
            translateStepDef(step.copy(withName = cLambda.doStep), ctx) match {
              case None => 
                sdLambda
              case _ => 
                cLambda
            }
          case None => 
            sdLambda
        }
      } else {
        sdLambda
      }
    } orElse translateCompositeStep(step) map { lambda =>
      lambda(parent, step.copy(withEvalStatus = Pending), ctx)
    } getOrElse {
      evaluateUnitStep(parent, step, ctx)
    }
  }

  private def evaluateUnitStep(parent: GwenNode, step: Step, ctx: T): Step = {
    Try(translateStep(step)) match {
      case Success(lambda) if (!step.evalStatus.isFailed) =>
        lambda(parent, step.copy(withEvalStatus = Pending), ctx)
      case Failure(e) =>
        parent match {
          case scenario: Scenario if scenario.isStepDef && e.isInstanceOf[Errors.UndefinedStepException] =>
            ctx.getStepDef(step.expression, step.docString.map(_._2)) match {
              case Some(stepDef) => 
                throw new Errors.RecursiveStepDefException(stepDef)
              case _ => 
                throw e
            }
          case _ =>
            throw e
        }
      case _ =>
        step
    }
  }

  private def healthCheck(parent: GwenNode, step: Step, ctx: T): Unit = {
    if (step.indexIn(parent).map(_ == 0).getOrElse(false)) {
      parent match {
        case scenario: Scenario if !scenario.isStepDef =>
          healthCheck(step, ctx)
        case _ => // noop
      }
    }
  }

  private def translateStepDef(step: Step, ctx: T): Option[CompositeStep[T]] = {
    ctx.getStepDef(step.expression, step.docString.map(_._2)) match {
      case Some(stepDef) if stepDef.isForEach && stepDef.isDataTable =>
        val dataTable = ForEachTableRecord.parseFlatTable {
          stepDef.tags.find(_.name.startsWith(s"${ReservedTags.DataTable.toString}")) map {
            tag => DataTable(tag, step)
          }
        }
        Some(new ForEachTableRecordAnnotated(stepDef, step, dataTable, this))
      case Some(stepDef) if !ctx.paramScope.containsScope(stepDef.name) =>
        Some(new StepDefCall(step, stepDef, this))
      case _ =>
        None
    }
  }

  /**
    * Binds all accumulated attachments to the given step.
    *
    * @param step the step to bind attachments to
    * @return the step with accumulated attachments
    */
  def finaliseStep(step: Step, ctx: T): Step = {
    val eStep = {
      if (step.stepDef.isEmpty) {
        step.evalStatus match {
          case failure: Failed if !step.attachments.exists{ case (n, _) => n == "Error details"} =>
            (if (!failure.isDisabledError) {
              if (ctx.options.verbose) {
                if (ctx.options.batch) {
                  logger.error(ctx.scopes.visible.asString)
                }
                logger.error(failure.error.getMessage)
              }
              ctx.topScope.set("gwen.eval.status.keyword", failure.keyword.toString)
              ctx.topScope.set("gwen.eval.status.message", failure.message)
              ctx.addErrorAttachments(step, failure)
            } else {
              step
            }) tap { _ =>
              if (ctx.options.verbose) {
                logger.whenDebugEnabled {
                  logger.error("Exception: ", failure.error)
                }
              }
            }
          case _ => step
        }
      } else {
        step
      }
    }
    val fStep = ctx.popAttachments() match {
      case Nil => eStep
      case attachments =>
        attachments.foldLeft(eStep) { case (accStep, (attachmentNo, name, file)) =>
          accStep.addAttachment(attachmentNo, name, file)
        }
    }
    fStep.evalStatus match {
      case status @ Failed(nanos, error) =>
        if (status.isSustainedError) {
          fStep.copy(withEvalStatus = Sustained(nanos, error))
        } else if (status.isDisabledError) {
          fStep.copy(withEvalStatus = Disabled)
        } else {
          fStep
        }
      case _ =>
        fStep
    }

  }

}
