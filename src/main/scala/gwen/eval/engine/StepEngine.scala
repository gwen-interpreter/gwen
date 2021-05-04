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

package gwen.eval.engine

import gwen._
import gwen.eval.EvalContext
import gwen.eval.EvalEngine
import gwen.model._
import gwen.model.gherkin.Scenario
import gwen.model.gherkin.Step

import scala.util.Failure
import scala.util.Success
import scala.util.Try

/**
  * Step evaluation engine.
  */
trait StepEngine[T <: EvalContext] {
  engine: EvalEngine[T] =>

  /**
    * Must be implemented to define the default composite steps supported by all engines.
    *
    * @param step the step to evaluate
    * @param ctx the evaluation context
    */
  def evaluateComposite(parent: Identifiable, step: Step, ctx: T): Option[Step]

  /**
    * Must be implemented to define the default steps supported by all engines.
    *
    * @param step the step to evaluate
    * @param ctx the evaluation context
    * @throws gwen.Errors.UndefinedStepException if the given step is undefined
    *         or unsupported
    */
  def evaluate(step: Step, ctx: T): Unit

    /**
    * Evaluates a list of steps.
    */
  def evaluateSteps(parent: Identifiable, steps: List[Step], ctx: T): List[Step] = ctx.withEnv { env =>
    var behaviorCount = 0
    try {
      steps.foldLeft(List[Step]()) {
        (acc: List[Step], step: Step) => 
          if (!StepKeyword.isAnd(step.keyword)) {
            env.addBehavior(BehaviorType.of(step.keyword))
            behaviorCount = behaviorCount + 1 
          }
          (EvalStatus(acc.map(_.evalStatus)) match {
            case status @ Failed(_, error) =>
              ctx.evaluate(evaluateStep(parent, step, ctx)) {
                val isAssertionError = status.isAssertionError
                val isHardAssert = ctx.evaluate(false) { AssertionMode.isHard }
                if (!isAssertionError || isHardAssert) {
                  ctx.lifecycle.transitionStep(parent, step, Skipped, env.scopes)
                } else {
                  evaluateStep(parent, step, ctx)
                }
              }
            case _ => evaluateStep(parent, step, ctx)
          }) :: acc
      } reverse
    } finally {
      0 until behaviorCount foreach { _ =>
        env.popBehavior()
      }
    }
  }

  /**
    * Evaluates a step.
    */
  def evaluateStep(parent: Identifiable, step: Step, ctx: T): Step = ctx.withEnv { env =>
    val start = System.nanoTime - step.evalStatus.nanos
    val ipStep = withStep(step) { ctx.interpolateParams }
    val iStep = withStep(ipStep) { ctx.interpolate }
    var cStep: Option[Step] = None
    logger.info(s"Evaluating Step: $iStep")
    ctx.lifecycle.beforeStep(parent, iStep, env.scopes)
    val hStep = if (step.index == 0 && (parent.isInstanceOf[Scenario] && !parent.asInstanceOf[Scenario].isStepDef)) {
      Try(ctx.lifecycle.healthCheck(parent, iStep, env.scopes)) match {
        case Success(_) => iStep
        case Failure(e) => iStep.copy(withEvalStatus = Failed(System.nanoTime - start, e))
      }
    } else iStep
    val eStep = if (hStep != iStep) {
      hStep
    } else {
      withStep(iStep) { s =>
        cStep = evaluateComposite(parent, s, ctx)
        cStep.getOrElse(s)
      }
      val hasSynthetic = cStep.flatMap(s => s.stepDef map { case (sd, _) => sd.isSynthetic }).getOrElse(false)
      cStep.filter(_.evalStatus.status != StatusKeyword.Failed || hasSynthetic).getOrElse {
        if (iStep.evalStatus.status != StatusKeyword.Failed) {
          Try(env.getStepDef(iStep.name)) match {
            case Failure(error) =>
              iStep.copy(withEvalStatus = Failed(System.nanoTime - start, new Errors.StepFailure(iStep, error)))
            case Success(stepDefOpt) =>
              (stepDefOpt match {
                case Some((stepDef, _)) if env.stepScope.containsScope(stepDef.name) => None
                case stepdef => stepdef
              }) match {
                case None =>
                  withStep(iStep) { step =>
                    step tap { _ =>
                      try {
                        evaluate(step, ctx)
                      } catch {
                        case e: Errors.UndefinedStepException =>
                          e.printStackTrace()
                          stepDefOpt.fold(throw e) { case (stepDef, _) =>
                            Errors.recursiveStepDefError(stepDef, step)
                          }
                      }
                    }
                  }
                case (Some((stepDef, params))) =>
                  if (stepDefSemaphors.containsKey(stepDef.name)) {
                    val semaphore = stepDefSemaphors.get(stepDef.name)
                    semaphore.acquire()
                    try {
                      logger.info(s"Synchronized StepDef execution started [StepDef: ${stepDef.name}] [thread: ${Thread.currentThread().getName}]")
                      evalStepDef(iStep, stepDef.copy(), iStep, params, ctx)
                    } finally {
                      logger.info(s"Synchronized StepDef execution finished [StepDef: ${stepDef.name}] [thread: ${Thread.currentThread().getName}]")
                      semaphore.release()
                    }
                  } else {
                    evalStepDef(iStep, stepDef.copy(), iStep, params, ctx)
                  }
              }
          }
        } else {
          iStep
        }
      }
    }
    val fStep = eStep.evalStatus match {
      case Failed(_, e: Errors.StepFailure) if e.getCause != null && e.getCause.isInstanceOf[Errors.UndefinedStepException] =>
        cStep.getOrElse(eStep)
      case _ =>
        eStep.evalStatus match {
          case Passed(_) => eStep
          case _ => cStep.filter(s => EvalStatus.isEvaluated(s.evalStatus.status)).getOrElse(eStep)
        }
    }
    ctx.finaliseStep(fStep) tap { step =>
      step.logStatus()
      ctx.lifecycle.afterStep(step, env.scopes)
    }
  }

  /**
    * Evaluates a step and captures the result.
    * 
    * @param step the step to evaluate
    * @param stepFunction the step evaluator function
    */
  def withStep(step: Step)(stepFunction: Step => Step): Step = {
    val start = System.nanoTime - step.evalStatus.nanos
    Try(stepFunction(step)) match {
      case Success(eStep) =>
        val status = eStep.stepDef map { case (sd, _) => sd.evalStatus }  getOrElse {
          eStep.evalStatus match {
            case Failed(_, error) => Failed(System.nanoTime - start, error)
            case _ => Passed(System.nanoTime - start)
          }
        }
        eStep.copy(withEvalStatus = status)
      case Failure(error) =>
        val failure = Failed(System.nanoTime - start, new Errors.StepFailure(step, error))
        step.copy(withEvalStatus = failure)
    }
  }

  /**
    * Repeats a step for each element in list of elements of type U.
    */
  def foreach[U](elements: ()=>Seq[U], element: String, parent: Identifiable, step: Step, doStep: String, ctx: T): Step = ctx.withEnv { env =>
    val keyword = FeatureKeyword.nameOf(FeatureKeyword.Scenario)
    val foreachSteps = elements().toList.zipWithIndex map { case (_, index) => 
      step.copy(
        withName = doStep.replaceAll(s"$ZeroChar", ""),
        withKeyword = if (index == 0) step.keyword else StepKeyword.nameOf(StepKeyword.And)
      )
    }
    val tags = List(Tag(ReservedTags.Synthetic), Tag(ReservedTags.ForEach), Tag(ReservedTags.StepDef))
    val preForeachStepDef = Scenario(None, tags, keyword, element, Nil, None, foreachSteps, Nil)
    ctx.lifecycle.beforeStepDef(step, preForeachStepDef, env.scopes)
    val steps =
      elements() match {
        case Nil =>
          logger.info(s"For-each[$element]: none found")
          Nil
        case elems =>
          val noOfElements = elems.size
          logger.info(s"For-each[$element]: $noOfElements found")
          try {
            if(Try(ctx.getBoundReferenceValue(element)).isSuccess) {
              Errors.ambiguousCaseError(s"For-each element name '$element' already bound (use a free name instead)")
            }
            elems.zipWithIndex.foldLeft(List[Step]()) { case (acc, (currentElement, index)) =>
              val elementNumber = index + 1
              currentElement match {
                case stringValue: String =>
                  env.topScope.set(element, stringValue)
                  if (ctx.options.dryRun) {
                    env.topScope.pushObject(element, currentElement)
                  }
                case _ =>
                  env.topScope.pushObject(element, currentElement)
              }
              env.topScope.set(s"$element index", index.toString)
              env.topScope.set(s"$element number", elementNumber.toString)
              (try {
                EvalStatus(acc.map(_.evalStatus)) match {
                  case status @ Failed(_, error)  =>
                    val isAssertionError = status.isAssertionError
                    val isSoftAssert = ctx.evaluate(false) { isAssertionError && AssertionMode.isSoft }
                    val failfast = ctx.evaluate(false) { GwenSettings.`gwen.feature.failfast` }
                    if (failfast && !isSoftAssert) {
                      logger.info(s"Skipping [$element] $elementNumber of $noOfElements")
                      ctx.lifecycle.transitionStep(preForeachStepDef, foreachSteps(index), Skipped, env.scopes)
                    } else {
                      logger.info(s"Processing [$element] $elementNumber of $noOfElements")
                      evaluateStep(preForeachStepDef, Step(step.sourceRef, if (index == 0) step.keyword else StepKeyword.nameOf(StepKeyword.And), doStep, Nil, None, Nil, None, Pending), ctx)
                    }
                  case _ =>
                    logger.info(s"Processing [$element] $elementNumber of $noOfElements")
                    evaluateStep(preForeachStepDef, Step(step.sourceRef, if (index == 0) step.keyword else StepKeyword.nameOf(StepKeyword.And), doStep, Nil, None, Nil, None, Pending), ctx)
                }
              } finally {
                env.topScope.popObject(element)
              }) :: acc
            } reverse
          } finally {
            env.topScope.set(element, null)
            env.topScope.set(s"$element index", null)
            env.topScope.set(s"$element number", null)
          }
      }
    val foreachStepDef = preForeachStepDef.copy(withSteps = steps)
    ctx.lifecycle.afterStepDef(foreachStepDef, env.scopes)
    step.copy(withStepDef = Some((foreachStepDef, Nil)))
  }
  
}
