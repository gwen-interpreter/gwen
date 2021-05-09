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
import gwen.core.Formatting.DurationFormatter
import gwen.core.eval.EvalContext
import gwen.core.eval.EvalEngine
import gwen.core.eval.EvalEnvironment
import gwen.core.model._
import gwen.core.model.gherkin.Scenario
import gwen.core.model.gherkin.Step

import scala.util.Failure
import scala.util.Success
import scala.util.Try
import gwen.core.eval.binding.JavaScriptBinding
import scala.concurrent.duration.Duration

/**
  * Step evaluation engine.
  */
trait StepEngine[T <: EvalContext] {
  engine: EvalEngine[T] =>

    /**
    * Evaluates a list of steps.
    */
  private [engine] def evaluateSteps(parent: Identifiable, steps: List[Step], ctx: T): List[Step] = {
    ctx.withEnv { env =>
      var behaviorCount = 0
      try {
        steps.foldLeft(List[Step]()) {
          (acc: List[Step], step: Step) => 
            if (!StepKeyword.isAnd(step.keyword)) {
              env.addBehavior(BehaviorType.of(step.keyword))
              behaviorCount = behaviorCount + 1 
            }
            evaluateOrTransitionStep(parent, step, acc, env, ctx) :: acc
        } reverse
      } finally {
        0 until behaviorCount foreach { _ =>
          env.popBehavior()
        }
      }
    }
  }

  private def evaluateOrTransitionStep(parent: Identifiable, step: Step, acc: List[Step], env: EvalEnvironment, ctx: T): Step = {
    EvalStatus(acc.map(_.evalStatus)) match {
      case status @ Failed(_, error) =>
        ctx.evaluate(evaluateStep(parent, step, ctx)) {
          val isAssertionError = status.isAssertionError
          val isHardAssert = ctx.evaluate(false) { AssertionMode.isHard }
          if (!isAssertionError || isHardAssert) {
            lifecycle.transitionStep(parent, step, Skipped, env.scopes)
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
  def evaluateStep(parent: Identifiable, step: Step, ctx: T): Step = {
    ctx.withEnv { env =>
      val start = System.nanoTime - step.evalStatus.nanos
      val pStep = withStep(step) { ctx.interpolateParams }
      val iStep = withStep(pStep) { ctx.interpolate }
      logger.info(s"Evaluating Step: $iStep")
      lifecycle.beforeStep(parent, iStep.copy(withEvalStatus = Pending), env.scopes)
      val hStep = healthCheck(parent, iStep, env, ctx, start)
      val eStep = if (hStep == iStep) {
        translateComposite(parent, iStep, env, ctx) map { function => 
          withStep(iStep.copy(withEvalStatus = Pending)) { s =>
            function(s)
          }
        } getOrElse {
          Try(env.getStepDef(iStep.name)) match {
            case Success(stepDefOpt) =>
              stepDefOpt.filter { case (stepDef, _) => 
                !env.stepScope.containsScope(stepDef.name)
              } map { case (stepDef, params) =>
                withStep(iStep) { s =>
                  evaluateStepDef(s, stepDef.copy(), s, params, ctx)   
                }
              } getOrElse {
                Try(translate(parent, iStep, env, ctx)) match {
                  case Success(operation) => 
                    if (iStep.evalStatus.status != StatusKeyword.Failed) {
                      withStep(iStep) { s => s tap { _ => operation(s) } }
                    } else {
                      iStep
                    }
                  case Failure(e) if e.isInstanceOf[Errors.UndefinedStepException] =>
                    stepDefOpt.fold(throw e) { case (stepDef, _) =>
                      Errors.recursiveStepDefError(stepDef, iStep)
                    }
                }
              }
            case Failure(error) =>
              withStep(iStep) { throw error }
          }
        }
      } else {
        hStep
      }
      ctx.finaliseStep(eStep) tap { fStep =>
        fStep.logStatus()
        lifecycle.afterStep(fStep, env.scopes)
      }
    }
  }

  private def healthCheck(parent: Identifiable, step: Step, env: EvalEnvironment, ctx: T, startNanos: Long): Step = {
    if (step.index == 0 && (parent.isInstanceOf[Scenario] && !parent.asInstanceOf[Scenario].isStepDef)) {
      Try(lifecycle.healthCheck(parent, step, env.scopes)) match {
        case Success(_) => step
        case Failure(e) => step.copy(withEvalStatus = Failed(System.nanoTime - startNanos, e))
      }
    } else step
  }

  /**
    * Evaluates a step and captures the result.
    * 
    * @param step the step to evaluate
    * @param stepFunction the step evaluator function
    */
  private def withStep(step: Step)(stepFunction: Step => Step): Step = {
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

  def evaluateIf(step: Step, conditionalStep: String, condition: String, env: EvalEnvironment, ctx: T): Step = {
    if (condition.matches(""".*( until | while | for each | if ).*""") && !condition.matches(""".*".*((until|while|for each|if)).*".*""")) {
        Errors.illegalStepError("Nested 'if' condition found in illegal step position (only trailing position supported)")
      }
      val binding = new JavaScriptBinding(condition, ctx)
      val javascript = binding.resolve()
      env.getStepDef(conditionalStep) foreach { stepDef =>
        checkStepDefRules(step.copy(withName = conditionalStep, withStepDef = Some(stepDef)), env)
      }
      val iStep = step.copy(withEvalStatus = Pending)
      val tags = List(Tag(ReservedTags.Synthetic), Tag(ReservedTags.If), Tag(ReservedTags.StepDef))
      val iStepDef = Scenario(None, tags, ReservedTags.If.toString, condition, Nil, None, List(step.copy(withName = conditionalStep)), Nil)
      ctx.evaluate(evaluateStepDef(step, iStepDef, iStep, Nil, ctx)) {
        val satisfied = ctx.evaluateJSPredicate(ctx.interpolate(javascript))
        if (satisfied) {
          logger.info(s"Processing conditional step ($condition = true): ${step.keyword} $conditionalStep")
          evaluateStepDef(step, iStepDef, iStep, Nil, ctx)
        } else {
          logger.info(s"Skipping conditional step ($condition = false): ${step.keyword} $conditionalStep")
          step.copy(withEvalStatus = Passed(0))
        }
      }
  }

  def evaluateAnnotatedForEach(parent: Identifiable, stepDef: Scenario, step: Step, dataTable: FlatTable, env: EvalEnvironment, ctx: T): Step = {
    env.topScope.pushObject(DataTable.tableKey, dataTable)
    val doStepDef = stepDef.copy(
      withTags = stepDef.tags filter { tag => 
        tag.name != ReservedTags.ForEach.toString &&
        !tag.name.startsWith(ReservedTags.DataTable.toString)
      }
    )
    env.removeStepDef(stepDef.name)
    env.addStepDef(doStepDef)
    try {
      val records = () => {
        dataTable.records.indices.map(idx => dataTable.recordScope(idx))
      }
      evaluateForEach(records, DataTable.recordKey, parent, step, doStepDef.name, ctx)
    } finally {
      env.removeStepDef(doStepDef.name)
      env.addStepDef(stepDef)
      env.topScope.popObject(DataTable.tableKey)
    }
  }

  /**
    * Repeats a step for each element in list of elements of type U.
    */
  def evaluateForEach[U](elements: ()=>Seq[U], element: String, parent: Identifiable, step: Step, doStep: String, ctx: T): Step = {
    ctx.withEnv { env =>
      val keyword = FeatureKeyword.nameOf(FeatureKeyword.Scenario)
      val foreachSteps = elements().toList.zipWithIndex map { case (_, index) => 
        step.copy(
          withKeyword = if (index == 0) step.keyword else StepKeyword.nameOf(StepKeyword.And)
        )
      }
      val tags = List(Tag(ReservedTags.Synthetic), Tag(ReservedTags.ForEach), Tag(ReservedTags.StepDef))
      val preForeachStepDef = Scenario(None, tags, keyword, element, Nil, None, foreachSteps, Nil)
      lifecycle.beforeStepDef(step, preForeachStepDef, env.scopes)
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
                        lifecycle.transitionStep(preForeachStepDef, foreachSteps(index), Skipped, env.scopes)
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
      lifecycle.afterStepDef(foreachStepDef, env.scopes)
      step.copy(withStepDef = Some((foreachStepDef, Nil)))
    }
  }

  /**
    * Performs a repeat until or while operation 
    */
  def evaluateRepeat(operation: String, parent: Identifiable, step: Step, doStep: String, condition: String, delay: Duration, timeout: Duration, ctx: T): Step = {
    ctx.withEnv { env =>
      assert(delay.gteq(Duration.Zero), "delay cannot be less than zero")
      assert(timeout.gt(Duration.Zero), "timeout must be greater than zero")
      assert(timeout.gteq(delay), "timeout cannot be less than or equal to delay")
      val operationTag = Tag(if (operation == "until") ReservedTags.Until else ReservedTags.While)
      val tags = List(Tag(ReservedTags.Synthetic), operationTag, Tag(ReservedTags.StepDef))
      val preCondStepDef = Scenario(None, tags, operationTag.name, condition, Nil, None, Nil, Nil)
      var condSteps: List[Step] = Nil
      var evaluatedStep = step
      val start = System.nanoTime()
      ctx.perform {
        var iteration = 0
        try {
          ctx.waitUntil(timeout.toSeconds, s"trying to repeat: ${step.name}") {
            iteration = iteration + 1
            env.topScope.set("iteration number", iteration.toString)
            val preStep = step.copy(withKeyword = if(iteration == 1) step.keyword else StepKeyword.And.toString, withName = doStep)
            operation match {
              case "until" =>
                logger.info(s"repeat-until $condition: iteration $iteration")
                if (condSteps.isEmpty) {
                  lifecycle.beforeStepDef(step, preCondStepDef, env.scopes)
                }
                val iterationStep = evaluateStep(preCondStepDef, preStep, ctx)
                condSteps = iterationStep :: condSteps
                iterationStep.evalStatus match {
                  case Failed(_, e) => throw e
                  case _ =>
                    val javascript = ctx.interpolate(env.scopes.get(JavaScriptBinding.key(condition)))
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
                val javascript = ctx.interpolate(env.scopes.get(JavaScriptBinding.key(condition)))
                val result = ctx.evaluateJSPredicate(javascript)
                if (result) {
                  logger.info(s"repeat-while $condition: iteration $iteration")
                  if (condSteps.isEmpty) {
                    lifecycle.beforeStepDef(step, preCondStepDef, env.scopes)
                  }
                  val iterationStep = evaluateStep(preCondStepDef, preStep, ctx)
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
            logger.error(e.getMessage)
            val nanos = System.nanoTime() - start
            val durationNanos = {
              if (nanos > timeout.toNanos) timeout.toNanos
              else nanos
            }
            evaluatedStep = step.copy(withEvalStatus = Failed(durationNanos, Errors.stepError(step, e)))
        } finally {
          env.topScope.set("iteration number", null)
        }
      } getOrElse {
        try {
          operation match {
            case "until" =>
              evaluatedStep = this.evaluateStep(step, step.copy(withName = doStep), ctx)
              env.scopes.get(JavaScriptBinding.key(condition))
            case _ =>
              env.scopes.get(JavaScriptBinding.key(condition))
              evaluatedStep = this.evaluateStep(step, step.copy(withName = doStep), ctx)
          }
        } catch {
          case _: Throwable => 
            // ignore in dry run mode
        }
      }
      if (condSteps.nonEmpty) {
        val steps = evaluatedStep.evalStatus match {
          case Failed(nanos, error) if (EvalStatus(condSteps.map(_.evalStatus)).status == StatusKeyword.Passed) => 
            val preStep = condSteps.head.copy(withKeyword = StepKeyword.And.toString, withName = doStep)
            lifecycle.beforeStep(preCondStepDef, preStep, env.scopes)
            val fStep = ctx.finaliseStep(
              preStep.copy(
                withEvalStatus = Failed(nanos - condSteps.map(_.evalStatus.nanos).sum, error),
                withStepDef = None
              )
            )
            lifecycle.afterStep(fStep, env.scopes)
            fStep :: condSteps
          case _ => 
            condSteps
            
        }
        val condStepDef = preCondStepDef.copy(withSteps = steps.reverse)
        lifecycle.afterStepDef(condStepDef, env.scopes)
        evaluatedStep.copy(
          withEvalStatus = condStepDef.evalStatus,
          withStepDef = Some((condStepDef, Nil)),
          withAttachments = condStepDef.attachments
        )
      } else {
        evaluatedStep
      }
    }
  }
  
}
