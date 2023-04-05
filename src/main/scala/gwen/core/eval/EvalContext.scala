/*
 * Copyright 2015-2023 Branko Juric, Brady Wood
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

package gwen.core.eval

import gwen.core._
import gwen.core.eval.ComparisonOperator
import gwen.core.eval.binding.Binding
import gwen.core.eval.binding.BindingResolver
import gwen.core.eval.binding.LoadStrategyBinding
import gwen.core.eval.support._
import gwen.core.Errors.UnboundAttributeException
import gwen.core.node.gherkin.Step
import gwen.core.state.Environment
import gwen.core.state.EnvState
import gwen.core.status._

import scala.io.Source
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import scala.util.chaining._

import java.io.File
import java.io.FileNotFoundException

import java.net.URL

/**
  * Provides all evaluation capabilities.
  */
class EvalContext(val options: GwenOptions, envState: EnvState)
    extends Environment(envState) with RegexSupport with XPathSupport with JsonPathSupport
    with SQLSupport with ScriptSupport with DecodingSupport with TemplateSupport with SimilaritySupport with SysProcSupport {

  // resolves locator bindings
  private val bindingResolver = new BindingResolver(this)

  // Interpolator for resolving $<param> and ${property} references
  private def interpolator: Interpolator = new Interpolator(name => {
    paramScope.getOpt(name) orElse { 
      try {
        Option(getBoundReferenceValue(name))
      } catch {
        case _: UnboundAttributeException => None
      }
    }
  })

  // Interpolator for resolving $<param> references only
  private def paramInterpolator: Interpolator = new Interpolator(paramScope.getOpt)

  /** 
   * Interpolates a given string by resolving all $<param> and ${property} references. 
   * 
   * @param source the string to interpolate
   */
  def interpolate(source: String): String = interpolator.interpolate(source)

  /** 
   * Interpolates a given string by resolving all $<param> and ${property} references. 
   * 
   * @param source the string to interpolate
   */
  def interpolateLenient(source: String): String = interpolator.lenient.interpolate(source)

  /** 
   * Interpolates a given string by resolving all $<param> references only. 
   * 
   * @param source the string to interpolate
   */
  def interpolateParams(source: String): String = paramInterpolator.interpolateParams(source)

  /**
   * Gets the list of DSL steps supported by this context.  This implementation
   * returns all user defined stepdefs. Subclasses can override to return
   * addtional entries. The entries returned by this method are used for tab
   * completion in the REPL.
   */
  def dsl: List[String] = stepDefs.keys.toList

  /**
   * Evaluates an function or returns the given dry value depending on--dry-run mode.
   *
   * @param dryValue the dry value
   * @param function the function to evaluate
   */
  def evaluate[U](dryValue: => U)(function: => U): U = if (!options.dryRun) function else dryValue

  /**
   * Performs an operation only if --dry-run mode is off.
   *
   * @param operation the operation to execute
   */
  def perform(operation: => Unit): Option[Any] = if (!options.dryRun) Some(operation) else None

  /**
   * Gets get value bound to the given name.
   *  @param name the name of the attribute or value
   */
  def getBoundReferenceValue(name: String): String = {
    getBinding(name).resolve()
  }

  /**
    * Gets a named binding
    *
    * @param name the binding name
    * @return a binding
    */
  def getBinding(name: String): Binding[EvalContext, String] = bindingResolver.getBinding(name)

  def compare(sourceName: String, expected: String, actual: String, operator: ComparisonOperator, negate: Boolean): Try[Boolean] = Try {
    val res = operator match {
      case ComparisonOperator.be => expected == actual
      case ComparisonOperator.contain => actual.contains(expected)
      case ComparisonOperator.`start with` => actual.startsWith(expected)
      case ComparisonOperator.`end with` => actual.endsWith(expected)
      case ComparisonOperator.`match regex` => actual.matches(expected)
      case ComparisonOperator.`match xpath` => !evaluateXPath(expected, actual, XMLNodeType.text).isEmpty
      case ComparisonOperator.`match json path` => !evaluateJsonPath(expected, actual).isEmpty
      case ComparisonOperator.`match template` | ComparisonOperator.`match template file` =>
        matchTemplate(expected, actual, sourceName, topScope) match {
          case Success(result) =>
            if (negate) Errors.templateMatchError(s"Expected $sourceName to not match template but it did") else result
          case Failure(failure) =>
            if (negate) false else throw failure
        }
    }
    if (!negate) res else !res
  }

  def checkSimilarity(value1: String, value2: String, operator: SimilarityOperator, percentage: Double, negate: Boolean): Try[(Boolean, Option[Double])] = Try {
    val percentageBD =  BigDecimal(percentage).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
    dscSimilarity(value1, value2) map { score => 
      topScope.set("similarity score", score.toString)
      val percentageScoreBD = BigDecimal(score * 100).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
      val res = operator match {
        case SimilarityOperator.be =>
          percentageScoreBD == percentageBD
        case SimilarityOperator.`be less than` =>
          percentageScoreBD < percentageBD
        case SimilarityOperator.`be at most` =>
          percentageScoreBD <= percentageBD
        case SimilarityOperator.`be more than` =>
          percentageScoreBD > percentageBD
        case SimilarityOperator.`be at least` =>
          percentageScoreBD >= percentageBD
      }
      (if (!negate) res else !res, Some(score))
    } getOrElse {
      if (topScope.getOpt("similarity score").nonEmpty) {
        topScope.set("similarity score", null)
      }
      (negate, None)
    }
  }

  def parseExpression(operator: ComparisonOperator, expression: String): String = {
    (if (operator == ComparisonOperator.`match template file`) {
      val filepath = interpolate(expression)
      if (new File(filepath).exists()) {
        interpolate(Source.fromFile(filepath).mkString)
      } else throw new FileNotFoundException(s"Template file not found: $filepath")
    } else {
      expression
    }) tap { expr =>
      if (options.dryRun && operator.toString.startsWith("match template")) {
        """@\{.*?\}""".r.findAllIn(expr).toList foreach { name =>
          topScope.set(name.substring(2, name.length - 1), "[dryRun:templateExtract]")
        }
      }
    }
  }

  /**
    * Waits until a given condition is ready for a given number of seconds.
    * Errors on given timeout out seconds. Checks condition every 1 second.
    *
    * @param timeoutSecs the number of seconds to wait before timing out
    * @param reason a description of what is being waited on
    * @param condition the boolean condition to wait for (until true)
    */
  def waitUntil(timeoutSecs: Long, reason: String)(condition: => Boolean): Unit = {
    Wait.waitUntil(timeoutSecs, reason) { condition }
  }

  /**
    * Applies a function to a step and captures the result.
    *
    * @param step the step to evaluate
    * @param stepFunction the step evaluator function
    */
  def withStep(step: Step)(stepFunction: Step => Step): Step = {
    val start = System.nanoTime - step.evalStatus.nanos
    Try(stepFunction(step)) match {
      case Success(eStep) =>
        val status = eStep.stepDef map { sd => sd.evalStatus }  getOrElse {
          eStep.evalStatus match {
            case Failed(_, error) => Failed(System.nanoTime - start, error)
            case Disabled => Disabled
            case _: Ignored => Ignored(System.nanoTime - start)
            case p: Passed => Passed(System.nanoTime - start, p.abstained)
            case _ => Passed(System.nanoTime - start)
          }
        }
        eStep.copy(withEvalStatus = status)
      case Failure(error) =>
        val failure = Failed(System.nanoTime - start, new Errors.StepException(step, error.getMessage, error))
        step.copy(withEvalStatus = failure)
    }
  }

  /**
    * Adds error attachments to the given step.
    * This includes the error trace and environment context.
    *
    * @param failure the failed status
    */
  def addErrorAttachments(step: Step, failure: Failed): Step = {
    step
      .addAttachment("Error details", "txt", failure.error.writeStackTrace())
      .addAttachment(s"Environment", "txt", scopes.visible.asString)
  }

  def assertWithError(assertion: Boolean, customError: Option[String], assertError: String): Unit = {
    customError map { msg => 
      if (!assertion) Errors.customAssertionError(Try(interpolateLenient(msg)).getOrElse(msg))
    } getOrElse {
      assert(assertion, assertError)
    }
  }

}
