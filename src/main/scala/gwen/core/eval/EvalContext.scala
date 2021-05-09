/*
 * Copyright 2015-2021 Branko Juric, Brady Wood
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
import gwen.core.eval.binding.Binding
import gwen.core.eval.binding.BindingResolver
import gwen.core.eval.support._
import gwen.core.model.gherkin.Step

import scala.io.Source
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import java.io.File
import java.io.FileNotFoundException
import gwen.core.model.Failed
import gwen.core.model.StateLevel

import org.apache.log4j.PropertyConfigurator

import java.net.URL
import java.util.concurrent.Semaphore

/**
  * Provides all evaluation capabilities.
  */
class EvalContext(val options: GwenOptions, env: EvalEnvironment) extends InterpolationSupport with RegexSupport with XPathSupport with JsonPathSupport
  with SQLSupport with ScriptSupport with DecodingSupport with TemplateSupport {

  Settings.getOpt("log4j.configuration").orElse(Settings.getOpt("log4j.configurationFile")).foreach { config => 
    if (config.toLowerCase.trim startsWith "file:") {
      PropertyConfigurator.configure(new URL(config));
    } else {
      PropertyConfigurator.configure(config); 
    }
  }

  // resolves locator bindings
  private val bindingResolver = new BindingResolver(this)

  def close(): Unit = { 
    env.close()
  }

  /** Resets the context for the given state level. */
  def reset(level: StateLevel.Value): Unit = {
    env.reset(level)
  }

  /**
   * Gets the list of DSL steps supported by this context.  This implementation 
   * returns all user defined stepdefs. Subclasses can override to return  
   * addtional entries. The entries returned by this method are used for tab 
   * completion in the REPL.
   */
  def dsl: List[String] = env.stepDefs.keys.toList

  /** 
   * Providesa a function with access to the environment context.
   */
  def withEnv[U](function: EvalEnvironment => U): U = function(env)

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
  def getBoundReferenceValue(name: String): String = getBinding(name).resolve()

  /**
    * Gets a named binding
    *
    * @param name the binding name
    * @return a binding
    */
  def getBinding(name: String): Binding[EvalContext, String] = bindingResolver.getBinding(name)
  
  def interpolate(value: String): String = interpolateString(value)(getBoundReferenceValue)

  /**
    * Interpolate all parameters in the given step.
    * 
    * @param step the step to interpolate
    * @return the interpolated step
    */
  def interpolateParams(step: Step): Step = interpolate(step, interpolateParams)

  /**
    * Interpolate all references in the given step.
    * 
    * @param step the step to interpolate
    * @return the interpolated step
    */
  def interpolate(step: Step): Step = interpolate(step, interpolateString)

  private def interpolate(step: Step, interpolator: String => (String => String) => String): Step = {
    val resolver: String => String = name => Try(env.stepScope.get(name)).getOrElse(getBoundReferenceValue(name))
    val iName = interpolator(step.name) { resolver }
    val iTable = step.table map { case (line, record) =>
      (line, record.map(cell => interpolator(cell) { resolver }))
    }
    val iDocString = step.docString map { case (line, content, contentType) =>
      (line, interpolator(content) { resolver }, contentType)
    }
    if (iName != step.name || iTable != step.table || iDocString != step.docString) {
      step.copy(
        withName = iName,
        withTable = iTable,
        withDocString = iDocString
      ) tap { iStep =>
        logger.debug(s"Interpolated ${step.name} to: ${iStep.expression}${if (iTable.nonEmpty) ", () => dataTable" else ""}")
      }
    } else step
  }

  def compare(sourceName: String, expected: String, actual: String, operator: String, negate: Boolean): Try[Boolean] = Try {
    val res = operator match {
      case "be"      => expected == actual
      case "contain" => actual.contains(expected)
      case "start with" => actual.startsWith(expected)
      case "end with" => actual.endsWith(expected)
      case "match regex" => actual.matches(expected)
      case "match xpath" => !evaluateXPath(expected, actual, XMLNodeType.text).isEmpty
      case "match json path" => !evaluateJsonPath(expected, actual).isEmpty
      case "match template" | "match template file" =>
        matchTemplate(expected, actual, sourceName, env.topScope) match {
          case Success(result) =>
            if (negate) Errors.templateMatchError(s"Expected $sourceName to not match template but it did") else result
          case Failure(failure) =>
            if (negate) false else throw failure
        }
    }
    if (!negate) res else !res
  }

  def parseExpression(operator: String, expression: String): String = {
    (if (operator == "match template file") {
      val filepath = interpolate(expression)
      if (new File(filepath).exists()) {
        interpolate(Source.fromFile(filepath).mkString)
      } else throw new FileNotFoundException(s"Template file not found: $filepath")
    } else {
      expression
    }) tap { expr =>
      if (options.dryRun && operator.startsWith("match template")) {
        """@\{.*?\}""".r.findAllIn(expr).toList foreach { name =>
          env.topScope.set(name.substring(2, name.length - 1), "[dryRun:templateExtract]")
        }
      }
    }
  }

  /**
    * Adds error attachments to the current context. This includes the error trace and environment context.
    * 
    * @param failure the failed status
    */
  def addErrorAttachments(failure: Failed): Unit = { 
    env.addErrorAttachments(failure)
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
    val lock = new Semaphore(1)
    lock.acquire()
    val start = System.currentTimeMillis
    while(lock.availablePermits < 1 && (System.currentTimeMillis - start) < timeoutSecs) { 
      if (condition) lock.release()
      lock.tryAcquire()
    }
    try {
      if (lock.availablePermits < 1) {
        Errors.waitTimeoutError(timeoutSecs, reason)
      }
    } finally {
      lock.release()
    }
  }

}