/*
 * Copyright 2014-2015 Branko Juric, Brady Wood
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

package gwen.eval

import java.io.File
import gwen.Predefs.Exceptions
import gwen.Predefs.FileIO
import gwen.Predefs.Kestrel
import gwen.dsl.Failed
import gwen.dsl.Scenario
import gwen.dsl.Step
import play.api.libs.json.JsObject
import play.api.libs.json.Json
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import gwen.dsl.EvalStatus
import gwen.dsl.Pending
import gwen.dsl.SpecType
import com.typesafe.scalalogging.slf4j.LazyLogging
import scala.collection.mutable.Stack
import gwen.eval.support.InterpolationSupport
import gwen.errors._
import gwen.Settings
import scala.util.Try
import gwen.dsl.Tag

/**
  * Base environment context providing access to all resources and services to 
  * engines.  Specific [[EvalEngine evaluation engines]] can 
  * define and use their own specific context by extending this one. 
  * 
  * Access to page scope data is provided through a dataScope method.
  * 
  * @author Branko Juric
  */
class EnvContext(options: GwenOptions, scopes: ScopedDataStack) extends LazyLogging with ExecutionContext with InterpolationSupport {
  
  /** Map of step definitions keyed by callable expression name. */
  private var stepDefs = Map[String, Scenario]()
  
  /** List of current attachments (name-file pairs). */
  private var currentAttachments: List[(String, File)] = Nil
  private var attachementCount = 0
  
  /** The current type of specification being interpreted. */
  var specType = SpecType.feature
  
  /** Provides access to the global feature scope. */
  def featureScope = scopes.featureScope
  
  /** Provides access to the local scope. */
  private[eval] val localScope = scopes.localScope
  
  /**
    * Closes any resources associated with the evaluation context. This implementation
    * does nothing (but subclasses can override).
    */
  def close() { }
  
  /** Resets the current context but does not close it so it can be reused. */
  def reset() {
    scopes.reset()
    stepDefs = Map[String, Scenario]()
    resetAttachments
    attachementCount = 0
  }
    
  def json: JsObject = scopes.json
  
  /** Returns the current visible scopes. */  
  def visibleScopes: ScopedDataStack = scopes.visible
  
  /**
   * Filters all attributes in all scopes based on the given predicate.
   * 
   * @param pred the predicate filter to apply; a (name, value) => boolean function
   * @return a new Scoped data stack containing only the attributes accepted by the predicate; 
   */
  def filterAtts(pred: ((String, String)) => Boolean): ScopedDataStack = scopes.filterAtts(pred) 
  
  /**
    * Gets a named data scope (creates it if it does not exist)
    * 
    * @param name the name of the data scope to get (or create and get)
    */
  def addScope(name: String) = scopes.addScope(name)
  
  /**
    * Adds a step definition to the context.
    * 
    * @param stepDef the step definition to add
    */
  def addStepDef(stepDef: Scenario) {
    val tags = stepDef.metaFile.map(meta => stepDef.tags + Tag(s"""Meta("${meta.getPath()}")""")).getOrElse(stepDef.tags)
    stepDefs += ((stepDef.name, Scenario(tags, stepDef.name, stepDef.background, stepDef.steps, stepDef.metaFile))) 
  }
  
  /**
    * Gets the executable step definition for the given expression (if there is
    * one).
    * 
    * @param expression the expression to match
    * @return the step definition if a match is found; false otherwise
    */
  def getStepDef(expression: String): Option[(Scenario, List[(String, String)])] = 
    stepDefs.get(expression) match {
      case None => getStepDefWithParams(expression)
      case Some(stepDef) => Some((stepDef, Nil))
    }
  
  /**
    * Gets the paraterised step definition for the given expression (if there is
    * one).
    * 
    * @param expression the expression to match
    * @return the step definition and its parameters (name value tuples) if a 
    *         match is found; false otherwise
    */
  private def getStepDefWithParams(expression: String): Option[(Scenario, List[(String, String)])] = {
    stepDefs.values.view.flatMap { stepDef =>
      ("<.+?>".r.findAllIn(stepDef.name).toList match {
        case Nil => None  
        case names =>
          names.groupBy(identity).collectFirst { case (n, vs) if (vs.size > 1) =>
            ambiguousCaseError(s"$n parameter defined ${vs.size} times in StepDef '${stepDef.name}'")
          } 
          (stepDef.name.split(names.mkString("|")).toList match { 
            case tokens if (tokens.forall(expression.contains(_))) => 
              tokens.foldLeft((expression, List[String]())) { case ((expr, acc), token ) => 
                expr.indexOf(token) match { 
                  case -1 => ("", Nil) 
                  case 0 => (expr.substring(token.length), acc) 
                  case idx => (expr.substring(idx + token.length), expr.substring(0, idx)::acc)
                } 
              } 
            case _ => ("", Nil)
          }) match { 
            case (value, values) =>
              val params = names zip (if (value != "") value::values else values).reverse
              if (expression == params.foldLeft(stepDef.name) { (result, param) => result.replaceAll(param._1, param._2) }) {
                Some(stepDef, params)
              } else None
          }
      })
    }.collectFirst { case (stepDef, params) => 
        logger.debug(s"Mapped $expression to StepDef: ${stepDef.name} { ${(params.map { case (n, v) => s"$n=$v"}).mkString(", ")} }")
      (stepDef, params)
    }}
  
  /**
   * Gets the list of DSL steps supported by this context.  This implementation 
   * returns all user defined stepdefs. Subclasses can override to return  
   * addtional entries. The entries returned by this method are used for tab 
   * completion in the REPL.
   */
  def dsl: List[String] = stepDefs.keys.toList
  
  /**
    * Fail handler.
    * 
    * @param failed the failed status
    */
  final def fail(failure: Failed): Unit = { 
    addErrorAttachments(failure)
    logger.error(Json.prettyPrint(this.scopes.visible.json))
    logger.error(failure.error.getMessage())
    logger.debug(s"Exception: ", failure.error)
  }
  
  /**
    * Adds error attachments to the current context.
    * 
    * @param failed the failed status
    */
  def addErrorAttachments(failure: Failed): Unit = { 
    addAttachment("Error details", "txt", failure.error.writeStackTrace())
    addAttachment("Environment (all)", "json", Json.prettyPrint(this.scopes.json))
    addAttachment("Environment (visible)", "json", Json.prettyPrint(this.scopes.visible.json))
  }
  
  /**
   * Creates an attachment file.
   * 
   * @param name the attachment name
   * @param extension the filename extension
   * @param content the content to write to the file
   */
  def addAttachment(name: String, extension: String, content: String): (String, File) = {
      attachementCount = attachementCount + 1
      val file = File.createTempFile(s"${"%04d".format(attachementCount)}-${name.replaceAll("[^A-Za-z0-9]", "-")}-", s".${extension}") tap { f =>
        f.deleteOnExit()
        Option(content) foreach { f.writeText }
        currentAttachments = ((name, f)) :: currentAttachments
      }
      (name, file)
  }
  
  /**
    * Adds an attachment to the current context.
    * 
    * @param attachment the attachment (name-file pair) to add
    * @param file the attachment fileß
    */
  def addAttachment(attachment: (String, File)): Unit = {
    currentAttachments = attachment :: currentAttachments
  } 
  
  /** Resets/clears current attachments. */
  private[eval] def resetAttachments() {
    currentAttachments = Nil
  }
  
  /** Gets the list of attachments (sorted by file name).*/
  def attachments = currentAttachments.sortBy(_._2 .getName())
  
  /**
    * Interpolate the given step before it is evaluated.
    * 
    * @param step the step to interpolate
    * @return the interpolated step
    */
  def interpolate(step: Step): Step = 
    if (SpecType.feature.equals(specType)) {
      interpolate(step.expression) { name =>
        Try(localScope.get(name)).getOrElse(getBoundReferenceValue(name)) 
      } match {
        case step.expression => step
        case expr =>
          Step(step, expr) tap { iStep => logger.debug(s"Interpolated ${step.expression} to: ${iStep.expression}") }
      }
    } else {
      step
    }
  
  /**
    * Gets the scoped attribute or settings value bound to the given name.
    * Subclasses can override this method to perform additional lookups.
    * 
    *  @param name the name of the attribute or value
    *  @throws `gwen.errors.UnboundAttributeException` if no value is bound 
    *          to the given name 
    */
  def getBoundReferenceValue(name: String): String = {
    scopes.getOpt(name) match {
      case Some(value) => value
      case _ => Settings.getOpt(name) match { 
        case Some(value) => value
        case _ => 
          unboundAttributeError(name)
      }
    }
  }
  
  val isDryRun = options.dryRun
  
}

/** Merges two contexts into one. */
class HybridEnvContext[A <: EnvContext, B <: EnvContext](val envA: A, val envB: B, val options: GwenOptions, val scopes: ScopedDataStack) extends EnvContext(options, scopes) {
  override def dsl = envA.dsl ++ envB.dsl
  override def close() {
    try {
      envB.close()
    } finally {
      envA.close()
    }
  }
  override def getBoundReferenceValue(name: String) = 
    try {
      envB.getBoundReferenceValue(name)
    } catch {
      case _: Throwable => envA.getBoundReferenceValue(name)
    }
}