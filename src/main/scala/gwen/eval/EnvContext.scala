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


/**
  * Base environment context providing access to all resources and services to 
  * engines.  Specific [[EvalEngine evaluation engines]] can 
  * define and use their own specific context by extending this one. 
  * 
  * Access to page scope data is provided through a dataScope method.
  * 
  * @author Branko Juric
  */
class EnvContext(scopes: ScopedDataStack) extends LazyLogging {
  
  /** Map of step definitions keyed by callable expression name. */
  private var stepDefs = Map[String, Scenario]()
  
  /** List of current attachments (name-file pairs). */
  private var _attachments: List[(String, File)] = Nil
  
  /** The current type of specification being interpreted. */
  var specType = SpecType.feature
  
  /** Provides access to the global feature scope. */
  def featureScope = scopes.featureScope
  
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
  }
    
  def json: JsObject = scopes.json
  
  /** Returns the current visible attributes as a Json object. */  
  def visible: ScopedDataStack = scopes.visible
  
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
    stepDefs += ((stepDef.name, stepDef)) 
  }
  
  /**
    * Gets the executable step definition for the given expression (if there is
    * one).
    * 
    * @param expression the expression to match
    * @return the step definition if a match is found; false otherwise
    */
  def getStepDef(expression: String): Option[Scenario] = stepDefs.get(expression)
  
  /**
    * Fail handler.
    * 
    * @param failed the failed status
    */
  final def fail(failure: Failed): Unit = { 
    _attachments = createErrorAttachments(failure)
    logger.error(Json.prettyPrint(this.scopes.visible.json))
    logger.error(failure.error.getMessage())
    logger.debug(s"Exception: ", failure.error)
  }
  
  /**
    * Adds error attachments to the current context.
    * 
    * @param failed the failed status
    */
  def createErrorAttachments(failure: Failed): List[(String, File)] = List( 
    ("Error details", 
      File.createTempFile("error-details-", ".txt") tap { f =>
        f.deleteOnExit()
        f.writeText(failure.error.writeStackTrace())
      }
    ), 
    ("Environment (all)", 
      File.createTempFile("env-all-", ".txt") tap { f =>
        f.deleteOnExit()
        f.writeText(Json.prettyPrint(this.scopes.json))
      }
    ),
    ("Environment (visible)", 
      File.createTempFile("env-visible-", ".txt") tap { f =>
        f.deleteOnExit()
        f.writeText(Json.prettyPrint(this.scopes.visible.json))
      }
    )
  )
  
  /**
    * Adds an attachment to the current context.
    * 
    * @param attachment the attachment (name-file pair) to add
    * @param file the attachment file
    */
  def addAttachment(attachment: (String, File)): Unit = {
    _attachments = attachment :: _attachments
  } 
  
  /** Resets/clears current attachments. */
  private[eval] def resetAttachments() {
    _attachments = Nil
  }
  
  /** Gets the list of attachments (in attached order).*/
  def attachments = _attachments.reverse
  
  /**
    * Can be overridden by subclasses to parse the given step 
    * before it is evaluated. This implementation simply returns the step 
    * as is.
    * 
    * @param step the step to parse
    * @return the resolved step
    */
  def parse(step: Step): Step = step
}

/** Merges two contexts into one. */
class HybridEnvContext[A <: EnvContext, B <: EnvContext](val envA: A, val envB: B, val scopes: ScopedDataStack) extends EnvContext(scopes) {
  override def close() {
    try {
      envB.close()
    } finally {
      envA.close()
    }
  }
}