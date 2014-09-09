/*
 * Copyright 2014 Branko Juric, Brady Wood
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

import com.typesafe.scalalogging.slf4j.LazyLogging

import gwen.Predefs.Exceptions
import gwen.Predefs.FileIO
import gwen.Predefs.Kestrel
import gwen.dsl.Failed
import gwen.dsl.Scenario
import gwen.dsl.Step
import play.api.libs.json.JsObject
import play.api.libs.json.Json
import play.api.libs.json.Json.toJsFieldJsValueWrapper


/**
 * Base environment context providing access to all resources and services to 
 * engines.  Specific [[EvalEngine evaluation engines]] can 
 * define and use their own specific context by extending this one. 
 * 
 * Access to page scope data is provided through a dataScope method.
 * 
 * @author Branko Juric
 */
class EnvContext(dataScopes: DataScopes) extends LazyLogging {
  
  /**
   * Map of step definitions keyed by callable expression name.
   */
  private var stepDefs = Map[String, Scenario]()
  
  /**
   * Closes any resources associated with the evaluation context. This implementation
   * does nothing (but subclasses can override).
   */
  def close() { }
  
  /**
   * Resets the current context but does not close it so it can be reused.
   */
  def reset() {
    dataScopes.reset()
    stepDefs = Map[String, Scenario]()
  }
  
  /**
   * Returns the current state of all scoped attributes as a Json object.
   */  
  def toJson: JsObject = dataScopes.toJson
  
  /**
   * Writes the environment context scopes to a Json string.
   */
  override def toString = Json.prettyPrint(Json.obj("env" -> this.toJson))
  
  /**
   * Gets a named data scope (creates it if it does not exist)
   * 
   * @param name
   * 			the name of the data scope to get (or create and get)
   */
  def dataScope(name: String) = dataScopes.scope(name)
  
  /**
   * Adds a step definition to the context.
   * 
   * @param stepDef
   * 			the step definition to add
   */
  def addStepDef(stepDef: Scenario) {
    stepDefs += ((stepDef.name, stepDef)) 
  }
  
  /**
   * Gets the executable step definition for the given expression (if there is
   * one).
   * 
   * @param expression 
   * 			the expression to match
   * @return the step definition if a match is found; false otherwise
   */
  def getStepDef(expression: String): Option[Scenario] = 
    stepDefs.get(expression) collect { case Scenario(tags, expression, _, steps) => 
      Scenario(tags, expression, steps map { step => 
        Step(step.keyword, step.expression) tap { _.pos = step.pos }
      }) 
    }
  
  /**
   * Fail callback.
   * 
   * @param step 
   * 			the step that failed
   * @param failed
   * 			the failed status
   */
  final def fail(step: Step, failure: Failed): Step = 
    Step(step.keyword, step.expression, failure, step.attachments ++ createAttachments(failure)) tap { step =>
      logger.error(failure.error.getMessage())
      logger.debug(s"Exception: ", failure.error)
      logger.error(this.toString)
    }
  
  /**
   * Creates and returns the stack trace and environment context dump 
   * file attachments.
   * 
   * @param failed
   * 			the failed status
   */
  def createAttachments(failure: Failed): List[(String, File)] = List( 
    ("Stack trace", 
      File.createTempFile("errortrace", ".txt") tap { f =>
        f.deleteOnExit()
        f.writeText(failure.error.writeStackTrace())
      }
    ), 
    ("Environment context", 
      File.createTempFile("envcontext", ".txt") tap { f =>
        f.deleteOnExit()
        f.writeText(this.toString)
      }
    )
  )
  
}

class HybridEnvContext[A <: EnvContext, B <: EnvContext](val envA: A, val envB: B, val dataScopes: DataScopes) extends EnvContext(dataScopes)