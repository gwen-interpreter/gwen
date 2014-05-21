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

import com.typesafe.scalalogging.slf4j.LazyLogging

import gwen.Predefs.Kestrel
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
class EnvContext extends LazyLogging {
  
  /**
   * Map of scoped data keyed by name.
   */
  private var dataScopes = Map[String, ScopedDataStack]()
  
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
    dataScopes = Map[String, ScopedDataStack]()
    stepDefs = Map[String, Scenario]()
  }
  
  /**
   * Returns the current state of all scoped attributes as a Json object.
   */  
  def toJson: JsObject = {
    Json.obj("data" -> dataScopes.map { case (name, scope) => scope.toJson })
  }
  
  /**
   * Writes the environment context scopes to a Json string.
   */
  override def toString = Json.prettyPrint(Json.obj("env" -> this.toJson))
  
  /**
   * Gets a named data scope (creates it if it does not exist)
   * 
   * @param the name of the data scope to get (or create and get)
   */
  def dataScope(name: String) = 
    if (dataScopes.contains(name)) {
      dataScopes(name)
    } else {
      new ScopedDataStack(name) tap { scope =>
        scope.addScope("default")
        dataScopes += (name -> scope)
      }
    }
  
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
        val s = Step(step.keyword, step.expression)
        s.pos = step.pos
        s
      }) 
    }
  
}