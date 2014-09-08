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

import scala.util.Try
import gwen.dsl.Step
import scala.util.matching.Regex
import com.typesafe.scalalogging.slf4j.LazyLogging

/**
 * Base trait for gwen evaluation engines. An evaluation engine performs the
 * actual processing work required to evaluate individual
 * [[gwen.dsl.Step Step]] expressions. This work can be done using various
 * available frameworks and technologies. This trait serves as a base for
 * concrete engine implementations coupled to specific such frameworks or
 * technologies.
 *
 * Evaluation engines are never invoked directly, but are rather invoked by
 * the [[GwenInterpreter]].  This interpreter can mix in any evaluation engine 
 * that has this trait.
 *
 * @author Branko Juric
 */
trait EvalEngine[T <: EnvContext] extends LazyLogging {
  
  /**
   * Implicit regex string interpolator.  This makes it easy to match 
   * incoming steps against regular expressions and capture their parameters.
   */
  implicit class RegexContext(sc: StringContext) {
    def r = new Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
  }
  
  /**
   * Initialises the engine and returns a bootstrapped evaluation context.
   * This is a lifecycle method and as such it is called by the
   * [[GwenInterpreter]] at the appropriate times.
   * 
   * @param options
   * 			command line options
   * @param dataScopes
   * 			initial data scopes
   */
  private [eval] def init(options: GwenOptions, dataScopes: DataScopes): T

  /**
   * Should be overridden to evaluate a given step (this implementation 
   * can be used as a fallback as it simply throws an unsupported step 
   * exception)
   *
   * @param step
   * 			the step to evaluate
   * @param env
   * 			the environment context
   */
  def evaluate(step: Step, env: T) {
    throw new UnsupportedStepException(step)
  }
}

class UnsupportedStepException(step: Step) extends Exception(s"Unsupported step: ${step}")