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

import scala.sys.process.stringSeqToProcess
import scala.sys.process.stringToProcess

import com.typesafe.scalalogging.slf4j.LazyLogging

import gwen.Predefs.RegexContext
import gwen.dsl.Step

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
   * Initialises the engine and returns a bootstrapped evaluation context.
   * This is a lifecycle method and as such it is called by the
   * [[GwenInterpreter]] at the appropriate times.
   * 
   * @param options
   * 			command line options
   * @param scopes
   * 			initial data scopes
   */
  private [eval] def init(options: GwenOptions, scopes: ScopedDataStack): T

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
  def evaluate(step: Step, env: T): Unit = {
    step.expression match {
      case r"""I execute system process "(.+?)"$$$systemproc""" =>
        systemproc.! match {
          case 0 => 
          case _ => sys.error(s"The call to $systemproc has failed.")
        }
      case r"""I execute a unix system process "(.+?)"$$$systemproc""" =>
        Seq("/bin/sh", "-c", systemproc).! match {
          case 0 => 
          case _ => sys.error(s"The call to $systemproc has failed.")
        }
      case _ => throw new UnsupportedStepException(step)
    }
  }

  /**
   * Adds another engine to this one to create a new hybrid engine.
   * 
   * @param otherEngine
   * 			the other engine
   */
  def +[U <: EnvContext](otherEngine: EvalEngine[U]) = new HybridEvalEngine[T, U] {
	override val engineA = EvalEngine.this
	override val engineB = otherEngine
  }
}

/**
 * Hybrid engine trait
 */
trait HybridEvalEngine[A <: EnvContext, B <: EnvContext] extends EvalEngine[HybridEnvContext[A, B]] {
  
  val engineA: EvalEngine[A]
  val engineB: EvalEngine[B]
  
  override def init(options: GwenOptions, scopes: ScopedDataStack): HybridEnvContext[A, B] = 
    new HybridEnvContext(
      engineA.init(options, scopes), 
      engineB.init(options, scopes), 
      scopes)
  
  override def evaluate(step: Step, env: HybridEnvContext[A, B]) {
    try {
  	  engineA.evaluate(step, env.envA)
    } catch {
      case _: UnsupportedStepException => 
        engineB.evaluate(step, env.envB)
    }
  }
  
}

/**
 * Thrown when an engine does not support a step.
 */
class UnsupportedStepException(step: Step) extends Exception(s"Unsupported step: ${step}")