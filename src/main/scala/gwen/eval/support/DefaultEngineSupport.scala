/*
 * Copyright 2015 Branko Juric, Brady Wood
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

package gwen.eval.support

import scala.sys.process.stringSeqToProcess
import scala.sys.process.stringToProcess
import gwen.Predefs.RegexContext
import gwen.dsl.Step
import gwen.eval.EnvContext
import gwen.errors._
import gwen.Settings

/** Provides the common default steps that all engines can support. */
trait DefaultEngineSupport[T <: EnvContext] {

  /**
    * Defines the default steps supported by all engines.
    *
    * @param step the step to evaluate
    * @param env the environment context
    * @throws gwen.errors.UndefinedStepException if the given step is undefined
    *         or unsupported
    */
  def evaluate(step: Step, env: T): Unit = {
    step.expression match {

      case r"""my (.+?)$name (?:property|setting) (?:is|will be) "(.*?)"$$$value""" =>
        Settings.add(name, value)
        
      case r"""(.+?)$attribute (?:is|will be) "(.*?)"$$$value""" => 
        env.featureScope.set(attribute, value)
      
      case r"""I wait ([0-9]+?)$duration second(?:s?)""" => env.execute {
        Thread.sleep(duration.toLong * 1000)
      }
      
      case r"""I execute system process "(.+?)"$$$systemproc""" => env.execute {
        systemproc.! match {
          case 0 => 
          case _ => systemProcessError(s"The call to $systemproc has failed.")
        }
      }
      case r"""I execute a unix system process "(.+?)"$$$systemproc""" => env.execute {
        Seq("/bin/sh", "-c", systemproc).! match {
          case 0 => 
          case _ => systemProcessError(s"The call to $systemproc has failed.")
        }
      }
      
      case _ => undefinedStepError(step)
      
    }
  }
  
}