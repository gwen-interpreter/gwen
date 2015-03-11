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

/** Can be mixed into evaluation engines to provide system process support. */
trait SystemProcessSupport[T <: EnvContext] {

  /**
    * Defines steps to perform system processes.
    *
    * @param step the step to evaluate
    * @param env the environment context
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
    }
  }
  
}