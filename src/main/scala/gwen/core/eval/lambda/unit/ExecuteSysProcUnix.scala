/*
 * Copyright 2021 Branko Juric, Brady Wood
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

package gwen.core.eval.lambda.unit

import gwen.core._
import gwen.core.eval.EvalContext
import gwen.core.eval.lambda.UnitStep
import gwen.core.node.GwenNode
import gwen.core.node.gherkin.Step
import gwen.core.behavior.BehaviorType
import gwen.core.state.SensitiveData

import scala.sys.process.stringSeqToProcess
import scala.util.chaining._

class ExecuteSysProcUnix[T <: EvalContext](systemproc: String, delimiter: Option[String]) extends UnitStep[T] {

  override def apply(parent: GwenNode, step: Step, ctx: T): Step = {
    step tap { _ =>
      checkStepRules(step, BehaviorType.Action, ctx)
      ctx.perform {
        SensitiveData.withValue(systemproc) { sproc =>
          delimiter match {
            case None => Seq("/bin/sh", "-c", sproc).!
            case Some(delim) => 
              SensitiveData.withValue(delim) { d =>
                (Seq("/bin/sh", "-c") ++ sproc.split(d).toSeq).!
              }
          } match {
            case 0 =>
            case _ => Errors.systemProcessError(s"The call to system process '$systemproc' has failed.")
          }
        }
      }
    }
  }

}
