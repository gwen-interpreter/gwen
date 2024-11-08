/*
 * Copyright 2024 Branko Juric, Brady Wood
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
import gwen.core.behavior.BehaviorType
import gwen.core.eval.EvalContext
import gwen.core.eval.lambda.UnitStep
import gwen.core.node.GwenNode
import gwen.core.node.gherkin.Step
import gwen.core.result.ResultFile

import scala.util.chaining._

import java.io.File

class LogResultsRecord[T <: EvalContext](resultsFileId: String) extends UnitStep[T] {
  override def apply(parent: GwenNode, step: Step, ctx: T): Step = {
    step tap { _ =>
      checkStepRules(step, BehaviorType.Action, ctx)
      val resultsFile = ctx.options.resultFiles.find(_.id == resultsFileId) getOrElse {
        Errors.resultsFileError(s"No such result file: gwen.reports.results.files.$resultsFileId setting not found")
      }
      resultsFile.scope foreach { scope =>
        Errors.resultsFileError(s"Cannot explicitly write to results file (id: $resultsFileId) defined with $scope scope")
      }
      resultsFile.status foreach { status =>
        Errors.resultsFileError(s"Cannot explicitly write to results file (id: $resultsFileId) defined with $status status")
      }
      resultsFile.logRecord(ctx)
    }
  }

}
