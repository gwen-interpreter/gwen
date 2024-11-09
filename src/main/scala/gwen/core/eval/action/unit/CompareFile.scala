/*
 * Copyright 2021-2024 Branko Juric, Brady Wood
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

package gwen.core.eval.action.unit

import gwen.core.eval.FileComparisonOperator
import gwen.core.eval.EvalContext
import gwen.core.eval.action.UnitStepAction
import gwen.core.eval.support.FileCondition
import gwen.core.node.GwenNode
import gwen.core.node.gherkin.Step
import gwen.core.behavior.BehaviorType

import scala.util.chaining._
import scala.util.Failure
import scala.util.Success
import scala.util.Try

class CompareFile[T <: EvalContext](filepath: Option[String], filepathRef: Option[String], operator: FileComparisonOperator, negate: Boolean, message: Option[String]) extends UnitStepAction[T] {

  override def apply(parent: GwenNode, step: Step, ctx: T): Step = {
    step tap { _ =>
      checkStepRules(step, BehaviorType.Assertion, ctx)
      val fCond = FileCondition(filepath, filepathRef, operator, negate, ctx)
      ctx.perform {
        val result = Try(fCond.evaluate())
        result match {
          case Success(assertion) =>
            val displayName = fCond.condition
            ctx.assertWithError(
              assertion, 
              message, 
              s"${filepath.map(fp => s"""'$fp' file""").getOrElse(filepathRef.get)} should${if (negate) " not" else ""} ${if(operator == FileComparisonOperator.exists) s"exist but did${if(!negate) " not" else ""}" else s"be empty but was${if(!negate) " not" else ""}"}",
              step.assertionMode)
          case Failure(error) =>
            throw error;
        }
      }
    }
  }

}
