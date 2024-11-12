/*
 * Copyright 2022-2024 Branko Juric, Brady Wood
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

import gwen.core._
import gwen.core.eval.EvalContext
import gwen.core.eval.action.UnitStepAction
import gwen.core.node.GwenNode
import gwen.core.node.gherkin.Step
import gwen.core.behavior.BehaviorType

import scala.util.chaining._

import java.io.File

class WriteTextToFile[T <: EvalContext](content: Option[String], contentRef: Option[String], filepath: Option[String], filepathRef: Option[String], overwrite: Boolean) extends UnitStepAction[T] {
  override def apply(parent: GwenNode, step: Step, ctx: T): Step = {
    step tap { _ =>
      checkStepRules(step, BehaviorType.Action, ctx)
      val text = content.getOrElse(ctx.getBoundValue(contentRef.get))
      val file = new File(filepath.getOrElse(ctx.getBoundValue(filepathRef.get)))
      ctx.evaluate(step) {
        if (overwrite) {
          file.writeText(text)
        } else {
          file.appendText(text)
        }
      }
    }
  }

}