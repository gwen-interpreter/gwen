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

package gwen.core.eval.lambda.composite

import com.fasterxml.jackson.databind.ObjectMapper

import scala.jdk.CollectionConverters._

import gwen.core.Errors
import gwen.core.eval.EvalContext
import gwen.core.eval.EvalEngine
import gwen.core.node.GwenNode
import gwen.core.node.gherkin.Step

import com.fasterxml.jackson.databind.exc.MismatchedInputException

class ForEachJsonArrayElement[T <: EvalContext](doStep: String, entry: String, source: String, engine: EvalEngine[T]) extends ForEach[T](engine, doStep) {

  override def apply(parent: GwenNode, step: Step, ctx: T): Step = {
    val sourceValue = ctx.getBoundValue(source)
    val values = () => {
      if (sourceValue.nonEmpty) {
        val mapper = new ObjectMapper()
        try {
          mapper.readValue(sourceValue, classOf[java.util.List[String]]).asScala.toSeq
        } catch {
          case e: MismatchedInputException => 
            Errors.unsupportedJsonStructureError(e)
        }
      } else {
        Nil
      }
    }
    evaluateForEach(values, entry, parent, step, ctx)
  }

}
