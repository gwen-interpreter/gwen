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
import gwen.core.behavior.BehaviorType
import gwen.core.data.DataRecord
import gwen.core.data.DataSource
import gwen.core.eval.EvalContext
import gwen.core.eval.action.UnitStepAction
import gwen.core.node.GwenNode
import gwen.core.node.gherkin.Step

import scala.util.chaining._
import scala.util.Try

import java.io.File
import gwen.core.eval.ComparisonOperator

class UniqueDataCheck[T <: EvalContext](names: List[String], filepath: Option[String], filepathRef: Option[String]) extends UnitStepAction[T] {
  override def apply(parent: GwenNode, step: Step, ctx: T): Step = {
    step tap { _ =>
      checkStepRules(step, BehaviorType.Assertion, ctx)
      val values = names map { name => ctx.getBoundValue(name) } mkString ","
      val file = new File(filepath.getOrElse(ctx.getBoundValue(filepathRef.get)))
      if (!file.exists()) Errors.missingFileError("Data file", file)
      val dataSource =  DataSource(file)
      val header = dataSource.header
      val hIndexes = names flatMap { name =>
        if (!header.exists(_ == name)) {
          Errors.dataLookupError(file, name)
        }
        header.zipWithIndex.filter(_._1 == name).map(_._2)
      }
      val matches = dataSource.data map { d =>
        hIndexes.map(idx => d(idx)) mkString ","
      } filter (_ == values)
      ctx.assertWithError(
          matches.size < 2, 
          s"${names.mkString(",")}: $values is not unique in $file file (${matches.size} occurrences found)",
          step.assertionMode)
    }
  }

}

