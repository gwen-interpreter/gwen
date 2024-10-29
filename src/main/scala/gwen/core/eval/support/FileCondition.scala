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

package gwen.core.eval.support

import gwen.core.Booleans
import gwen.core.Errors
import gwen.core.eval.EvalContext
import gwen.core.eval.FileComparisonOperator
import gwen.core.eval.binding.Binding
import gwen.core.eval.binding.BindingType
import gwen.core.eval.binding.DataRecordBinding
import gwen.core.eval.binding.SimpleBinding

import scala.util.Failure
import scala.util.Success
import scala.util.Try

import util.chaining.scalaUtilChainingOps

import java.io.File
import java.nio.file.Files

class FileCondition[T <: EvalContext](filepath: Option[String], filepathRef: Option[String], operator: FileComparisonOperator, negate: Boolean, ctx: T) {
  
  private val file = new File(filepath.getOrElse(ctx.getBoundValue(filepathRef.get)))

  val condition = s"${filepath.map(fp => s""""$fp" file""").getOrElse(filepathRef.get)} " + (
    operator match {
      case FileComparisonOperator.exists =>
        if (negate) "does not exist" else "exists"
      case FileComparisonOperator.empty =>
        if (negate) "is not empty" else "is empty"
    }
  )

  def evaluate(): Boolean = {
    operator match {
      case FileComparisonOperator.exists =>
        if (negate) !file.exists() else file.exists()
      case FileComparisonOperator.empty =>
        if (!file.exists()) Errors.missingFileError("File", file)
        if (negate) Files.size(file.toPath()) > 0 else Files.size(file.toPath()) == 0
    }
  }

}
