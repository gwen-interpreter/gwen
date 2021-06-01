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

package gwen.core.eval.binding

import gwen.core.eval.EvalContext
import gwen.core.state.Environment

import scala.io.Source
import scala.util.Try

import java.io.File
import java.io.FileNotFoundException

object FileBinding {
  
  def key(name: String) = s"$name/${BindingType.file}"

  def bind(name: String, filepath: String, env: Environment): Unit = {
    env.scopes.set(key(name), filepath)
  }

}

class FileBinding[T <: EvalContext](name: String, ctx: T) extends Binding[T, String](name, ctx) {

  private val key = FileBinding.key(name)

  override def resolve(): String = {
    resolveValue(key) { filePath =>
      ctx.evaluate(s"$$[dryRun:${BindingType.file}]") {
        val file = new File(filePath)
        if (file.exists()) {
          ctx.interpolate(Source.fromFile(file).mkString)
        } else throw new FileNotFoundException(s"File bound to '$name' not found: $file")
      }
    }
  }

  override def toString: String = Try {
    resolveValue(key) { filePath =>
      s"$name [file: $filePath]"
    }
  } getOrElse name

}
