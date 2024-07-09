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
  def encodingKey(name: String) = s"$name/${BindingType.file}/encoding"

  def bind(name: String, filepath: String, encoding: Option[String], env: Environment): Unit = {
    env.scopes.clear(name)
    env.scopes.set(key(name), filepath)
    encoding foreach { enc => 
      env.scopes.set(encodingKey(name), enc)
    }
  }

}

class FileBinding[T <: EvalContext](name: String, ctx: T) extends Binding[T, String](name, ctx) {

  private val key = FileBinding.key(name)
  private val encodingKey = FileBinding.encodingKey(name)

  override def resolve(): String = {
    bindIfLazy(
      resolveValue(key) { filePath =>
        ctx.evaluate(resolveDryValue(BindingType.file.toString)) {
          val file = new File(filePath)
          if (file.exists()) {
            val contents = ctx.scopes.getOpt(encodingKey) match {
              case Some(enc) => 
                Source.fromFile(file, enc).mkString
              case _ => 
                Try(Source.fromFile(file).mkString) getOrElse {
                  Source.fromFile(file, "ISO-8859-1").mkString
                }
            }
            ctx.interpolate(contents)
          } else throw new FileNotFoundException(s"File bound to '$name' not found: $file")
        }
      }
    )
  }

  override def toString: String = Try {
    resolveValue(key) { filePath =>
      s"$name [${BindingType.file}: $filePath]"
    }
  } getOrElse name

}
