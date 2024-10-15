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
import gwen.core.eval.support.SQLSupport
import gwen.core.state.Environment
import gwen.core.state.SensitiveData

import scala.util.Try

object SQLBinding {

  def baseKey(name: String) = s"$name/${BindingType.sql}"
  private def databaseKey(name: String) = s"${baseKey(name)}/dbName"
  private def selectKey(name: String) = s"${baseKey(name)}/selectStmt"
  private def maskedKey(name: String) = s"${baseKey(name)}/masked"

  def bind(name: String, database: String, selectStmt: String, masked: Boolean, env: Environment): Unit = {
    SQLSupport.checkDBSettings(database)
    env.topScope.set(databaseKey(name), database)
    env.topScope.set(selectKey(name), selectStmt)
    if (masked) {
      env.topScope.set(maskedKey(name), true.toString)
    }
  }

}

class SQLBinding[T <: EvalContext](name: String, ctx: T) extends Binding[T, String](name, ctx) {

  private val databaseKey = SQLBinding.databaseKey(name)
  private val selectKey = SQLBinding.selectKey(name)
  private val maskedKey = SQLBinding.maskedKey(name)

  override def resolve(): String = {
    bindIfLazy(
      resolveValue(databaseKey) { database => 
        resolveValue(selectKey) { selectStmt =>
          ctx.evaluate(resolveDryValue(BindingType.sql.toString)) {
            val value = ctx.executeSQLQuery(selectStmt, database)
            val masked = ctx.topScope.getOpt(maskedKey).map(_.toBoolean).getOrElse(false)
            if (masked) SensitiveData.mask(name, value) else value
          }
        }
      }
    )
  }

  override def toString: String = Try {
    resolveValue(databaseKey) { database => 
      resolveValue(selectKey) { selectStmt =>
        s"$name [db: $database, ${BindingType.sql}: $selectStmt]"
      }
    }
  } getOrElse name

}
