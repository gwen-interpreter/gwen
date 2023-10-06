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

import gwen.core.Errors
import gwen.core.Settings
import gwen.core.eval.EvalContext
import gwen.core.state.ScopedData
import gwen.core.node.gherkin.table.DataTable

import scala.util.matching.Regex
import scala.util.Failure
import scala.util.Success
import scala.util.Try

/** 
 * Resolves all bindings.
 */
class BindingResolver[T <: EvalContext](ctx: T) {

  /**
    * Finds the value of the binding containing a name and applies optional function.
    *
    * @param expression the binding expression
    * @return some value or None otherwise
    */
  def resolveOpt(expression: String): Option[String] = {
    Option(expression).map(_.trim) map { expr => 
      Try(ctx.getBoundValue(expr)) match {
        case Success(value) => value
        case Failure(e) =>
          ctx.parseArrowFunction(expr).map(_.apply) getOrElse { 
            throw e
          }
      }
    }
  }

  /**
    * Finds and returns a given binding.
    *
    * @param name the bindnng name
    * @return a binding
    */
  def getBinding(name: String): Binding[T, String] = {
    ctx.scopes.visibleEntry(name) { (n, _) => 
      n.matches(s"""${Regex.quote(name)}(/(javascript|function.+|xpath.+|regex.+|json path.+|sysproc|file|sql.+${if (ctx.options.dryRun) "|dryValue" else ""}))?""")
     } map { case (n, _) =>
      if (n == DryValueBinding.key(name)) new DryValueBinding(name, "", ctx)
      else if (n == JSBinding.key(name)) new  JSBinding(name, Nil, ctx)
      else if (n.startsWith(JSFunctionBinding.baseKey(name))) new JSFunctionBinding(name, ctx)
      else if (n.startsWith(XPathBinding.baseKey(name))) new XPathBinding(name, ctx)
      else if (n.startsWith(RegexBinding.baseKey(name))) new RegexBinding(name, ctx)
      else if (n.startsWith(JsonPathBinding.baseKey(name))) new JsonPathBinding(name, ctx)
      else if (n == SysprocBinding.key(name)) new SysprocBinding(name, ctx)
      else if (n == FileBinding.key(name)) new FileBinding(name, ctx)
      else if (n.startsWith(SQLBinding.baseKey(name))) new SQLBinding(name, ctx)
      else new  SimpleBinding(name, ctx)
    } getOrElse {
      (ctx.topScope.getObject(DataTable.recordKey) match {
        case Some(record: ScopedData) => Some(new DataRecordBinding(name, ctx))
        case _ => ctx.topScope.getObject(DataTable.tableKey) match {
          case Some(table: DataTable) => Some(new DataTableBinding(name, ctx))
          case _ => None
        }
      }).getOrElse {
        ctx.scopes.getOpt(name) map { _ => 
          new SimpleBinding(name, ctx)
        } getOrElse {
          Settings.getOpt(name) map { _ => 
            new SettingsBinding(name, ctx)
          } getOrElse {
            Errors.unboundAttributeError(name)
          }
        }
      }
    }
  }

}

 