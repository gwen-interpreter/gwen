/*
 * Copyright 2022 Branko Juric, Brady Wood
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
import gwen.core.eval.binding.Binding
import gwen.core.eval.binding.BindingType
import gwen.core.eval.binding.DataRecordBinding
import gwen.core.eval.binding.SimpleBinding

import scala.util.Failure
import scala.util.Success
import scala.util.Try

import util.chaining.scalaUtilChainingOps

class BooleanCondition[T <: EvalContext](condition: String, negate: Boolean, timeoutSecs: Long, ctx: T) {
  
  val (name, binding, negated) = {
    if (negate) {
      find(s"not $condition") match {
        case Success(b) => (s"not $condition", b, false)
        case Failure(e) => (condition, find(condition) getOrElse { throw e }, true)
      }
    } else {
      find(condition) match {
        case Success(b) => (condition, b, false)
        case Failure(e) => throw e
      }
    }
  }
    
  private def find (name: String): Try[Binding[T, String]] = {
    Try(ctx.getBinding(name).asInstanceOf[Binding[T, String]]) map { binding => 
      binding tap { _ =>
        if (binding.isInstanceOf[SimpleBinding[T]] || binding.isInstanceOf[DataRecordBinding[T]]) {
          val v = binding.resolve()
          if (!Booleans.isBoolean(v) && !isDryRunValue(v)) {
            Errors.unboundBooleanAttributeError(name, v)
          }
        }
      }
    }
  }

  private def isDryRunValue(value: String): Boolean = ctx.options.dryRun && value.contains(s"$$[${BindingType.dryValue}:")

  def evaluate(): Boolean = {
    var result: Option[Boolean] = None
    var error: Option[Throwable] = None
    try {
      ctx.waitUntil(timeoutSecs, s"waiting for condition: $condition") {
        try {
          val raw = binding.resolve()
          val res = Try(raw.toBoolean).getOrElse(Errors.invalidTypeError(s"Boolean expected but got '$raw' when evaluating condition: $condition"))
          result = Option(if (negated) !res else res)
        } catch {
          case e: Throwable => 
            error = Some(e)
        }
        result.nonEmpty
      }
    } catch {
      case e: Throwable =>
        result getOrElse {
          error map { err => 
            throw err
          } getOrElse {
            throw e
          }
        }
    }
    result getOrElse {
      Errors.waitTimeoutError(timeoutSecs, s"Timed out waiting for condition: $condition")
    }
    
  }

}
