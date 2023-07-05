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

import gwen.core.BaseTest
import gwen.core.Errors._
import gwen.core.GwenOptions
import gwen.core.eval.binding.JSBinding
import gwen.core.eval.binding.JSFunctionBinding
import gwen.core.eval.EvalContext
import gwen.core.state.EnvState

import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._

class BooleanConditionTest extends BaseTest with Matchers with ScriptSupport {

  val timeout = 1

  "Unbound JS condition" should "fail" in {
    val ctx = newCtx
    intercept[UnboundAttributeException] {
      BooleanCondition("condition", false, timeout, ctx).evaluate()
    }
  }

  "Bound true JS condition" should "evaluate" in {
    val ctx = newCtx
    ctx.scopes.set(JSBinding.key("condition"), "1 === 1")
    BooleanCondition("condition", false, timeout, ctx).evaluate() should be (true)
  }

  "Bound false JS condition" should "evaluate" in {
    val ctx = newCtx
    ctx.scopes.set(JSBinding.key("condition"), "1 === 2")
    BooleanCondition("condition", false, timeout, ctx).evaluate() should be (false)
  }

  "Bound true JS condition" should "invert when negated" in {
    val ctx = newCtx
    ctx.scopes.set(JSBinding.key("condition"), "1 === 1")
    BooleanCondition("condition", true, timeout, ctx).evaluate() should be (false)
  }

  "Bound false JS condition" should "invert when negated" in {
    val ctx = newCtx
    ctx.scopes.set(JSBinding.key("condition"), "1 === 2")
    BooleanCondition("condition", true, timeout, ctx).evaluate() should be (true)
  }

  "Bound JS condition with not prefix" should "evaluate" in {
    val ctx = newCtx
    ctx.scopes.set(JSBinding.key("not condition"), "1 === 1")
    BooleanCondition("condition", true, timeout, ctx).evaluate() should be (true)
    BooleanCondition("not condition", false, timeout, ctx).evaluate() should be (true)
  }

  "Bound true literal" should "evaluate" in {
    val ctx = newCtx
    ctx.scopes.set("condition", "true")
    BooleanCondition("condition", false, timeout, ctx).evaluate() should be (true)
  }

  "Bound false literal" should "evaluate" in {
    val ctx = newCtx
    ctx.scopes.set("condition", "false")
    BooleanCondition("condition", false, timeout, ctx).evaluate() should be (false)
  }

  "Bound true literal" should "invert when negated" in {
    val ctx = newCtx
    ctx.scopes.set("condition", "true")
    BooleanCondition("condition", true, timeout, ctx).evaluate() should be (false)
  }

  "Bound false literal" should "invert when negated" in {
    val ctx = newCtx
    ctx.scopes.set("condition", "false")
    BooleanCondition("condition", true, timeout, ctx).evaluate() should be (true)
  }

  "Bound literal with not prefix" should "evaluate" in {
    val ctx = newCtx
    ctx.scopes.set("not condition", "true")
    BooleanCondition("condition", true, timeout, ctx).evaluate() should be (true)
    BooleanCondition("not condition", false, timeout, ctx).evaluate() should be (true)
  }

  "Bound true JS function as condition" should "evaluate" in {
    val ctx = newCtx
    ctx.scopes.set(JSFunctionBinding.jsRefKey("condition"), "toBoolean")
    ctx.scopes.set(JSFunctionBinding.argsKey("condition"), "true")
    ctx.scopes.set(JSBinding.key("toBoolean"), "arguments[0]")
    BooleanCondition("condition", false, timeout, ctx).evaluate() should be (true)
  }

  "Bound false JS function as condition" should "evaluate" in {
    val ctx = newCtx
    ctx.scopes.set(JSFunctionBinding.jsRefKey("condition"), "toBoolean")
    ctx.scopes.set(JSFunctionBinding.argsKey("condition"), "false")
    ctx.scopes.set(JSBinding.key("toBoolean"), "arguments[0]")
    BooleanCondition("condition", false, timeout, ctx).evaluate() should be (false)
  }

  "Bound true JS function as condition" should "invert when negated" in {
    val ctx = newCtx
    ctx.scopes.set(JSFunctionBinding.jsRefKey("condition"), "toBoolean")
    ctx.scopes.set(JSFunctionBinding.argsKey("condition"), "true")
    ctx.scopes.set(JSBinding.key("toBoolean"), "arguments[0]")
    BooleanCondition("condition", true, timeout, ctx).evaluate() should be (false)
  }

  "Bound false JS function as condition" should "invert when negated" in {
    val ctx = newCtx
    ctx.scopes.set(JSFunctionBinding.jsRefKey("condition"), "toBoolean")
    ctx.scopes.set(JSFunctionBinding.argsKey("condition"), "false")
    ctx.scopes.set(JSBinding.key("toBoolean"), "arguments[0]")
    BooleanCondition("condition", true, timeout, ctx).evaluate() should be (true)
  }

  "Bound JS function as condition with not prefix" should "evaluate" in {
    val ctx = newCtx
    ctx.scopes.set(JSFunctionBinding.jsRefKey("not condition"), "toBoolean")
    ctx.scopes.set(JSFunctionBinding.argsKey("not condition"), "true")
    ctx.scopes.set(JSBinding.key("toBoolean"), "arguments[0]")
    BooleanCondition("condition", true, timeout, ctx).evaluate() should be (true)
    BooleanCondition("not condition", false, timeout, ctx).evaluate() should be (true)
  }

  private def newCtx = new EvalContext(GwenOptions(), EnvState())
  
}
