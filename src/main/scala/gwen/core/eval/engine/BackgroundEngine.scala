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

package gwen.core.eval.engine

import gwen.core._
import gwen.core.eval.EvalContext
import gwen.core.eval.EvalEngine
import gwen.core.node.GwenNode
import gwen.core.node.gherkin.Background
import gwen.core.node.gherkin.SpecNormaliser

import scala.util.chaining._

import com.typesafe.scalalogging.LazyLogging

/**
  * Background evaluation engine.
  */
trait BackgroundEngine[T <: EvalContext] extends SpecNormaliser with LazyLogging {
  engine: EvalEngine[T] =>

  /**
    * Evaluates a given background.
    */
  private [engine] def evaluateBackground(parent: GwenNode, background: Background, ctx: T): Background = {
    beforeBackground(background, ctx)
    logger.info(s"Evaluating ${background.keyword}: $background")
    background.copy(withSteps = evaluateSteps(background, background.steps, ctx)) tap { bg =>
      logStatus(ctx.options, bg)
      afterBackground(bg, ctx)
    }
  }

}
