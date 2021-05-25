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

package gwen.core.engine.spec

import gwen.core._
import gwen.core.model._
import gwen.core.model.node.Background
import gwen.core.engine.EvalContext
import gwen.core.engine.EvalEngine
import gwen.core.engine.SpecNormaliser

import com.typesafe.scalalogging.LazyLogging

/**
  * Background evaluation engine.
  */
trait BackgroundEngine[T <: EvalContext] extends SpecNormaliser with LazyLogging {
    engine: EvalEngine[T] =>

  /**
    * Evaluates a given background.
    */
  private [spec] def evaluateBackground(parent: Identifiable, background: Background, ctx: T): Background = {
    beforeBackground(parent, background, ctx.scopes)
    logger.info(s"Evaluating ${background.keyword}: $background")
    background.copy(withSteps = evaluateSteps(background, background.steps, ctx)) tap { bg =>
      logStatus(bg)
      afterBackground(bg, ctx.scopes)
    }
  }

}
