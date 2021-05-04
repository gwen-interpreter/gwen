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

package gwen.eval.engine

import gwen._
import gwen.model._
import gwen.model.gherkin.Examples
import gwen.eval.EvalContext
import gwen.eval.EvalEngine
import gwen.eval.SpecNormaliser

import com.typesafe.scalalogging.LazyLogging

/**
  * Examples evaluation engine.
  */
trait ExamplesEngine[T <: EvalContext] extends SpecNormaliser with LazyLogging {
    engine: EvalEngine[T] =>

  def evaluateExamples(parent: Identifiable, examples: List[Examples], ctx: T): List[Examples] = ctx.withEnv { env => 
    examples map { exs =>
      ctx.lifecycle.beforeExamples(parent, exs, env.scopes)
      exs.copy(
        withScenarios = exs.scenarios map { scenario =>
          evaluateScenario(exs, scenario, ctx)
        }
      ) tap { exs =>
        ctx.lifecycle.afterExamples(exs, env.scopes)
      }
    }
  }

}
