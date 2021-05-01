/*
 * Copyright 2014-2020 Branko Juric, Brady Wood
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

package gwen.eval

import gwen._
import gwen.eval.engine.UnitEngine
import gwen.model._
import gwen.model.gherkin._

import scala.util.Try

import com.typesafe.scalalogging.LazyLogging
import org.apache.log4j.PropertyConfigurator

import java.net.URL


/**
  * Interprets and executes all steps via a mixed in evaluation engine.
  */
class GwenInterpreter[T <: EvalContext] extends UnitEngine[T] with GwenInfo with LazyLogging {
  engine: EvalEngine[T] =>

   /**
    * Initialises the interpreter
    * 
    * @param options the command line options
    */
  def initialise(options: GwenOptions): T = {
    Settings.getOpt("log4j.configuration").orElse(Settings.getOpt("log4j.configurationFile")).foreach { config => 
      if (config.toLowerCase.trim startsWith "file:") {
        PropertyConfigurator.configure(new URL(config));
      } else {
        PropertyConfigurator.configure(config); 
      }
    }
    engine.init(options) tap { env =>
      logger.info(s"${this.getClass.getSimpleName} initialised")
    }
  }
  
  /**
    * Interprets a single step.
    *
    * @param input the input step
    * @param ctx the evaluation context
    * @return the evaluated step (or an exception if a runtime error occurs)
    */
  private[eval] def interpretStep(input: String, ctx: T): Try[Step] = {
    parseStep(input).map { step =>
      engine.evaluateStep(Root, step, ctx)
    }
  }
  
}

