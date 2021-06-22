/*
 * Copyright 2017 Branko Juric, Brady Wood
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

package gwen.report

import gwen.dsl.EvalStatus
import gwen.eval.EnvContext
import gwen.eval.GwenInterpreter
import gwen.eval.GwenOptions
import gwen.report.portal.RPReporter
import gwen.report.portal.RPClient

/**
  * Generates reports in reoprt portal.
  */
class RPReportGenerator(val options: GwenOptions) extends NoopReportGenerator(RPReportConfig, options) with NoopReportFormatter {

  private var rpReporter: Option[RPReporter] = None

  override def init[T <: EnvContext](interpreter: GwenInterpreter[T]): Unit = { 
    if (!options.dryRun) {
      val client = new RPClient(options)
      val reporter = new RPReporter(client)
      interpreter.addLifecycleEventListener(reporter)
      rpReporter = Some(reporter)
    }
  }

  override def close[T <: EnvContext](interpreter: GwenInterpreter[T], evalStatus: EvalStatus): Unit = { 
    rpReporter.foreach { reporter => 
      reporter.close(evalStatus)
      interpreter.removeLifecycleEventListener(reporter)
    }
  }

}