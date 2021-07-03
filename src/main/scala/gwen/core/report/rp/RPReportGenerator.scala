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

package gwen.core.report.rp

import gwen.GwenInterpreter
import gwen.core.GwenOptions
import gwen.core.eval.EvalContext
import gwen.core.node.event.NodeEventDispatcher
import gwen.core.report.NoopReportGenerator
import gwen.core.report.ReportFormatter
import gwen.core.state.Environment
import gwen.core.status.EvalStatus

/**
  * Generates reports in reoprt portal.
  */
class RPReportGenerator(val options: GwenOptions) extends NoopReportGenerator(RPReportConfig, options) with ReportFormatter {

  private var rpReporter: Option[RPReporter] = None

  override def init(lifecycle: NodeEventDispatcher): Unit = { 
    if (!options.dryRun) {
      val client = new RPClient(options)
      val reporter = new RPReporter(client)
      lifecycle.addListener(reporter)
      rpReporter = Some(reporter)
    }
  }

  override def close(lifecycle: NodeEventDispatcher, evalStatus: EvalStatus): Unit = { 
    rpReporter.foreach { reporter => 
      reporter.close(evalStatus)
      lifecycle.removeListener(reporter)
    }
  }

}