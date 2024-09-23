/*
 * Copyright 2023 Branko Juric, Brady Wood
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

package gwen.core.eval.lambda.unit

import gwen.core._
import gwen.core.eval.EvalContext
import gwen.core.eval.lambda.UnitStep
import gwen.core.node.GwenNode
import gwen.core.node.gherkin.Step
import gwen.core.behavior.BehaviorType

import java.io.File
import java.io.FileOutputStream
import java.net.URL
import java.nio.channels.Channels

class DownloadToFile[T <: EvalContext](sourceUrl: String, filepath: Option[String], filepathRef: Option[String], timeoutSecs: Long) extends UnitStep[T] {

  override def apply(parent: GwenNode, step: Step, ctx: T): Step = {
    checkStepRules(step, BehaviorType.Action, ctx)
    val file = new File(filepath.getOrElse(ctx.getBoundValue(filepathRef.get)))
    ctx.perform{
      ctx.getWithWait(timeoutSecs, s"downloading $sourceUrl to $file") { () => 
        val urlChannel = Channels.newChannel(new URL(sourceUrl).openStream())
        val fileChannel = file.newFileOutputStream.getChannel
        fileChannel.transferFrom(urlChannel, 0, Long.MaxValue)
      }
    }
    step
  }

}

