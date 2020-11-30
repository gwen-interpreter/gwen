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

package gwen.report.portal

import gwen.dsl.NodeType

object RPConfig {

  object StepDefFormat extends Enumeration {
    type StepDefFormat = Value
    val inlined, nested, none = Value
  }

  object SendStepDefs {
    def isInlined = RPSettings.`gwen.rp.send.stepDefs` == StepDefFormat.inlined
    def isNested = RPSettings.`gwen.rp.send.stepDefs` == StepDefFormat.nested
    def isNone = RPSettings.`gwen.rp.send.stepDefs` == StepDefFormat.none
  }

  object SendFailedStepDefs {
    def isInlined = RPSettings.`gwen.rp.send.failed.stepDefs` == StepDefFormat.inlined
    def isNested = RPSettings.`gwen.rp.send.failed.stepDefs` == StepDefFormat.nested
    def isNone = RPSettings.`gwen.rp.send.failed.stepDefs` == StepDefFormat.none
  }

  object ErrorReportingMode extends Enumeration {
    type ErrorReportingMode = Value
    val attached, inlined, none = Value
  }

  object SendErrorTrace extends Enumeration {
    def isAttached = RPSettings.`gwen.rp.send.failed.errorTrace` == ErrorReportingMode.attached
    def isInlined = RPSettings.`gwen.rp.send.failed.errorTrace` == ErrorReportingMode.inlined
    def isNone = RPSettings.`gwen.rp.send.failed.errorTrace` == ErrorReportingMode.none
  }

  object SendEnvTrace extends Enumeration {
    def isAttached = RPSettings.`gwen.rp.send.failed.envTrace` == ErrorReportingMode.attached
    def isInlined = RPSettings.`gwen.rp.send.failed.envTrace` == ErrorReportingMode.inlined
    def isNone = RPSettings.`gwen.rp.send.failed.envTrace` == ErrorReportingMode.none
  }

  object SendHierarchy extends Enumeration {
    def isAttached = RPSettings.`gwen.rp.send.failed.hierarchy` == ErrorReportingMode.attached
    def isInlined = RPSettings.`gwen.rp.send.failed.hierarchy` == ErrorReportingMode.inlined
    def isNone = RPSettings.`gwen.rp.send.failed.hierarchy` == ErrorReportingMode.none
  }

  object StepNodes extends Enumeration {
    type StepNodes = Value
    val all, leaf, none = Value
  }

  object AppendFailedMsgToStepNodes extends Enumeration {
    def all = RPSettings.`gwen.rp.append.failed.msg.toStepNodes` == StepNodes.all
    def leaf = RPSettings.`gwen.rp.append.failed.msg.toStepNodes` == StepNodes.leaf
    def none = RPSettings.`gwen.rp.append.failed.msg.toStepNodes` == StepNodes.none
  }

  def bypassNodeTypes: Set[NodeType.Value] = {
    Set(
      (NodeType.Meta, !RPSettings.`gwen.rp.send.meta`), 
      (NodeType.StepDef, SendStepDefs.isNone)
    ) filter {
      case (_, bypass) => bypass
    } map {
      case (nodeType, _) => nodeType
    }
  }

}
