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

package gwen.core.report.rp

import gwen.core.node.NodeType

object RPConfig {

  val nameMaxChars = if (RPSettings.`gwen.rp.send.markdownBlocks`) 1014 else 1024
  val attributeMaxChars = 128

  enum StepDefFormat {
    case inlined, nested, none
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

  enum ErrorReportingMode {
    case attached, inlined, none
  }

  object SendErrorTrace {
    def isAttached = RPSettings.`gwen.rp.send.failed.errorTrace` == ErrorReportingMode.attached
    def isInlined = RPSettings.`gwen.rp.send.failed.errorTrace` == ErrorReportingMode.inlined
    def isNone = RPSettings.`gwen.rp.send.failed.errorTrace` == ErrorReportingMode.none
  }

  object SendEnvTrace {
    def isAttached = RPSettings.`gwen.rp.send.failed.envTrace` == ErrorReportingMode.attached
    def isInlined = RPSettings.`gwen.rp.send.failed.envTrace` == ErrorReportingMode.inlined
    def isNone = RPSettings.`gwen.rp.send.failed.envTrace` == ErrorReportingMode.none
  }

  object SendHierarchy {
    def isAttached = RPSettings.`gwen.rp.send.failed.hierarchy` == ErrorReportingMode.attached
    def isInlined = RPSettings.`gwen.rp.send.failed.hierarchy` == ErrorReportingMode.inlined
    def isNone = RPSettings.`gwen.rp.send.failed.hierarchy` == ErrorReportingMode.none
  }

  enum ErrorBlocks{
    case all, leaf, none
  }

  object AppendErrorBlocks {
    def all = RPSettings.`gwen.rp.send.failed.errorBlocks` == ErrorBlocks.all
    def leaf = RPSettings.`gwen.rp.send.failed.errorBlocks` == ErrorBlocks.leaf
    def none = RPSettings.`gwen.rp.send.failed.errorBlocks` == ErrorBlocks.none
  }

  enum TestCaseIdKeys {
    case auto, nodePath, sourceRef, `sourceRef+params`, `nodePath+params`
  }

  def bypassNodeTypes: Set[NodeType] = {
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
