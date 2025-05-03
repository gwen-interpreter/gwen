/*
 * Copyright 2021-2024 Branko Juric, Brady Wood
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

package gwen.core.node.gherkin

import gwen.core.Errors
import gwen.core.node.SourceRef

enum Annotations:
  case Ignore, Context, Action, Assertion, Import, StepDef, ForEach, DataTable, HorizontalTable, VerticalTable, Examples, Synchronised, Synchronized, Synthetic, If, While, Until, Breakpoint, Finally, Eager, Lazy, Deferred, Message, Try, Data, NoData, Hard, Soft, Sustained, DryRun, IgnoreCase, Trim, Masked, Parallel, Timeout, Delay, Results, Abstract, ShadowRoot

object Annotations {
  private val stepLevelAnnotations: List[String] = List(Message, Try, Finally, Eager, Lazy, Breakpoint, Hard, Soft, Sustained, DryRun, Masked, Timeout, Delay, Trim, IgnoreCase, Abstract, ShadowRoot).map(_.toString)
  def validateStepLevel(sourceRef: Option[SourceRef], annotation: Tag): Unit = {
    if (!stepLevelAnnotations.contains(annotation.name)) {
      stepLevelAnnotations.find(_.toUpperCase == annotation.name.toUpperCase) map { annot =>
        Errors.illegalStepAnnotationError(sourceRef, s"Invalid annonation $annotation. Did you mean @${annot}?")
      } getOrElse {
        Errors.illegalStepAnnotationError(sourceRef, s"Invalid annonation $annotation. Valid step level annotations include: ${stepLevelAnnotations.mkString(", ")}")
      }
    }
  }
}
