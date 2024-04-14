/*
 * Copyright 2021-2023 Branko Juric, Brady Wood
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

enum Annotations:
  case Ignore, Context, Action, Assertion, Import, StepDef, ForEach, DataTable, HorizontalTable, VerticalTable, MatrixTable, Examples, Synchronised, Synchronized, Synthetic, If, While, Until, Breakpoint, Finally, Eager, Lazy, Deferred, Message, Try, Data, NoData, Hard, Soft, Sustained, DryRun, IgnoreCase, Trim
