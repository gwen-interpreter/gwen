/*
 * Copyright 2017-2023 Branko Juric, Brady Wood
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

package gwen.core

import gwen.core.Errors.GwenAssertionError
import gwen.core.node.gherkin.Annotations

enum AssertionMode:
  case hard, soft, sustained
  def annotation: Annotations = {
    if (this == hard) Annotations.Hard
    else if (this == soft) Annotations.Soft
    else Annotations.Sustained
  }
