/*
 * Copyright 2026 Branko Juric, Brady Wood
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

package gwen.core.eval.support

import gwen.core._

import org.scalatest.matchers.should.Matchers

import scala.util.chaining._

class TextFormatSupportTest extends BaseTest with Matchers with TextFormatSupport {

  "ISO date" should "convert to DMY date" in {
    formatDateTime("2026-03-15", "yyyy-MM-dd", "dd/MM/yyyy") should be ("15/03/2026")
  }

  "Number" should "convert to dollars" in {
    formatNumber("1234", "#", "$#,##0.00") should be ("$1,234.00")
  }

}
