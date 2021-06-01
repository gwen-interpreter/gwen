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

package gwen.core.eval

object ComparisonOperator extends Enumeration {

  val be, contain = Value
  val `start with` = Value("start with")
  val `end with` = Value("end with")
  val `match regex` = Value("match regex")
  val `match xpath` = Value("match xpath")
  val `match json path` = Value("match json path")
  val `match template` = Value("match template")
  val `match template file` = Value("match template file")

}
