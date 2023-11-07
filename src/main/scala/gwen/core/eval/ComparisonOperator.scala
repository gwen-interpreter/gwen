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

enum ComparisonOperator:
  case be, contain, `start with`, `end with`, `match regex`, `match xpath`, `match json path`, `match template`, `match template file`

object ComparisonOperator {
  def fromModal(modal: String): ComparisonOperator = {
    modal match {
      case "is" => ComparisonOperator.be
      case "contains" => ComparisonOperator.contain
      case "starts with" => ComparisonOperator.`start with`
      case "ends with" => ComparisonOperator.`end with`
      case "matches regex" => ComparisonOperator.`match regex`
      case "matches xpath" => ComparisonOperator.`match xpath`
      case "matches json path" => ComparisonOperator.`match json path`
      case "matches template" => ComparisonOperator.`match template`
      case "matches template file" => ComparisonOperator.`match template file`
      case _ => ComparisonOperator.valueOf(modal)
    }
  }
}
