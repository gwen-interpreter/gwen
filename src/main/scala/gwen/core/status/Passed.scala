/*
 * Copyright 2014-2021 Branko Juric, Brady Wood
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

package gwen.core.status

object Passed {
  
  def apply(nanos: Long): Passed = Passed(nanos, false)

}

/**
  * Defines an Passed status.
  * 
  * @param nanos the duration in nanoseconds
  * @param abstrained true if abstained, false otherwise
  */
case class Passed(nanos: Long, abstained: Boolean) extends EvalStatus {
  override val keyword: StatusKeyword = StatusKeyword.Passed
  override def exitCode = 0
  override def icon = Some("✔")
  override def emoticon = "[:)]"

  override def isAbstained: Boolean = abstained
}
