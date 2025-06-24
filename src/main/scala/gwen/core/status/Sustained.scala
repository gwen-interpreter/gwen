/*
 * Copyright 2014-2024 Branko Juric, Brady Wood
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

import gwen.core.Errors.CustomErrorMessage

/**
  * Defines the sustained status.
  *
  * @param nanos the duration in nanoseconds
  * @param error the error to sustain
  */
case class Sustained(nanos: Long, error: Throwable) extends EvalError(error) {
  override val keyword: StatusKeyword = StatusKeyword.Sustained
  override def exitCode = 0
  override def emoticon = "[:|]"
}
