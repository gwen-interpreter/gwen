/*
 * Copyright 2014-2023 Branko Juric, Brady Wood
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

import gwen.core._

object Failed {
  def apply(nanos: Long, msg: String): Failed = new Failed(0, new Exception(msg))
}

/**
  * Defines a failed status.
  * 
  * @param nanos the duration in nanoseconds
  * @param error the error
  */
case class Failed(nanos: Long, error: Throwable) extends EvalStatus {
  override val keyword: StatusKeyword = StatusKeyword.Failed
  override def exitCode = 1
  override def icon = Some("✘")
  override def emoticon = "[:(]"
  override def cause = Option(error).map(e => Option(e.getCause).getOrElse(error))
  override def message: String = {
    cause.map(getErrorMessage).orElse(Option(getErrorMessage(error))).getOrElse(error.getClass.getSimpleName)
  }
  private def getErrorMessage(err: Throwable): String = err.getMessage
  override def asIconString(statusName: String): String = asStatusIconString(statusName)
}
