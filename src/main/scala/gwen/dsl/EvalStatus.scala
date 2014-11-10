/*
 * Copyright 2014 Branko Juric, Brady Wood
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

package gwen.dsl

import scala.concurrent.duration._
import java.text.DecimalFormat

/**
 * Captures the evaluation status of a [[SpecNode]].
 * 
 * @param status
 * 			the status keyword
 * @param duration
 * 			the duration in nanoseconds
 * @param message
 * 			optional message
 */
sealed trait EvalStatus {
  val status: StatusKeyword.Value
  val duration: Long
  def code: Int
  override def toString = 
    if (duration > 0) {
      s"[${EvalStatus.formatSecs(duration)}] ${status}"
    } else status.toString
}

/**
 * Defines a passed evaluation status.
 * 
 * @param duration
 * 		the duration in nanoseconds
 */
case class Passed(val duration: Long) extends EvalStatus {
  val status = StatusKeyword.Passed
  def code = 0
}

/**
 * Defines a failed evaluation status.
 * 
 * @param duration
 * 		the duration in nanoseconds
 * @param error
 * 		the error message
 */
case class Failed(val duration: Long, val error: Throwable) extends EvalStatus {
  val status = StatusKeyword.Failed
  def code = 1
}

/**
 * Defines the skipped status.
 */
case object Skipped extends EvalStatus {
  val duration = 0L
  val status = StatusKeyword.Skipped
  def code = 2
}

/**
 * Defines the pending status.
 * 
 * @param duration
 * 		the duration in nanoseconds (default value is zero)
 */
case object Pending extends EvalStatus {
  val duration = 0L
  val status = StatusKeyword.Pending
  def code = 3
}

/**
 * Defines the loaded status.
 */
case object Loaded extends EvalStatus {
  val duration = 0L
  val status = StatusKeyword.Loaded
  def code = 0
}

object EvalStatus {

  private val secsFormatter = new DecimalFormat("0.0000")
  
  /**
   * Function for getting the effective evaluation status of a given list of statuses.
   * 
   * @param statuses
   * 			the list of evaluation statuses
   *  
   */
  def apply(statuses: List[EvalStatus]): EvalStatus = {
    val duration = (statuses map (_.duration)).sum
    statuses.collectFirst { case failed @ Failed(_, _) => failed } match {
      case Some(failed) => Failed(duration, failed.error)  
      case None =>
        if (statuses.forall(_ == Loaded)) {
          Loaded
        } else {
          statuses.filter(_ != Loaded).lastOption match {
            case Some(lastStatus) => lastStatus match {
              case Passed(_) => Passed(duration)
              case Skipped => lastStatus
              case _ => Pending
            }
            case None => Pending
          }
        }
    }
  }
  
  def formatSecs(durationNano: Long): String = 
    if (durationNano > 0) { 
      s"${secsFormatter.format(durationNano.toDouble / 1000000000)} secs"
    } else ""
  
}

/**
 * Enumeration of supported status keywords.
 * 
 * @author Branko Juric
 */
object StatusKeyword extends Enumeration {

  val Passed, Failed, Skipped, Pending, Loaded = Value
  
  val valuesFixedOrder = List(Passed, Failed, Skipped, Pending, Loaded)

  /**
   * Groups counts by status.
   * 
   * @param statuses
   * 		the statuses to group
   */
  def countsByStatus(statuses: List[EvalStatus]): Map[StatusKeyword.Value, Int] = 
    statuses.groupBy(_.status) map { case (k, v) => (k, v.size) }
}