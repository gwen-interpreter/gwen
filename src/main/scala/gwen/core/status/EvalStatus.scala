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

import gwen.core.AssertionMode
import gwen.core.DurationOps
import gwen.core.Errors
import gwen.core.Formatting

import scala.concurrent.duration._

import java.util.Date

/** The evaluation status of a node. */
trait EvalStatus {

  val keyword: StatusKeyword
  val nanos: Long
  val timestamp = new Date()

  def isPassed: Boolean = keyword == StatusKeyword.Passed
  def isFailed: Boolean = keyword == StatusKeyword.Failed
  def isSustained: Boolean = keyword == StatusKeyword.Sustained
  def isSkipped: Boolean = keyword == StatusKeyword.Skipped
  def isPending: Boolean = keyword == StatusKeyword.Pending
  def isLoaded: Boolean = keyword == StatusKeyword.Loaded
  def isDisabled: Boolean = keyword == StatusKeyword.Disabled
  def isAbstained: Boolean = false

  def isEvaluated: Boolean = isPassed || isDisabled || isError
  def isError: Boolean = isFailed || isSustained

  /** Returns the duration in nanoseconds. */
  def duration: Duration = Duration.fromNanos(nanos)

  /** Must be overriden to return exit code. */
  def exitCode: Int

  /** Must be overriden to return an emoticon. */
  def emoticon: String

  /** Optional error cause. */
  def cause: Option[Throwable] = None

  /** Determines whether or not this status is due to a technical error. */
  def isTechError: Boolean = !isLicenseError && !isDisabledError

  /** Determines whether or not this status is due to an assertion error. */
  def isAssertionError: Boolean =
    cause.exists(c => c != null && c.isInstanceOf[AssertionError])

  def isSustainedError = isAssertionError && AssertionMode.isSustained

  /** Determines whether or not this status is due to an disabled step error. */
  def isDisabledError: Boolean =
    cause.exists(c => c != null && c.isInstanceOf[Errors.DisabledStepException])

  /** Determines whether or not this status is due to a licens error. */
  def isLicenseError: Boolean =
    cause.exists(c => c != null && c.isInstanceOf[Errors.LicenseException])

  def message: String = cause.map(_.getMessage).getOrElse(keyword.toString)

  override def toString: String =
    if (nanos > 0) {
      s"[${Formatting.formatDuration(duration)}] $keyword"
    } else keyword.toString
}

object EvalStatus {

  /**
    * Function for getting the effective status of a given list of statuses.
    *
    * @param statuses the list of statuses
    */
  def apply(statuses: List[EvalStatus]): EvalStatus = apply(statuses, ignoreSustained = true)

  /**
    * Function for getting the effective evaluation status of a given list of statuses.
    *
    * @param statuses the list of evaluation statuses
    * @param ignoreSustained true to ignore sustained errors, false otherwise
    */
  def apply(statuses: List[EvalStatus], ignoreSustained: Boolean): EvalStatus = {
    val fStatuses = statuses.filter(!_.isDisabled)
    if (fStatuses.nonEmpty) {
      val duration = DurationOps.sum(fStatuses.map(_.duration))
      fStatuses.collectFirst { case failed @ Failed(_, _) => failed } match {
        case Some(failed) => Failed(duration.toNanos, failed.error)
        case None =>
          fStatuses.collectFirst { case sustained @ Sustained(_, _) => sustained } match {
            case Some(sustained) =>
              if (ignoreSustained) Passed(duration.toNanos, false)
              else Sustained(duration.toNanos, sustained.error)
            case None =>
              if (fStatuses.forall(_.isLoaded)) {
                Loaded
              } else {
                fStatuses.filter(_ != Loaded).lastOption match {
                  case Some(lastStatus) => lastStatus match {
                    case _: Passed => Passed(duration.toNanos, false)
                    case Skipped => lastStatus
                    case _ => Pending
                  }
                  case None => Pending
                }
              }
          }
      }
    } else Skipped
  }

   /**
    * Groups counts by status name.
    *
    * @param statuses the statuses to group
    */
  def countsByType(statuses: List[EvalStatus]): Map[StatusKeyword, Int] =
    statuses.groupBy(_.keyword) map { case (k, v) => (k, v.size) }

}
