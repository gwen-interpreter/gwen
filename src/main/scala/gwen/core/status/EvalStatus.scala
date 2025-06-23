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
  def isIgnored: Boolean = keyword == StatusKeyword.Ignored
  def isAbstained: Boolean = false

  def isEvaluated: Boolean = isPassed || isDisabled || isIgnored || isError
  def isError: Boolean = isFailed || isSustained

  /** Returns the duration in nanoseconds. */
  def duration: Duration = Duration.fromNanos(nanos)

  /** Must be overriden to return exit code. */
  def exitCode: Int

  /** Should be overriden to return an icon. */
  def icon: Option[String] = None

  /** Must be overriden to return an emoticon (legacy). */
  def emoticon: String

  /** Optional error cause. */
  def cause: Option[Throwable] = None

  /** Determines whether or not this status is due to a technical error. */
  def isTechError: Boolean = !isLicenseError && !isDisabledError

  /** Determines whether or not this status is due to an assertion error. */
  def isAssertionError: Boolean =
    cause.exists(c => c != null && c.isInstanceOf[Errors.GwenAssertionError])

  def isAccumulatedAssertionError: Boolean =
    cause.exists(c => c != null && c.isInstanceOf[Errors.AccumulatedAssertionError])

  def isHardAssertionError = isAssertionError(AssertionMode.hard)
  def isSoftAssertionError = isAssertionError(AssertionMode.soft)
  def isSustainedAssertionError = isAssertionError(AssertionMode.sustained)
  def isAssertionError(mode: AssertionMode): Boolean = isAssertionError && cause.map(_.asInstanceOf[Errors.GwenAssertionError].mode == mode).getOrElse(false)
  def isDeprecationError = cause.map(_.isInstanceOf[Errors.DeprecatedException]).getOrElse(false)

  /** Determines whether or not this status is due to an disabled step error. */
  def isDisabledError: Boolean =
    cause.exists(c => c != null && c.isInstanceOf[Errors.DisabledStepException])

  /** Determines whether or not this status is due to a license error. */
  def isLicenseError: Boolean =
    cause.exists(c => c != null && c.isInstanceOf[Errors.LicenseException])

  def message: String = cause.map(_.getMessage).getOrElse(keyword.toString)

  override def toString: String = asString(keyword.toString)

  def asString(statusName: String): String = asString(statusName, statusName)
  def asIconString(statusName: String): String = asString(icon.getOrElse(statusName), statusName)
  def asStatusIconString(statusName: String): String = {
    val statusStr = s"$statusName${icon.map(i => s" $i").getOrElse("")}"
    asString(statusStr, statusStr)
  }

  def asString(evalName: String, statusName: String): String =
    if (nanos > 0) {
      s"[${Formatting.formatDuration(duration)}] $evalName"
    } else statusName
}


trait EvalError(error: Throwable) extends EvalStatus {
  override def cause = {
    Option(error) map { e => 
      Option(e.getCause).getOrElse(error) 
    }
  }
  override def message: String = {
    if (error != null && error.isInstanceOf[Errors.CustomErrorMessage]) error.getMessage
    else cause.map(_.getMessage).orElse(Option(error.getMessage)).getOrElse(error.getClass.getSimpleName)
  }
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
    val fStatuses = statuses.filter(s => !s.isDisabled && !s.isSkipped)
    if (fStatuses.nonEmpty) {
      val duration = DurationOps.sum(fStatuses.map(_.duration))
      fStatuses.collectFirst { case failed: Failed if failed.isHardAssertionError => failed } match {
        case Some(failed) => Failed(duration.toNanos, failed.error)
        case None =>
          fStatuses.collectFirst { case failed: Failed if !failed.isHardAssertionError => failed } match {
            case Some(failed) => Failed(duration.toNanos, failed.error)
            case None =>
              fStatuses.collectFirst { case sustained: Sustained => sustained } match {
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
                        case _: Ignored => Passed(duration.toNanos, false)
                        case _ => Pending
                      }
                      case None => Pending
                    }
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
