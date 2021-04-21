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

package gwen.dsl

import gwen._

import scala.concurrent.duration._

import java.util.Date

/** Captures the evaluation status of a [[SpecNode]]. */
sealed trait EvalStatus {
  
  val status: StatusKeyword.Value
  val nanos: Long
  val timestamp = new Date()
  
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
  
  def message: String = cause.map(_.getMessage).getOrElse(status.toString)

  override def toString: String =
    if (nanos > 0) {
      s"[${Formatting.formatDuration(duration)}] $status"
    } else status.toString
}

/**
  * Defines a passed evaluation status.
  * 
  * @param nanos the duration in nanoseconds
  */
case class Passed(nanos: Long) extends EvalStatus {
  val status = StatusKeyword.Passed
  override def exitCode = 0
  override def emoticon = "[:)]"
}

/**
  * Defines a failed evaluation status.
  * 
  * @param nanos the duration in nanoseconds
  * @param error the error
  */
case class Failed(nanos: Long, error: Throwable) extends EvalStatus {
  val status = StatusKeyword.Failed
  override def exitCode = 1
  override def emoticon = "[:(]"
  override def cause = Option(error.getCause)
  override def message: String = cause.map(_.getMessage).getOrElse(error.getMessage)
}

/**
  * Defines the sustained status.
  *
  * @param nanos the duration in nanoseconds
  * @param error the error to sustain
  */
case class Sustained(nanos: Long, error: Throwable) extends EvalStatus {
  val status = StatusKeyword.Sustained
  override def exitCode = 0
  override def emoticon = "[:|]"
  override def cause = Option(error.getCause)
  override def message: String = cause.map(_.getMessage).getOrElse(error.getMessage)
}

/** Defines the skipped status. */
case object Skipped extends EvalStatus {
  val nanos = 0L
  val status = StatusKeyword.Skipped
  override def exitCode = 0
  override def emoticon = "[:|]"
}

/**
  * Defines the pending status.
  */
case object Pending extends EvalStatus {
  val nanos = 0L 
  val status = StatusKeyword.Pending
  override def exitCode = 1
  override def emoticon = "[:|]"
}

/** Defines the loaded status. */
case object Loaded extends EvalStatus {
  val nanos = 0L
  val status = StatusKeyword.Loaded
  override def exitCode = 0
  override def emoticon = "[:)]"
}

/** Defines the disabled status. */
case object Disabled extends EvalStatus {
  val nanos = 0L
  val status = StatusKeyword.Disabled
  override def exitCode = 0
  override def emoticon = "[:)]"
}

object EvalStatus {
  
  /**
    * Function for getting the effective evaluation status of a given list of statuses.
    * 
    * @param statuses the list of evaluation statuses
    */
  def apply(statuses: List[EvalStatus]): EvalStatus = apply(statuses, ignoreSustained = true)

  /**
    * Function for getting the effective evaluation status of a given list of statuses.
    *
    * @param statuses the list of evaluation statuses
    * @param ignoreSustained true to ignore sustained errors, false otherwise
    */
  def apply(statuses: List[EvalStatus], ignoreSustained: Boolean): EvalStatus = {
    val fStatuses = statuses.filter(s => !EvalStatus.isDisabled(s.status))
    if (fStatuses.nonEmpty) {
      val duration = DurationOps.sum(fStatuses.map(_.duration))
      fStatuses.collectFirst { case failed @ Failed(_, _) => failed } match {
        case Some(failed) => Failed(duration.toNanos, failed.error)
        case None =>
          fStatuses.collectFirst { case sustained @ Sustained(_, _) => sustained } match {
            case Some(sustained) =>
              if (ignoreSustained) Passed(duration.toNanos)
              else Sustained(duration.toNanos, sustained.error)
            case None =>
              if (fStatuses.forall(_ == Loaded)) {
                Loaded
              } else {
                fStatuses.filter(_ != Loaded).lastOption match {
                  case Some(lastStatus) => lastStatus match {
                    case Passed(_) => Passed(duration.toNanos)
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
    * Returns true if the given status is an evaluated status. A status is considered evalauted if it is
    * Passed, Failed, or Disabled.
    *
    * @param status the status to check
    * @return true if the status is Passed, Failed or Disabled, false otherwise
    */
  def isEvaluated(status: StatusKeyword.Value): Boolean = status == StatusKeyword.Passed || isDisabled(status) || isError(status)

   /**
    * Returns true if the given status is Disabled.
    *
    * @param status the status to check
    * @return true if the status is Disabled, false otherwise
    */
  def isDisabled(status: StatusKeyword.Value): Boolean = status == StatusKeyword.Disabled

  /**
    * Returns true if the given status is Sustained.
    *
    * @param status the status to check
    * @return true if the status is Sustained, false otherwise
    */
  def isSustained(status: StatusKeyword.Value): Boolean = status == StatusKeyword.Sustained

  /**
    * Returns true if the given status is an error status. A status is considered an error if it is
    * Failed or Sustained
    *
    * @param status the status to check
    * @return true if the status is Failed or Sustained, false otherwise
    */
  def isError(status: StatusKeyword.Value): Boolean = status == StatusKeyword.Failed || isSustained(status)
  
}

/**
  * Enumeration of supported status keywords.
  * 
  * @author Branko Juric
  */
object StatusKeyword extends Enumeration {

  val Passed, Failed, Sustained, Skipped, Pending, Loaded, Disabled = Value
  
  val reportables = List(Passed, Failed, Sustained, Skipped, Pending)

  /**
    * Groups counts by status.
    * 
    * @param statuses the statuses to group
    */
  def countsByStatus(statuses: List[EvalStatus]): Map[StatusKeyword.Value, Int] = 
    statuses.groupBy(_.status) map { case (k, v) => (k, v.size) }
}