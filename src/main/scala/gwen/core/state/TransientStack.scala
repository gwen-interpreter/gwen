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

package gwen.core.state

import gwen.core._
import gwen.core.Formatting.DurationFormatter
import gwen.core.node.gherkin.Annotations
import gwen.core.status.StatusKeyword
import gwen.core.status.EvalStatus

import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.util.chaining._

import java.util.concurrent.TimeUnit
import java.util.Date

/**
  * Manages and maintains a temporary in memory stack of data.
  *
  * @author Branko Juric
  */
abstract class TransientStack(stackName: String) extends ImplicitValueKeys {

  /**
    * The transient stack.
    */
  private val transientStack = mutable.Stack[ScopedData]()

  private[state] def bindingName(name: String): String = name
  private[state] def rawName(name: String): String = name

  def deepCloneInto(tStack: TransientStack): TransientStack = tStack tap { _ =>
    transientStack foreach { sd => 
      tStack.transientStack.push(sd.deepClone)
    }
  }

  def deepCopyInto(tStack: TransientStack): TransientStack = { 
    transientStack foreach { sd =>
      tStack.transientStack.push(sd)
    }
    tStack
  }

  def boundary[T](scope: String, data: List[(String, String)])(body: =>T): T = {
    push(scope, data)
    try {
      body
    } finally {
      pop()
    }
  }

  /**
    * Adds the given data (name-value pairs) to a new scope
    * and pushes it onto the stack
    *
    * @param scope the name of the scope entry to add
    * @param data the data to add
    * @return the newly added scope
    */
  private[state] def push(scope: String, data: List[(String, String)]): ScopedData = {
    ScopedData(scope) tap { sd =>
      data foreach { case (name, value) =>
       sd.set(bindingName(name), value)
      }
      transientStack.push(sd)
    }
  }

  /** Pops the current parameters off the stack. */
  private def pop(): ScopedData = transientStack.pop()

  /**
    * Finds and retrieves data bound in the current stack.
    *
    * @param name the name of the data element to find
    * @return the value if it is found (or throws error)
    */
  def get(name: String): String =
    getOpt(name).getOrElse(Errors.unboundAttributeError(name, stackName))

  /**
    * Finds and retrieves an optional data bound in the current stack.
    *
    * @param name the name of the data to find
    * @return Some(value) if a value is found or None otherwise
    */
  def getOpt(name: String): Option[String] = {
    transientStack.headOption.flatMap(_.getOpt(name)).headOption orElse {
      findOpt(name)
    }
  }

  private def findOpt(name: String): Option[String] = {
    if (name == s"gwen.$stackName.eval.status.keyword.upperCased") getOpt(s"gwen.$stackName.eval.status.keyword").map(_.toUpperCase)
    else if (name == s"gwen.$stackName.eval.status.keyword.lowerCased") getOpt(s"gwen.$stackName.eval.status.keyword").map(_.toLowerCase)
    else if (name == s"gwen.$stackName.eval.status.message.escaped") getOpt(s"gwen.$stackName.eval.status.message").map(Formatting.escapeJava)
    else if (name == s"gwen.$stackName.eval.status.message.csvEscaped") getOpt(s"gwen.$stackName.eval.status.message").map(Formatting.escapeCSV)
    else if (name == s"gwen.$stackName.eval.status.isFailed") getOpt(s"gwen.$stackName.eval.status.keyword").map(_ == StatusKeyword.Failed.toString).map(_.toString)
    else if (name == s"gwen.$stackName.eval.status.isPassed") getOpt(s"gwen.$stackName.eval.status.keyword").map(_ == StatusKeyword.Passed.toString).map(_.toString)
    else if (name == s"gwen.$stackName.eval.duration.msecs") getOpt(s"gwen.$stackName.eval.start.msecs").map(started => (new Date().getTime() - started.toLong).toString)
    else if (name == s"gwen.$stackName.eval.duration.secs") getOpt(s"gwen.$stackName.eval.duration.msecs").map(msecs => (msecs.toDouble / 1000d).toString)
    else if (name == s"gwen.$stackName.eval.duration") getOpt(s"gwen.$stackName.eval.duration.msecs").map(msecs => DurationFormatter.format(Duration(msecs.toLong, TimeUnit.MILLISECONDS)))
    else None
  }

  /**
    * Binds a data value in the current stack.
    *
    * @param name the name of the data element to bind
    * @param value the value to bind
    * @return this stack object
    */
  def set(name: String, value: String): TransientStack = {
    transientStack.headOption.getOrElse(push(stackName, Nil)).set(bindingName(name), value)
    this
  }

  def setStatus(status: EvalStatus, force: Boolean): Unit = {
    if (status.isFailed || getOpt(s"gwen.$stackName.eval.status.keyword").map(_ != StatusKeyword.Failed.toString).getOrElse(true) || force) {
      set(s"gwen.$stackName.eval.status.keyword", (if(status.isPending || status.isFailed) status.keyword else StatusKeyword.Passed).toString)
      set(s"gwen.$stackName.eval.status.message", if (status.isFailed) status.message else "")
    }
  }

  /**
    * Checks whether or not the stack contains the
    * given scope.
    *
    * @param scope the scope name to check
    * @return true if the scope is found; false otherwise
    */
  def containsScope(scope: String): Boolean = transientStack.exists(_.scope == scope)

  /** Checks whether or not the local stack is empty. */
  def isEmpty = transientStack.isEmpty

  /**
    * Returns a string representation of data in the current stack
    */
  override def toString: String = {
    transientStack.headOption.map(scope => (scope.scope, scope.findEntries(_ => true).toList)) match {
      case Some((scope, entries)) if entries.nonEmpty =>
        s"$stackName : { scope: $scope, entries : [ ${entries map { case (n, v) =>
          s"{ ${rawName(n)}: $v }"
        } mkString ", "} ] }"
      case _ =>
        s"$stackName : { }"
    }
  }

}

class TransientNodeStack(stackName: String) extends TransientStack(stackName) {
  override def push(scope: String, data: List[(String, String)]): ScopedData = {
    val start = new Date()
    super.push(
      scope,
      List(
        (s"gwen.$stackName.name", scope),
        (s"gwen.$stackName.eval.start.msecs", start.getTime().toString),
        (s"gwen.$stackName.eval.started", start.toString),
        (s"gwen.$stackName.eval.status.keyword", StatusKeyword.Passed.toString),
        (s"gwen.$stackName.eval.status.message", "")
      ) ++ data
    )
  }
}

class TransientDataStack(stackName: String) extends TransientStack(stackName)

object TransientStack {
  def paramsStack: TransientStack = new TransientDataStack("params") {
    override def bindingName(name: String): String = s"<$name>"
    override def rawName(name: String): String = name.drop(1).dropRight(1)
  }
  def featureStack: TransientStack = new TransientNodeStack("feature")
  def ruleStack: TransientStack = new TransientNodeStack("rule")
  def scenarioStack: TransientStack = new TransientNodeStack("scenario")
  def examplesStack: TransientStack = new TransientNodeStack("examples")
  def stepDefStack: TransientStack = new TransientNodeStack("stepDef")
}