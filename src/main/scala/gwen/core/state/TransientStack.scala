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

import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.util.chaining._

import java.util.concurrent.TimeUnit
import java.util.Date
import gwen.core.status.EvalStatus

/**
  * Manages and maintains a temporary in memory stack of data.
  *
  * @author Branko Juric
  */
class TransientStack(stackName: String) extends ImplicitValueKeys {

  private val isParamStack = stackName == TransientStack.params
  /**
    * The transient stack.
    */
  private val transientStack = mutable.Stack[ScopedData]()

  def deepCloneInto(tStack: TransientStack): TransientStack = tStack tap { _ =>
    transientStack foreach { sd => 
      tStack.transientStack.push(sd.deepClone)
    }
  }

  def parseName(name: String): String = {
    if (isParamStack) s"<$name>" else name
  }

  def deepCopyInto(tStack: TransientStack): TransientStack = { 
    transientStack foreach { sd =>
      tStack.transientStack.push(sd)
    }
    tStack
  }

  /**
    * Adds the given data (name-value pairs) to a new scope
    * and pushes it onto the stack
    *
    * @param scope the name of the scope entry to add
    * @param data the data to add
    * @return the newly added scope
    */
  def push(scope: String, data: List[(String, String)]): ScopedData = {
    ScopedData(scope) tap { sd =>
      data foreach { case (name, value) =>
       sd.set(parseName(name), value)
      }
      transientStack.push(sd)
    }
  }

  /** Pops the current parameters off the stack. */
  def pop(): ScopedData = transientStack.pop()

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
      findOpt(name, s"${stackName}.", s"${stackName}.") orElse {
        if (stackName == "feature") {
          if (name == `gwen.eval.status.keyword`) getOpt(`gwen.feature.eval.status.keyword`)
          else if (name == `gwen.eval.status.message`) getOpt(`gwen.feature.eval.status.message`)
          else findOpt(name, "", "feature.") 
        } else None
      }
    }
  }

  private def findOpt(name: String, level1: String, level2: String): Option[String] = {
    if (name == s"gwen.${level1}eval.status.keyword.upperCased") getOpt(s"gwen.${level2}eval.status.keyword").map(_.toUpperCase)
    else if (name == s"gwen.${level1}eval.status.keyword.lowerCased") getOpt(s"gwen.${level2}eval.status.keyword").map(_.toLowerCase)
    else if (name == s"gwen.${level1}eval.status.message") getOpt(s"gwen.${level2}eval.status.message")
    else if (name == s"gwen.${level1}eval.status.message.escaped") getOpt(s"gwen.${level2}eval.status.message").map(Formatting.escapeJava)
    else if (name == s"gwen.${level1}eval.status.message.csvEscaped") getOpt(s"gwen.${level2}eval.status.message").map(Formatting.escapeCSV)
    else if (name == s"gwen.${level1}eval.status.isFailed") getOpt(s"gwen.${level2}eval.status.keyword").map(_ == StatusKeyword.Failed.toString).map(_.toString)
    else if (name == s"gwen.${level1}eval.status.isPassed") getOpt(s"gwen.${level2}eval.status.keyword").map(_ == StatusKeyword.Passed.toString).map(_.toString)
    else if (name == s"gwen.${level1}eval.duration.msecs") getOpt(s"gwen.${level2}eval.start.msecs").map(started => (new Date().getTime() - started.toLong).toString)
    else if (name == s"gwen.${level1}eval.duration.secs") getOpt(s"gwen.${level2}eval.duration.msecs").map(msecs => (msecs.toDouble / 1000d).toString)
    else if (name == s"gwen.${level1}eval.duration") getOpt(s"gwen.${level2}eval.duration.msecs").map(msecs => DurationFormatter.format(Duration(msecs.toLong, TimeUnit.MILLISECONDS)))
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
    transientStack.headOption.getOrElse(push(stackName, Nil)).set(parseName(name), value)
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
          val name = if (isParamStack) n.substring(1, n.length - 1) else n
          s"{ $name: $v }"
        } mkString ", "} ] }"
      case _ =>
        s"$stackName : { }"
    }
  }

}

object TransientStack {
  private val params = "params"
  def paramsStack: TransientStack = new TransientStack(params)
  def featureStack: TransientStack = new TransientStack("feature")
  def ruleStack: TransientStack = new TransientStack("rule")
  def scenarioStack: TransientStack = new TransientStack("scenario")
  def examplesStack: TransientStack = new TransientStack("examples")
  def stepDefStack: TransientStack = new TransientStack("stepDef")
}