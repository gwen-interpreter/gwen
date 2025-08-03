/*
 * Copyright 2016-2024 Branko Juric, Brady Wood
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
import gwen.core.data.DataRecord
import gwen.core.node.gherkin.Spec
import gwen.core.state.StateLevel
import gwen.core.status.EvalStatus
import gwen.core.status.StatusKeyword

import net.minidev.json.JSONArray

import scala.util.chaining._
import scala.jdk.CollectionConverters._


/**
  * Binds all top level attributes. Also included is a cache for various scopes and arbitrary objects. 
  *
  * @author Branko Juric
  */
class TopScope(stateLevel: StateLevel) extends ScopedData(stateLevel.toString) with ImplicitValueKeys {

  override val isTopScope = true

  val featureScope = ScopedDataStack.featureStack
  val ruleScope = ScopedDataStack.ruleStack
  val scenarioScope = ScopedDataStack.scenarioStack
  val examplesScope = ScopedDataStack.examplesStack
  val stepDefScope = ScopedDataStack.stepDefStack
  val paramScope = ScopedDataStack.paramsStack
  val iterationScope = ScopedDataStack.iterationStack

  /** Map of object stacks. */
  private var objectStack = Map[String, List[Any]]()

  def deepClone(stateLevel: StateLevel): TopScope = deepCloneInto(new TopScope(stateLevel))

  def deepCloneInto(tScope: TopScope): TopScope = tScope tap { _ => 
    tScope.objectStack = objectStack
    copyImplicitsInto(tScope)
    super.deepCopyInto(tScope)
  }

  def copyImplicitsInto(tScope: TopScope): TopScope = tScope tap { _ =>
    featureScope.deepCloneInto(tScope.featureScope)
    ruleScope.deepCloneInto(tScope.ruleScope)
    scenarioScope.deepCloneInto(tScope.scenarioScope)
    stepDefScope.deepCloneInto(tScope.stepDefScope)
    paramScope.deepCloneInto(tScope.paramScope)
    iterationScope.deepCloneInto(tScope.iterationScope)
  }

  /**
    * Pushes a named object to the object stack.
    *
    * @param name the name to bind the object to
    * @param obj the object to push
    */
  def pushObject(name: String, obj: Any): Unit = {
    objectStack.get(name) match {
      case Some(objs) => objectStack += (name -> (obj :: objs))
      case None => objectStack += (name -> List(obj))
    }
  }

  /**
    * Gets a bound object from the object stack.
    *
    * @param name the name of the bound object to get
    * @return Some(bound object) or None
    */
  def getObject(name: String): Option[Any] = objectStack.get(name).flatMap(_.headOption)

  /**
    * Clears a bound object from the object stack. Performs no operation if no object is not bound to the name.
    *
    * @param name the name of the bound object to pop
    */
  def popObject(name: String): Option[Any] = {
    objectStack.get(name) match {
      case Some(head::tail) =>
        if (tail.nonEmpty) {
          objectStack += (name -> tail)
        } else {
          objectStack -= name
        }
        Option(head)
      case obj @ Some(_) =>
        objectStack -= name
        obj
      case _ => None
    }
  }

  def removeObject(name: String): Unit = {
    objectStack -= name
  }
  
  def setStatus(status: EvalStatus, force: Boolean): Unit = {
    featureScope.setStatus(status, force)
    ruleScope.setStatus(status, false)
    scenarioScope.setStatus(status, false)
    examplesScope.setStatus(status, false)
    stepDefScope.setStatus(status, false)
  }

  def bindDataRecord(rec: DataRecord): Unit = {
    rec.data foreach { case (name, value) =>
      set(name, value)
    }
    featureScope.set(`gwen.data.record.number`, rec.occurrence.number.toString)
    featureScope.set(`gwen.data.record.index`, rec.occurrence.index.toString)
  }

  override def get(name: String): String = getImplicitOpt(name).getOrElse(super.get(name))
  override def getOpt(name: String): Option[String] = getImplicitOpt(name).orElse(super.getOpt(name))

  private def getImplicitOpt(name: String): Option[String] = {
    if (name.startsWith(`gwen.feature.`) || name.startsWith(`gwen.data.record.`)) featureScope.getOpt(name)
    else if (name.startsWith(`gwen.scenario.`)) scenarioScope.getOpt(name)
    else if (name.startsWith(`gwen.examples.`)) examplesScope.getOpt(name)
    else if (name.startsWith(`gwen.stepDef.`)) stepDefScope.getOpt(name)
    else if (name.startsWith(`gwen.iteration.`)) iterationScope.getOpt(name)
    else if (name.startsWith(`gwen.rule.`)) ruleScope.getOpt(name)
    else if (name == `gwen.accumulated.errors`) getObject(`gwen.accumulated.errors`).map(_.asInstanceOf[List[String]]) map { errs => 
      errs match {
        case Nil => ""
        case err :: Nil => err
        case _ => s"${errs.size} errors:\n${errs.zipWithIndex.map { (e, i) => s"(${i+1}) $e" } mkString "\n" }"
      }
    } orElse(Some(""))
    else if (name == `gwen.accumulated.errors:JSONArray`) getObject(`gwen.accumulated.errors`).map(_.asInstanceOf[List[String]]) map { errs => 
      JSONArray.toJSONString(errs.asJava)
    } orElse(Some(JSONArray.toJSONString(Nil.asJava)))
    else {
      deprecatedImplicitOpt(name) flatMap { newName => 
        Deprecation.log("Implicit value reference", name.takeWhile(_ != '/'), Some(newName))
        getImplicitOpt(newName)
      }
    }
  }

  private def deprecatedImplicitOpt(name: String): Option[String] = {
    if (name.startsWith("gwen.eval.")) {
      Some(s"gwen.feature.eval.${name.substring("gwen.eval.".length)}").map(_.takeWhile(_ != '/'))
    } else if (name == "data record number" || name == "data.record.number") {
      Some("gwen.data.record.number	")
    } else if (name == "data record index" || name == "data.record.index") {
      Some("gwen.data.record.index")
    } else if (name == "record.number") {
      Some("gwen.table.record.number")
    } else if (name == "record.index") {
      Some("gwen.table.record.index")
    } else if (name == "iteration.number") {
      Some("gwen.iteration.number")
    } else if (name == "iteration.index") {
      Some("gwen.iteration.index")
    } else {
      None
    }
  }

   /**
   * Filters all contained attributes in all scopes based on the given predicate.
   *
   * @param pred the predicate filter to apply; a (name, value) => boolean function
   * @return Some(ScopedData) containing only the attributes accepted by the predicate;
   */
  def filterAllAtts(pred: ((String, String)) => Boolean): List[ScopedData] = {
    List(
      featureScope.filterAtts(pred),
      ruleScope.filterAtts(pred),
      examplesScope.filterAtts(pred),
      scenarioScope.filterAtts(pred),
      stepDefScope.filterAtts(pred),
      paramScope.filterAtts(pred),
      iterationScope.filterAtts(pred)
    ).flatten ++ List(filterAtts(pred))
  }

  /**
    * Returns this entire scope as a String.
    */
  def asString(all: Boolean, env: Boolean): String = {
    StringPrinter.withPrinter { pw =>
      if (all) {
        if (env) pw.println("""env : "implicits" {""")
        featureScope.asString.linesIterator foreach {line => pw.println(s"${if (env) "  " else ""}$line")}
        if (ruleScope.nonEmpty) ruleScope.asString.linesIterator foreach {line => pw.println(s"${if (env) "  " else ""}$line")} else ""
        if (examplesScope.nonEmpty) examplesScope.asString.linesIterator foreach {line => pw.println(s"${if (env) "  " else ""}$line")} else ""
        if (scenarioScope.nonEmpty) scenarioScope.asString.linesIterator foreach {line => pw.println(s"${if (env) "  " else ""}$line")} else ""
        if (stepDefScope.nonEmpty) stepDefScope.asString.linesIterator foreach {line => pw.println(s"${if (env) "  " else ""}$line")} else ""
        if (paramScope.nonEmpty) paramScope.asString.linesIterator foreach {line => pw.println(s"${if (env) "  " else ""}$line")} else ""
        if (iterationScope.nonEmpty) iterationScope.asString.linesIterator foreach {line => pw.println(s"${if (env) "  " else ""}$line")} else ""
        if (env) pw.println("}")
      }
      pw.print(super.asString(env))
    }
  }

}
