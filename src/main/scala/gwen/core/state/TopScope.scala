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
import gwen.core.eval.binding.ScopeRefBinding
import gwen.core.node.gherkin.Spec
import gwen.core.status.EvalStatus
import gwen.core.status.StatusKeyword

import net.minidev.json.JSONArray

import scala.util.chaining._
import scala.jdk.CollectionConverters._

/**
  * Binds all top level attributes. Also included is a cache for storing non string objects. 
  * The top scope can be a feature or scenario level scope depending on the `gwen.state.level` setting.
  *
  * @author Branko Juric
  */
class TopScope() extends ScopedData(GwenSettings.`gwen.state.level`.toString) with ImplicitValueKeys {

  override val isTopScope = true

  /**
    *  Provides access to the current (non top) scope.
    */
  private [state] var currentScope: Option[ScopedData] = None

  /**
    *  Provides access to the rule scope.
    */
  val featureScope = TransientStack.featureStack

  /**
    *  Provides access to the rule scope.
    */
  val ruleScope = TransientStack.ruleStack

  /**
    *  Provides access to the scenario scope.
    */
  val scenarioScope = TransientStack.scenarioStack

  /**
    *  Provides access to the rule scope.
    */
  val examplesScope = TransientStack.examplesStack

  /**
    *  Provides access to the stepDef scope.
    */
  val stepDefScope = TransientStack.stepDefStack

  /** Map of object stacks. */
  private var objectStack = Map[String, List[Any]]()

  def deepCopyInto(tScope: TopScope): TopScope = tScope tap { _ => 
    tScope.currentScope = currentScope.map(_.deepClone)
    tScope.objectStack = objectStack
    copyImplicitsInto(tScope)
    super.deepCopyInto(tScope)
  }

  def copyImplicitsInto(tScope: TopScope): TopScope = tScope tap { _ =>
    featureScope.deepCloneInto(tScope.featureScope)
    ruleScope.deepCloneInto(tScope.ruleScope)
    scenarioScope.deepCloneInto(tScope.scenarioScope)
    stepDefScope.deepCloneInto(tScope.stepDefScope)
  }

  /**
    * Binds a new attribute value to the scope.  If an attribute of the same
    * name already exists, then this new attribute overrides the existing one
    * but does not replace it. If an attribute of the same name exists in the
    * current scope, then a referene to the top attribute is added to the current scope.
    *
    * @param name the name of the attribute to bind
    * @param value the value to bind to the attribute
    * @return the current scope containing the old attributes plus the
    *         newly added attribute
    */
  override def set(name: String, value: String): ScopedData =
    super.set(name, value) tap { _=>
      currentScope foreach { cs =>
        if (cs.findEntries { case (n, _) => n == name || n.startsWith(s"$name/") } nonEmpty) {
          cs.set(ScopeRefBinding.key(name), scope)
        }
      }
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

  def initStart(timeMsecs: Long): Unit = {
    featureScope.set(`gwen.feature.eval.start.msecs`, timeMsecs.toString)
  }
  
  def setImplicitAtts(spec: Option[Spec], status: EvalStatus, force: Boolean): Unit = {
    spec foreach { s => 
      featureScope.set(`gwen.feature.name`, s.feature.name)
      featureScope.set(`gwen.feature.displayName`, s.feature.displayName)
      s.specFile foreach { file =>
        featureScope.set(`gwen.feature.file.name`, file.getName)
        featureScope.set(`gwen.feature.file.simpleName`, file.simpleName)
        featureScope.set(`gwen.feature.file.path`, file.getPath)
        featureScope.set(`gwen.feature.file.absolutePath`, file.getAbsolutePath)
      }
    }
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
    set(`gwen.data.record.number`, rec.occurrence.number.toString)
    set(`gwen.data.record.index`, rec.occurrence.index.toString)
  }

  override def get(name: String): String = getImplicitOpt(name).getOrElse(super.get(name))
  override def getOpt(name: String): Option[String] = getImplicitOpt(name).orElse(super.getOpt(name))

  private def getImplicitOpt(name: String): Option[String] = {
    if (name.startsWith(`gwen.feature.`)) featureScope.getOpt(name)
    else if (name.startsWith(`gwen.scenario.`)) scenarioScope.getOpt(name)
    else if (name.startsWith(`gwen.examples.`)) examplesScope.getOpt(name)
    else if (name.startsWith(`gwen.stepDef.`)) stepDefScope.getOpt(name)
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
    else None
  }

}
