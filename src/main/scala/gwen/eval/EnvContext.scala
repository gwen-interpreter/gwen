/*
 * Copyright 2014-2017 Branko Juric, Brady Wood
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

package gwen.eval

import java.io.{File, FileNotFoundException}

import gwen.Predefs.Exceptions
import gwen.Predefs.FileIO
import gwen.Predefs.Kestrel
import gwen.Predefs.Formatting._
import gwen.dsl.DataTable
import gwen.dsl.Failed
import gwen.dsl.Scenario
import gwen.dsl.Step
import com.typesafe.scalalogging.LazyLogging
import gwen.errors._
import gwen.Settings

import scala.sys.process._
import scala.util.Try
import gwen.dsl.Tag
import gwen.dsl.StepKeyword
import gwen.eval.support._

import scala.io.Source

/**
  * Base environment context providing access to all resources and services to 
  * engines.  Specific [[EvalEngine evaluation engines]] can 
  * define and use their own specific context by extending this one. 
  * 
  * Access to page scope data is provided through a dataScope method.
  * 
  * @author Branko Juric
  */
class EnvContext(options: GwenOptions, scopes: ScopedDataStack) extends Evaluatable
  with InterpolationSupport with RegexSupport with XPathSupport with JsonPathSupport
  with SQLSupport with ScriptSupport with DecodingSupport with LazyLogging {
  
  /** Map of step definitions keyed by callable expression name. */
  private var stepDefs = Map[String, Scenario]()
  
  /** List of current attachments (name-file pairs). */
  private var currentAttachments: List[(String, File)] = Nil
  private var attachmentPrefix = padWithZeroes(1)

  /** Dry run flag. */
  val isDryRun: Boolean = options.dryRun
  
  /** Current list of loaded meta (used to track and avoid duplicate meta loads). */
  var loadedMeta: List[File] = Nil

  /** Map of for-each StepDefs. */
  var foreachStepDefs: Map[String, Scenario] = Map[String, Scenario]()

  /** Provides access to the active data scope. */
  def activeScope = scopes
  
  /** Provides access to the global feature scope. */
  def featureScope: FeatureScope = scopes.featureScope

  /** Provides access to the local step scope. */
  private[eval] val stepScope = scopes.stepScope

  /**
    * Closes any resources associated with the evaluation context. This implementation
    * does nothing (but subclasses can override).
    */
  def close() { }
  
  /** Resets the current context but does not close it so it can be reused. */
  def reset() {
    scopes.reset()
    stepDefs = Map[String, Scenario]()
    resetAttachments()
    attachmentPrefix = padWithZeroes(1)
    loadedMeta = Nil
    foreachStepDefs = Map[String, Scenario]()
  }
    
  def asString: String = scopes.asString
  
  /** Returns the current visible scopes. */  
  def visibleScopes: ScopedDataStack = scopes.visible
  
  /**
   * Filters all attributes in all scopes based on the given predicate.
   * 
   * @param pred the predicate filter to apply; a (name, value) => boolean function
   * @return a new Scoped data stack containing only the attributes accepted by the predicate; 
   */
  def filterAtts(pred: ((String, String)) => Boolean): ScopedDataStack = scopes.filterAtts(pred) 
  
  /**
    * Gets a named data scope (creates it if it does not exist)
    * 
    * @param name the name of the data scope to get (or create and get)
    */
  def addScope(name: String): ScopedData = scopes.addScope(name)
  
  /**
    * Adds a step definition to the context.
    * 
    * @param stepDef the step definition to add
    */
  def addStepDef(stepDef: Scenario) {
    StepKeyword.names foreach { keyword =>
      if (stepDef.name.startsWith(keyword)) invalidStepDefError(stepDef, s"name cannot start with $keyword keyword")
    }
    val tags = stepDef.metaFile.map(meta => Tag(s"""Meta("${meta.getPath}")""")::stepDef.tags).getOrElse(stepDef.tags)
    stepDefs += ((stepDef.name, Scenario(tags, stepDef.name, stepDef.description, stepDef.background, stepDef.steps, stepDef.isOutline, stepDef.examples, stepDef.metaFile)))
  }
  
  /**
    * Gets the executable step definition for the given expression (if there is
    * one).
    * 
    * @param expression the expression to match
    * @return the step definition if a match is found; false otherwise
    */
  def getStepDef(expression: String): Option[(Scenario, List[(String, String)])] = 
    stepDefs.get(expression) match {
      case None => getStepDefWithParams(expression)
      case Some(stepDef) => Some((stepDef, Nil))
    }
  
  /**
    * Gets the paraterised step definition for the given expression (if there is
    * one).
    * 
    * @param expression the expression to match
    * @return the step definition and its parameters (name value tuples) if a 
    *         match is found; false otherwise
    */
  private def getStepDefWithParams(expression: String): Option[(Scenario, List[(String, String)])] = {
    val matches = stepDefs.values.view.flatMap { stepDef =>
      "<.+?>".r.findAllIn(stepDef.name).toList match {
        case Nil => None
        case names =>
          names.groupBy(identity).collectFirst { case (n, vs) if vs.size > 1 =>
            ambiguousCaseError(s"$n parameter defined ${vs.size} times in StepDef '${stepDef.name}'")
          }
          (stepDef.name.split(names.mkString("|")).toList match {
            case tokens if tokens.forall(expression.contains(_)) =>
              tokens.foldLeft((expression, List[String]())) { case ((expr, acc), token) =>
                expr.indexOf(token) match {
                  case -1 => ("", Nil)
                  case 0 => (expr.substring(token.length), acc)
                  case idx => (expr.substring(idx + token.length), expr.substring(0, idx) :: acc)
                }
              }
            case _ => ("", Nil)
          }) match {
            case (value, values) =>
              val params = names zip (if (value != "") value :: values else values).reverse
              if (expression == params.foldLeft(stepDef.name) { (result, param) => result.replace(param._1, param._2) }) {
                Some(stepDef, params)
              } else None
          }
      }
    }
    val iter = matches.iterator
    if (iter.hasNext) {
      val first = Some(iter.next)
      if (iter.hasNext) {
        val msg = s"Ambiguous condition in resolving '$expression': One StepDef match expected but ${matches.size} found" 
        ambiguousCaseError(s"$msg: ${matches.map { case (stepDef, _) => stepDef.name }.mkString(",")}")
      } else first
    } else None
  }
  
  /**
   * Gets the list of DSL steps supported by this context.  This implementation 
   * returns all user defined stepdefs. Subclasses can override to return  
   * addtional entries. The entries returned by this method are used for tab 
   * completion in the REPL.
   */
  def dsl: List[String] = stepDefs.keys.toList
  
  /**
    * Fail handler.
    * 
    * @param failure the failed status
    */
  final def fail(failure: Failed): Unit = { 
    addErrorAttachments(failure)
    if (options.batch) {
      logger.error(this.scopes.visible.asString)
    }
    logger.error(failure.error.getMessage)
    logger.debug(s"Exception: ", failure.error)
  }
  
  /**
    * Adds error attachments to the current context.
    * 
    * @param failure the failed status
    */
  def addErrorAttachments(failure: Failed): Unit = { 
    addAttachment("Error details", "txt", failure.error.writeStackTrace())
    addAttachment("Environment (all)", "txt", this.scopes.asString)
    addAttachment("Environment (visible)", "txt", this.scopes.visible.asString)
  }
  
  /**
   * Creates an attachment file.
   * 
   * @param name the attachment name
   * @param extension the filename extension
   * @param content the content to write to the file
   */
  def addAttachment(name: String, extension: String, content: String): (String, File) = { 
    val file = File.createTempFile(s"$attachmentPrefix-", s".$extension")
    file.deleteOnExit()
    Option(content) foreach { file.writeText }
    addAttachment((name, file))
  }
  
  /**
    * Adds an attachment to the current context.
    * 
    * @param attachment the attachment (name-file pair) to add
    */
  def addAttachment(attachment: (String, File)): (String, File) = 
    (attachment match {
      case (name, file) => 
        if (file.getName.startsWith(s"$attachmentPrefix-")) attachment
        else {
          val prefixedFilename = s"$attachmentPrefix-${file.getName}"
          val prefixedFile = if (file.getParentFile != null) new File(file.getParentFile, prefixedFilename) else new File(prefixedFilename)
          if (file.renameTo(prefixedFile)) (name, prefixedFile)
          else attachment
        }
    }) tap { att =>
      currentAttachments = att :: currentAttachments
      attachmentPrefix = padWithZeroes(attachmentPrefix.toInt + 1)
    }
  
  /** Resets/clears current attachments. */
  private[eval] def resetAttachments() {
    currentAttachments = Nil
  }
  
  /** Gets the list of attachments (sorted by file name).*/
  def attachments: List[(String, File)] = currentAttachments.sortBy(_._2 .getName())
  
  /**
    * Interpolate the given step before it is evaluated.
    * 
    * @param step the step to interpolate
    * @return the interpolated step
    */
  def interpolate(step: Step): Step = {
    val resolver: String => String = name => Try(stepScope.get(name)).getOrElse(getBoundReferenceValue(name))
    val iName = interpolate(step.name) { resolver }
    val iTable = step.table map { case (line, record) =>
      (line, record.map(cell => interpolate(cell) { resolver }))
    }
    val iDocString = step.docString map { case (line, content, contentType) =>
      (line, interpolate(content) { resolver }, contentType)
    }
    if (iName != step.name || iTable != step.table || iDocString != step.docString) {
      Step(step.keyword, iName, step.status, step.attachments, step.stepDef, iTable, iDocString) tap {
        iStep => logger.debug(s"Interpolated ${step.name} to: ${iStep.expression}${if (iTable.nonEmpty) ", () => dataTable" else ""}")
      }
    } else step
  }

  /**
    * Gets the scoped attribute or settings value bound to the given name.
    * Subclasses can override this method to perform additional lookups.
    *
    *  @param name the name of the attribute or value
    */
  def getBoundReferenceValue(name: String): String = {
    val attScopes = scopes.visible.filterAtts{case (n, _) => n.startsWith(name)}
    attScopes.findEntry { case (n, v) => n.matches(s"""$name(/(text|javascript|xpath.+|regex.+|json path.+|sysproc|file|sql.+))?""")} map {
      case (n, v) =>
        if (n == s"$name/text") v
        else if (n == s"$name/javascript")
          evaluate("$[dryRun:javascript]") {
            Option(evaluateJS(formatJSReturn(interpolate(v)(getBoundReferenceValue)))).map(_.toString).getOrElse("")
          }
        else if (n.startsWith(s"$name/xpath")) {
          val source = interpolate(getBoundReferenceValue(attScopes.get(s"$name/xpath/source")))(getBoundReferenceValue)
          val targetType = interpolate(attScopes.get(s"$name/xpath/targetType"))(getBoundReferenceValue)
          val expression = interpolate(attScopes.get(s"$name/xpath/expression"))(getBoundReferenceValue)
          evaluateXPath(expression, source, XMLNodeType.withName(targetType))
        }
        else if (n.startsWith(s"$name/regex")) {
          val source = interpolate(getBoundReferenceValue(attScopes.get(s"$name/regex/source")))(getBoundReferenceValue)
          val expression = interpolate(attScopes.get(s"$name/regex/expression"))(getBoundReferenceValue)
          extractByRegex(expression, source)
        }
        else if (n.startsWith(s"$name/json path")) {
          val source = interpolate(getBoundReferenceValue(attScopes.get(s"$name/json path/source")))(getBoundReferenceValue)
          val expression = interpolate(attScopes.get(s"$name/json path/expression"))(getBoundReferenceValue)
          evaluateJsonPath(expression, source)
        }
        else if (n == s"$name/sysproc") evaluate("$[dryRun:sysproc]") { v.!!.trim }
        else if (n == s"$name/file") {
          val filepath = interpolate(v)(getBoundReferenceValue)
          evaluate("$[dryRun:file]") {
            if (new File(filepath).exists()) {
              Source.fromFile(filepath).mkString
            } else throw new FileNotFoundException(s"File bound to '$name' not found: $filepath")
          }
        }
        else if (n.startsWith(s"$name/sql")) {
          val selectStmt = interpolate(attScopes.get(s"$name/sql/selectStmt"))(getBoundReferenceValue)
          val dbName = interpolate(attScopes.get(s"$name/sql/dbName"))(getBoundReferenceValue)
          evaluateSql(selectStmt, dbName)
        }
        else v
    } getOrElse {
      (featureScope.getObject("record") match {
        case Some(scope: ScopedData) =>
          scope.getOpt(name)
        case _ => featureScope.getObject("table") match {
          case Some(table: DataTable) => table.tableScope.getOpt(name)
          case _ => None
        }
      }).getOrElse {
          scopes.getOpt(name).getOrElse {
            Settings.getOpt(name).getOrElse {
              unboundAttributeError(name)
            }
          }
        }
      }
    } tap { value =>
      logger.debug(s"getBoundReferenceValue($name)='$value'")
    }
  
  def compare(expected: String, actual: String, operator: String, negate: Boolean): Boolean = {
    val res = operator match {
      case "be"      => expected.equals(actual)
      case "contain" => actual.contains(expected)
      case "start with" => actual.startsWith(expected)
      case "end with" => actual.endsWith(expected)
      case "match regex" => actual.matches(expected)
      case "match xpath" => !evaluateXPath(expected, actual, XMLNodeType.text).isEmpty
      case "match json path" => !evaluateJsonPath(expected, actual).isEmpty
    }
    if (!negate) res else !res
  }
  
}

/** Merges two contexts into one. */
class HybridEnvContext[A <: EnvContext, B <: EnvContext](val envA: A, val envB: B, val options: GwenOptions, val scopes: ScopedDataStack) extends EnvContext(options, scopes) {
  override def dsl: List[String] = envA.dsl ++ envB.dsl
  override def close() {
    try {
      envB.close()
    } finally {
      envA.close()
    }
  }
  override def getBoundReferenceValue(name: String): String =
    try {
      envB.getBoundReferenceValue(name)
    } catch {
      case _: Throwable => envA.getBoundReferenceValue(name)
    }
}