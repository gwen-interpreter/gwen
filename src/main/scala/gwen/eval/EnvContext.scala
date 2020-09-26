/*
 * Copyright 2014-2020 Branko Juric, Brady Wood
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
import gwen.dsl._
import com.typesafe.scalalogging.LazyLogging
import gwen.Errors._
import gwen.{GwenSettings, Settings}

import scala.sys.process._
import scala.util.{Failure, Success, Try}
import gwen.eval.support._

import scala.io.Source
import scala.util.matching.Regex

/**
  * Base environment context providing access to all resources and state.
  * 
  * @author Branko Juric
  */
class EnvContext(options: GwenOptions) extends Evaluatable
  with InterpolationSupport with RegexSupport with XPathSupport with JsonPathSupport
  with SQLSupport with ScriptSupport with DecodingSupport with TemplateSupport with LazyLogging {
  
  /** Current list of loaded meta (used to track and avoid duplicate meta loads). */
  var loadedMeta: List[File] = Nil

  /** Map of step definitions keyed by callable expression name. */
  private var stepDefs = Map[String, Scenario]()

  /** Scoped attributes and captured state. */
  private var state = new EnvState(new ScopedDataStack())

  /** Dry run flag. */
  val isDryRun: Boolean = options.dryRun

  /** Parallel features or scenarios execution mode flag. */
  val isParallel: Boolean = options.parallel

  /** Parallel feature execution mode flag. */
  val isParallelFeatures: Boolean = options.parallelFeatures

  /** Parallel scenario execution mode flag. */
  val isParallelScenarios: Boolean = options.isParallelScenarios

  /** Provides access to the configures state level. */
  val stateLevel: StateLevel.Value = GwenSettings.`gwen.state.level`

  /** Provides access to the active data scope. */
  def scopes = state.scopes
  
  /** Provides access to the top level scope. */
  def topScope: TopScope = scopes.topScope

  /** Provides access to the local step scope. */
  private[eval] def stepScope = scopes.stepScope

  /** Create a clone that preserves scoped data. */
  def clone[T <: EnvContext](engine: EvalEngine[T]): T = {
    engine.init(options) tap { clone => 
      clone.loadedMeta = loadedMeta
      clone.stepDefs = stepDefs
      clone.state = EnvState(topScope)
    }
  }

  /**
    * Closes any resources associated with the evaluation context. This implementation
    * does nothing (but subclasses can override).
    */
  def close(): Unit = { }
  
  /** Resets the current context but does not close it so it can be reused. */
  def reset(level: StateLevel.Value): Unit = {
    logger.info(s"Resetting environment context")
    state = EnvState(topScope)
    if (StateLevel.feature.equals(level)) {
      stepDefs = Map[String, Scenario]()
      loadedMeta = Nil
    }
  }
    
  def asString: String = scopes.asString

  def specFile: Option[File] = topScope.getObject("spec.file").map(_.asInstanceOf[File])
  
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
  def addStepDef(stepDef: Scenario): Unit = {
    StepKeyword.names foreach { keyword =>
      if (stepDef.name.startsWith(keyword)) invalidStepDefError(stepDef, s"name cannot start with $keyword keyword")
    }
    val tags = stepDef.metaFile.map(meta => Tag(s"""Meta("${meta.getPath}")""")::stepDef.tags).getOrElse(stepDef.tags)
    val sd = Scenario(tags, FeatureKeyword.nameOf(FeatureKeyword.Scenario), stepDef.name, stepDef.description, stepDef.background, stepDef.steps, stepDef.isOutline, stepDef.examples, stepDef.metaFile) tap { sd =>
      sd.pos = stepDef.pos
    }
    stepDefs += ((stepDef.name, sd))
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
      val pattern = Regex.quote(stepDef.name).replaceAll("<.+?>", """\\E(.*?)\\Q""").replaceAll("""\\Q\\E""", "")
      if (expression.matches(pattern)) {
        val names = "<.+?>".r.findAllIn(stepDef.name).toList
        names.groupBy(identity).collectFirst { case (n, vs) if vs.size > 1 =>
          ambiguousCaseError(s"$n parameter defined ${vs.size} times in StepDef '${stepDef.name}'")
        }
        val values = pattern.r.unapplySeq(expression).get
        val params = names zip values
        val resolved = params.foldLeft(stepDef.name) { (result, param) => result.replace(param._1, param._2) }
        if (expression == resolved) {
          Some((stepDef, params))
        } else None
      } else {
        None
      }
    }
    val iter = matches.iterator
    if (iter.hasNext) {
      val first = Some(iter.next())
      if (iter.hasNext) {
        val msg = s"Ambiguous condition in resolving '$expression': 1 StepDef match expected but ${matches.size} found"
        ambiguousCaseError(s"$msg: ${matches.map { case (stepDef, _) => stepDef.name }.mkString(",")}")
      } else first
    } else None
  }

  def addForeachStepDef(step: Step, stepDef: Scenario): Unit = {
    state.addForeachStepDef(step, stepDef)
  }

  /** Gets the optional for-each StepDef for a given step. */
  def getForeachStepDef(step: Step): Option[Scenario] = state.popForeachStepDef(step)

  /** Adds current behavior. */
  def addBehavior(behavior: BehaviorType.Value): BehaviorType.Value = 
    behavior tap { _ => state.addBehavior(behavior) }

  /** Removes the current behavior. */
  def popBehavior(): Option[BehaviorType.Value] = state.popBehavior()

  /** Gets the current behavior. */
  def currentBehavior: Option[BehaviorType.Value] = state.currentBehavior

  /** Checks if a feature file is currently being evaluated. */
  def isEvaluatingFeatureFile: Boolean = specFile.map(FileIO.isFeatureFile).getOrElse(false)

  /** Checks if a top level step is currently being evaluated). */
  def isEvaluatingTopLevelStep: Boolean = stepScope.isEmpty
  
  /**
   * Gets the list of DSL steps supported by this context.  This implementation 
   * returns all user defined stepdefs. Subclasses can override to return  
   * addtional entries. The entries returned by this method are used for tab 
   * completion in the REPL.
   */
  def dsl: List[String] = stepDefs.keys.toList

  /**
    * Binds all accumulated attachments to the given step.
    *
    * @param step the step to bind attachments to
    * @return the step with accumulated attachments
    */
  private[eval] def finaliseStep(step: Step): Step = {
    step.evalStatus match {
      case failure @ Failed(_, _) if !step.attachments.exists{ case (n, _) => n == "Error details"} =>
        if (!failure.isDisabledError) {
          if (options.batch) {
            logger.error(scopes.visible.asString)
          }
          logger.error(failure.error.getMessage)
          addErrorAttachments(failure)
        }
        logger.whenDebugEnabled {
          logger.error(s"Exception: ", failure.error)
        }
      case _ => // noop
    }
    val fStep = if (state.hasAttachments) {
      Step(step, step.evalStatus, (step.attachments ++ state.popAttachments()).sortBy(_._2 .getName()))
    } else {
      step
    }
    fStep.evalStatus match {
      case status @ Failed(nanos, error) =>
        if (status.isAssertionError && AssertionMode.isSustained) {
          Step(fStep, Sustained(nanos, error))
        } else if (status.isDisabledError) {
          Step(fStep, Disabled)
        } else {
          fStep
        }
      case _ =>
        fStep
    }
  }
  
  /**
    * Adds error attachments to the current context. This includes the error trace and environment context.
    * 
    * @param failure the failed status
    */
  def addErrorAttachments(failure: Failed): Unit = { 
    addAttachment("Error details", "txt", failure.error.writeStackTrace())
    addAttachment(s"Environment", "txt", this.scopes.visible.asString)
  }

  def addAttachment(name: String, extension: String, content: String): (String, File) = { 
    state.addAttachment(name, extension, content)
  }

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
      Step(step.keyword, iName, step.status, step.attachments, step.stepDef, iTable, iDocString) tap { iStep =>
        iStep.pos = step.pos
        logger.debug(s"Interpolated ${step.name} to: ${iStep.expression}${if (iTable.nonEmpty) ", () => dataTable" else ""}")
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
              interpolate(Source.fromFile(filepath).mkString)(getBoundReferenceValue)
            } else throw new FileNotFoundException(s"File bound to '$name' not found: $filepath")
          }
        }
        else if (n.startsWith(s"$name/sql")) {
          val selectStmt = interpolate(attScopes.get(s"$name/sql/selectStmt"))(getBoundReferenceValue)
          val dbName = interpolate(attScopes.get(s"$name/sql/dbName"))(getBoundReferenceValue)
          executeSQLQuery(selectStmt, dbName)
        }
        else v
    } getOrElse {
      (topScope.getObject("record") match {
        case Some(scope: ScopedData) =>
          scope.getOpt(name)
        case _ => topScope.getObject("table") match {
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
  
  def compare(sourceName: String, expected: String, actual: String, operator: String, negate: Boolean): Try[Boolean] = Try {
    val res = operator match {
      case "be"      => expected == actual
      case "contain" => actual.contains(expected)
      case "start with" => actual.startsWith(expected)
      case "end with" => actual.endsWith(expected)
      case "match regex" => actual.matches(expected)
      case "match xpath" => !evaluateXPath(expected, actual, XMLNodeType.text).isEmpty
      case "match json path" => !evaluateJsonPath(expected, actual).isEmpty
      case "match template" | "match template file" =>
        matchTemplate(expected, actual, sourceName) match {
          case Success(result) =>
            if (negate) templateMatchError(s"Expected $sourceName to not match template but it did") else result
          case Failure(failure) =>
            if (negate) false else throw failure
        }
    }
    if (!negate) res else !res
  }

  def parseExpression(operator: String, expression: String): String = {
    (if (operator == "match template file") {
      val filepath = interpolate(expression)(getBoundReferenceValue)
      if (new File(filepath).exists()) {
        interpolate(Source.fromFile(filepath).mkString)(getBoundReferenceValue)
      } else throw new FileNotFoundException(s"Template file not found: $filepath")
    } else {
      expression
    }) tap { expr =>
      if (isDryRun && operator.startsWith("match template")) {
        """@\{.*?\}""".r.findAllIn(expr).toList foreach { name =>
          topScope.set(name.substring(2, name.length - 1), "[dryRun:templateExtract]")
        }
      }
    }
  }  
  
}