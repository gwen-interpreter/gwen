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

/**
 * Defines methods for r
 * aising various kinds of errors (exceptions).
 */
package gwen.core

import gwen.core.behaviour.BehaviourType
import gwen.core.node.SourceRef
import gwen.core.node.gherkin._

import io.cucumber.gherkin.ParserException

import java.io.File
import gwen.core.eval.binding.BindingType

object Errors {

  def syntaxError(msg: String) = throw new GherkinSyntaxError(msg, None, None, None)
  def syntaxError(msg: String, line: Long) = throw new GherkinSyntaxError(msg, None, Some(line), None)
  def syntaxError(msg: String, file: Option[File], line: Long, col: Long) = throw new GherkinSyntaxError(msg, file, Some(line), Some(col))
  def syntaxError(file: File, cause: ParserException) = Option(cause.location) match {
    case Some(loc) =>
      throw new GherkinSyntaxError(cause.getMessage, Some(file), Some(loc.getLine), Some(loc.getColumn))
    case _ => throw new GherkinSyntaxError(cause.getMessage, Some(file), None, None)
  }
  def ambiguousCaseError(msg: String) = throw new AmbiguousCaseException(msg)
  def undefinedStepError(step: Step) = throw new UndefinedStepException(step)
  def illegalStepError(msg: String) = throw new IllegalStepException(msg)
  def disabledStepError(step: Step) = throw new DisabledStepException(step)
  def unboundAttributeError(name: String) = throw new UnboundAttributeException(name, None)
  def unboundAttributeError(name: String, scope: String) = throw new UnboundAttributeException(name, Some(scope))
  def missingSettingError(name: String) = throw new MissingSettingException(name)
  def unsupportedMaskedPropertyError(msg: String) = throw new UnsupportedMaskedPropertyException(msg)
  def invalidPropertyError(entry: String, propertyFile: File) = throw new InvalidPropertyException(entry, propertyFile)
  def illegalSettingError(name: String, value: String, validValues: String) = throw new IllegalSettingException(name, value, validValues)
  def propertyLoadError(name: String, cause: Throwable) = throw new PropertyLoadException(name, cause)
  def propertyLoadError(name: String, cause: String) = throw new PropertyLoadException(s"$name, cause: $cause", null)
  def licenseError(msg: String) = throw new LicenseException(msg)
  def invalidTagError(msg: String) = throw new InvalidTagException(msg)
  def regexError(msg: String) = throw new RegexException(msg)
  def systemProcessError(msg: String) = throw new SystemProcessException(msg)
  def xPathError(msg: String) = throw new XPathException(msg)
  def jsonPathError(msg: String) = throw new JsonPathException(msg)
  def evaluationError(msg: String) = throw new EvaluationException(msg)
  def invocationError(msg: String) = throw new InvocationException(msg)
  def stepEvaluationError(step: Step, cause: Throwable) = throw new StepEvaluationException(step, cause)
  def recursiveStepDefError(stepDef: Scenario) = throw new RecursiveStepDefException(stepDef)
  def decodingError(msg: String) = throw new DecodingException(msg)
  def invalidStepDefError(stepDef: Scenario, msg: String) = throw new InvalidStepDefException(stepDef, msg)
  def missingOrInvalidImportFileError(importTag: Tag) = throw new MissingOrInvalidImportFileException(importTag)
  def unsupportedImportError(importTag: Tag) = throw new UnsupportedImportException(importTag)
  def unsupportedDataFileError(dataTag: Tag) = throw new UnsupportedDataFileException(dataTag)
  def recursiveImportError(importTag: Tag) = throw new RecursiveImportException(importTag)
  def sqlError(msg: String) = throw new SQLException(msg)
  def dataTableError(msg: String) = throw new DataTableException(msg)
  def scriptError(language: String, script: String, cause: Throwable) = throw new ScriptException(language, script, cause)
  def javaScriptError(javascript: String, cause: Throwable) = throw new ScriptException("JavaScript", javascript, cause)
  def templateMatchError(msg: String) = throw new TemplateMatchException(msg)
  def unsupportedLocalSetting(name: String) = throw new UnsupportedLocalSettingException(name)
  def invalidSettingError(name: String, value: String, msg: String) = throw new InvalidSettingException(name, value, msg)
  def imperativeStepError(step: Step) = throw new ImperativeStepException(step)
  def imperativeStepDefError(stepDef: Scenario) = throw new ImperativeStepDefException(stepDef)
  def improperBehaviourError(node: GherkinNode) = throw new ImproperBehaviourException(node)
  def unexpectedBehaviourError(step: Step, expected: BehaviourType, actual: BehaviourType) = throw new UnexpectedBehaviourException(step, expected, actual)
  def undefinedStepDefBehaviourError(stepDef: Scenario) = throw new UndefinedStepDefBehaviourException(stepDef)
  def keywordDialectError(language: String, keyword: String) = throw new KeywordDialectException(language, keyword)
  def metaDialectError(language: String, specFile: File) = throw new MetaDialectException(language, specFile)
  def fileAttachError(file: File, msg: String) = throw new FileAttachException(file, msg)
  def serviceHealthCheckError(msg: String, cause: Throwable = null) = throw new ServiceHealthCheckException(msg, cause)
  def multilineParamError(msg: String) = throw new MultilineParamException(msg)
  def stepError(step: Step, cause: Throwable) = throw new StepFailure(step, cause)
  def waitTimeoutError(timeoutSecs: Long, reason: String, cause: Throwable = null) = throw new WaitTimeoutException(timeoutSecs, reason, cause)
  def invalidBindingPathTypeError(bindingType: BindingType) = throw new InvalidBindingPathTypeException(bindingType)
  def deprecatedError(msg: String) = throw new DeprecatedException(msg)
  def initProjectError(msg: String) = throw new InitProjectException(msg)

  private def at(sourceRef: Option[SourceRef]): String = at(sourceRef.map(_.toString).getOrElse(""))
  private def at(file: Option[File], line: Option[Long], column: Option[Long]): String = at(SourceRef.toString(file, line, column))
  private def at(location: String): String = if (location.length > 0) s" [at $location]" else ""
  
  /** Base exception\. */
  class GwenException (msg: String, cause: Throwable = null) extends RuntimeException(msg, cause)

  /** Signals a step that failed to execute. */
  class StepFailure(step: Step, cause: Throwable) extends GwenException(s"Failed step${at(step.sourceRef)}: $step: ${cause.getMessage}", cause)

  /** Thrown when a Gherkin parsing error occurs. */
  class GherkinSyntaxError(msg: String, file: Option[File], line: Option[Long], col: Option[Long]) extends GwenException(s"Gherkin syntax error${at(file, line, col)}: $msg")

  /** Thrown when an ambiguous condition is detected. */
  class AmbiguousCaseException(msg: String) extends GwenException(msg)

  /** Thrown when an unsupported or undefined step is encountered. */
  class UndefinedStepException(step: Step) extends GwenException(s"Unsupported or undefined step: $step")

  /** Thrown when an illegal step is detected. */
  class IllegalStepException(msg: String) extends GwenException(msg)

  /** Thrown when a step is disabled. */
  class DisabledStepException(step: Step) extends GwenException(s"Disabled step: $step")

  /** Thrown when an attribute cannot be found in a scope. */
  class UnboundAttributeException(name: String, scope: Option[String]) extends GwenException(s"Unbound reference${scope.map(x => s" in $x scope")getOrElse ""}: $name")
  
  /** Thrown when a setting is not found. */
  class MissingSettingException(name: String) extends GwenException(s"Setting not found: $name")

  /** Thrown when a property setting that does not support masking is masked. */
  class UnsupportedMaskedPropertyException(msg: String) extends GwenException(msg)

  /** Thrown when a an operation requires a license that could not be resolved. */
  class LicenseException(msg: String) extends GwenException(msg)
  
  /** Thrown when a property file setting is invalid. */
  class InvalidPropertyException(entry: String, propertyFile: File) extends GwenException(s"Invalid property entry '$entry' found in file: $propertyFile (name=value expected)")

  /** Thrown when a property file setting contains an invalid or unspported value. */
  class IllegalSettingException(name: String, value: String, validValues: String) extends GwenException(s"Invalid or illegal setting: $name = $value (valid values include: $validValues)")
  
  /** Thrown when a property setting fails to load. */
  class PropertyLoadException(name: String, cause: Throwable) extends GwenException(s"Failed to load property setting: $name", cause)

  /** Thrown when an invalid tag (annotation) is detected. */
  class InvalidTagException(tagString: String) extends GwenException(s"Invalid tag: $tagString")

  /** Thrown when a regex error occurs. */
  class RegexException(msg: String) extends GwenException(msg)

  /** Thrown when a system process fails. */
  class SystemProcessException(msg: String) extends GwenException(msg)

  /** Thrown when a xpath evaluation fails. */
  class XPathException(msg: String) extends GwenException(msg)
  
  /** Thrown when a JSON path evaluation fails. */
  class JsonPathException(msg: String) extends GwenException(msg)

  /** Throw when any evaluation error occurs in the interpreter. */
  class EvaluationException(msg: String) extends GwenException(msg)
  
  /** Throw when there is an error in invoking gwen. */
  class InvocationException(msg: String) extends GwenException(msg)
  
  /** Signals a step that failed to evaluate. */
  class StepEvaluationException(step: Step, val cause: Throwable) extends GwenException(s"Failed step${at(step.sourceRef)}: $step: ${cause.getMessage}", cause)
  
  /** Signals an infinite recursive StepDef. */
  class RecursiveStepDefException(stepDef: Scenario) extends GwenException(s"Illegal recursive call to StepDef${at(stepDef.sourceRef)}")

  /** Thrown when a decoding error occurs. */
  class DecodingException(msg: String) extends GwenException(msg)
  
  /** Thrown when an invalid StepDef is detected. */
  class InvalidStepDefException(stepDef: Scenario, msg: String) extends GwenException(s"Invalid StepDef: $stepDef: $msg")
  
  /** Thrown when an import file is not found. */
  class MissingOrInvalidImportFileException(importTag: Tag) extends GwenException(s"Missing or invalid file detected in $importTag${at(importTag.sourceRef)}")

  /** Thrown when an unsupported import file is detected. */
  class UnsupportedImportException(importTag: Tag) extends GwenException(s"Unsupported file type detected in $importTag${at(importTag.sourceRef)} (only .meta files can be imported)")

  /** Thrown when an unsupported data table file is detected. */
  class UnsupportedDataFileException(dataTag: Tag) extends GwenException(s"Unsupported file type detected in $dataTag${at(dataTag.sourceRef)}: only .csv data files supported")
  
  /** Thrown when a recursive import is detected. */
  class RecursiveImportException(importTag: Tag) extends GwenException(s"Recursive (cyclic) $importTag detected${at(importTag.sourceRef)}") {
    override def fillInStackTrace(): RecursiveImportException = this
  }
  
  /** Thrown when an SQL error is detected. */
  class SQLException(msg: String) extends GwenException(msg)

  /** Thrown when a data table error is detected. */
  class DataTableException(msg: String) extends GwenException(msg)

  /** Thrown when a script evaluation error is detected. */
  class ScriptException(language: String, script: String, cause: Throwable) extends GwenException(s"Failed to execute $language: ${if (language == "JavaScript" && script.startsWith("return ")) script.substring(7) else script}", cause)

  /** Thrown when a template match error is detected. */
  class TemplateMatchException(msg: String) extends GwenException(msg)

  /** Thrown when an attempt is made to set a non Gwen local setting. */
  class UnsupportedLocalSettingException(name: String) extends GwenException(s"Unsupported thread-local setting '$name': only Gwen settings that start with 'gwen.' are supported here")

  /** Thrown when an invalid setting is provided. */
  class InvalidSettingException(name: String, value: String, msg: String) extends GwenException(s"Invalid setting $name=$value: $msg")

  /** Thrown when an imperative step is detected in a feature when declarative mode is enabled. */
  class ImperativeStepException(step: Step) extends GwenException(s"Declarative feature violation: DSL step not permitted in feature${at(step.sourceRef)}: $step")

  /** Thrown when an imperative step defenition is detected in a feature when declarative mode is enabled. */
  class ImperativeStepDefException(stepDef: Scenario) extends GwenException(s"Declarative feature violation: StepDef declaration not permitted in feature${at(stepDef.sourceRef)}")

  /** Thrown in strict rules mode when Given-When-Then order is not satisfied in a scenario or background */
  class ImproperBehaviourException(node: GherkinNode) 
    extends GwenException(s"Strict behaviour violation: Given-When-Then order not satisfied by steps in ${node.nodeType.toString}${at(node.sourceRef)}")

  /** Thrown in strict rules mode when a step' behaviour type does not match its Given, When, or Then position. */
  class UnexpectedBehaviourException(step: Step, expected: BehaviourType, actual: BehaviourType) 
    extends GwenException(s"Strict behaviour violation: $actual behaviour not permitted where ${expected.toString.toLowerCase} is expected (StepDef has @$actual tag${at(step.stepDef.flatMap(_.behaviourTag.flatMap(_.sourceRef)))})")

  /** Thrown in strict rules mode when a step def does not declare a Given, When or Then tag. */
  class UndefinedStepDefBehaviourException(stepDef: Scenario) 
    extends GwenException(s"Strict behaviour violation: Missing @Context, @Action, or @Assertion behaviour annotation on StepDef${at(stepDef.sourceRef)}")

  /** Thrown when a keyword is unknown for a given language dialect. */
  class KeywordDialectException(language: String, keyword: String) extends GwenException(s"Unsupported or unknown keyword: $keyword (language=$language)")

  /** Thrown when an explicit dialiect directive is found in a meta spec. */
  class MetaDialectException(language: String, specFile: File) extends GwenException(s"Language '$language' not supported in meta file at: $specFile (English 'en' supported for meta only)")

  /** Thrown when a file cannot be attached. */
  class FileAttachException(file: File, msg: String) extends GwenException(s"Failed to attach file $file: $msg")

  /** Thrown when a service health check fails. */
  class ServiceHealthCheckException(msg: String, cause: Throwable) extends GwenException(msg, cause)

  /** Thrown when a multiline parameters is substributed into an invalid position in a step expression. */
  class MultilineParamException(msg: String) extends GwenException(msg)

  /** Thrown when a timeout error occurs. */
  class WaitTimeoutException(timeoutSecs: Long, reason: String, cause: Throwable) extends GwenException(s"Timed out after $timeoutSecs second(s) $reason", cause)

  /** Thrown when an invalid binding type is detected in a comparison operation. */
  class InvalidBindingPathTypeException(bindingType: BindingType) extends GwenException(s"Invalid path binding type: $bindingType (only ${BindingType.xpath} or ${BindingType.`json path`} supported)")

  /** Signals usage of a deprecated feature that is no longer supported. */
  class DeprecatedException(msg: String) extends GwenException(msg)

  /** Throw when there is an error tryig to initialise a Gwen project directory. */
  class InitProjectException(msg: String) extends GwenException(msg)
}
