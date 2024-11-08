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

/**
 * Defines methods for r
 * aising various kinds of errors (exceptions).
 */
package gwen.core

import gwen.core.behavior.BehaviorType
import gwen.core.eval.binding.BindingType
import gwen.core.node.SourceRef
import gwen.core.node.gherkin._

import io.cucumber.messages.types.ParseError

import java.io.File

import scala.jdk.OptionConverters._

import com.typesafe.scalalogging.LazyLogging

object Errors extends LazyLogging {

  def parseError(parseError: ParseError) = throw new ParserException(parseError)
  def syntaxError(msg: String) = throw new SyntaxError(msg, None, None, None)
  def syntaxError(msg: String, line: Long) = throw new SyntaxError(msg, None, Some(line), None)
  def syntaxError(msg: String, file: Option[File], line: Long, col: Option[Long]) = throw new SyntaxError(msg, file, Some(line), col)
  def syntaxError(file: File, cause: ParseError) = cause.getSource.getLocation.toScala match {
    case Some(loc) =>
      throw new SyntaxError(cause.getMessage, Some(file), Some(loc.getLine), loc.getColumn.toScala.map(_.toLong))
    case _ => throw new SyntaxError(cause.getMessage, Some(file), None, None)
  }
  def ambiguousCaseError(msg: String) = throw new AmbiguousCaseException(msg)
  def undefinedStepError(step: Step) = throw new UndefinedStepException(step)
  def illegalStepError(msg: String) = throw new IllegalStepException(msg)
  def illegalConditionError(condition: String) = throw new IllegalConditionException(condition)
  def disabledStepError(step: Step) = throw new DisabledStepException(step)
  def unboundAttributeError(name: String) = throw new UnboundReferenceException(name, None, None)
  def unboundAttributeError(name: String, cause: Throwable) = throw new UnboundReferenceException(name, None, Some(cause))
  def unboundAttributeError(name: String, scope: String) = throw new UnboundReferenceException(name, Some(scope), None)
  def unboundAttributeError(name: String, scope: Option[String], cause: Option[Throwable]) = throw new UnboundReferenceException(name, scope, cause)
  def unboundBooleanAttributeError(name: String, value: String) = throw new UnboundBooleanReferenceException(name, value)
  def missingSettingError(name: String) = throw new MissingSettingException(name)
  def settingsNotFound(files: List[File]) = throw new SettingsNotFoundException(files)
  def unsupportedMaskedPropertyError(msg: String) = throw new UnsupportedMaskedPropertyException(msg)
  def invalidPropertyError(entry: String, propertyFile: File) = throw new InvalidPropertyException(entry, propertyFile)
  def illegalSettingError(name: String, value: String, validValues: String) = throw new IllegalSettingException(name, value, validValues)
  def illegalSettingAttributeError(name: String, parent: String, validNames: String) = throw new IllegalSettingAttributeException(name, parent, validNames)
  def illegalStepAnnotationError(sourceRef: Option[SourceRef], msg: String) = throw new IllegalStepAnnotationException(sourceRef, msg)
  def propertyLoadError(name: String, cause: Throwable) = throw new PropertyLoadException(name, cause)
  def propertyLoadError(name: String, cause: String) = throw new PropertyLoadException(s"$name, cause: $cause", null)
  def licenseError(msg: String) = throw new LicenseException(msg)
  def invalidTagError(msg: String) = throw new InvalidTagException(msg)
  def invalidAnnotationError(sourceRef: Option[SourceRef], msg: String) = throw new InvalidAnnotationException(sourceRef, msg)
  def regexError(msg: String) = throw new RegexException(msg)
  def systemProcessError(msg: String, cause: Throwable) = throw new SystemProcessException(msg, cause)
  def xPathError(msg: String) = throw new XPathException(msg)
  def jsonPathError(msg: String) = throw new JsonPathException(msg)
  def evaluationError(msg: String) = throw new EvaluationException(msg)
  def invocationError(msg: String) = throw new InvocationException(msg)
  def stepEvaluationError(step: Step, cause: Throwable) = throw new StepEvaluationException(step, cause)
  def recursiveStepDefError(stepDef: Scenario) = throw new RecursiveStepDefException(stepDef)
  def decodingError(msg: String) = throw new DecodingException(msg)
  def invalidStepDefError(stepDef: Scenario, msg: String) = throw new InvalidStepDefException(stepDef, msg)
  def missingOrInvalidImportFileError(importAnnotation: Tag) = throw new MissingOrInvalidImportFileException(importAnnotation)
  def missingFileError(category: String, file: File) = throw new MissingFileException(category, file)
  def unsupportedImportError(importAnnotation: Tag) = throw new UnsupportedImportException(importAnnotation)
  def dataLookupError(file: File, name: String) = throw new DataLookupException(file, name)
  def dataHeaderNotFoundError(file: File) = throw new DataHeaderNotFoundException(file)
  def multilineDataFieldNameError(name: String, file: Option[File]) = throw new MultilineDataFieldNameException(name, file)
  def recursiveImportError(importAnnotation: Tag) = throw new RecursiveImportException(importAnnotation)
  def sqlError(msg: String) = throw new SQLException(msg)
  def dataTableError(msg: String) = throw new DataTableException(msg)
  def functionError(func: String, cause: Throwable) = throw new FunctionException(func, cause.getMessageLine1, Some(cause))
  def functionError(func: String, msg: String) = throw new FunctionException(func, msg, None)
  def templateMatchError(msg: String) = throw new TemplateMatchException(msg)
  def unsupportedLocalSetting(name: String) = throw new UnsupportedLocalSettingException(name)
  def invalidSettingError(name: String, value: String, msg: String) = throw new InvalidSettingException(name, value, msg)
  def invalidTypeError(msg: String) = throw new InvalidTypeException(msg)
  def imperativeStepError(step: Step) = throw new ImperativeStepException(step)
  def imperativeStepDefError(stepDef: Scenario) = throw new ImperativeStepDefException(stepDef)
  def improperBehaviorError(node: GherkinNode) = throw new ImproperBehaviorException(node)
  def unexpectedBehaviorError(step: Step, expected: BehaviorType, actual: BehaviorType) = throw new UnexpectedBehaviorException(step, expected, actual)
  def undefinedStepDefBehaviorError(stepDef: Scenario) = throw new UndefinedStepDefBehaviorException(stepDef)
  def keywordDialectError(language: String, keyword: String) = throw new KeywordDialectException(language, keyword)
  def metaDialectError(language: String, specFile: File) = throw new MetaDialectException(language, specFile)
  def fileAttachError(file: File, msg: String) = throw new FileAttachException(file, msg)
  def serviceHealthCheckError(msg: String, cause: Throwable = null) = throw new ServiceHealthCheckException(msg, cause)
  def multilineSubstitutionError(msg: String) = throw new MultilineSubstitutionException(msg)
  def stepError(step: Step, cause: Throwable) = throw new StepException(step, cause.getMessage, cause)
  def waitTimeoutError(timeoutSecs: Long, reason: String, cause: Throwable = null) = throw new WaitTimeoutException(timeoutSecs, reason, cause)
  def invalidBindingPathTypeError(bindingType: BindingType) = throw new InvalidBindingPathTypeException(bindingType)
  def deprecatedError(msg: String) = throw new DeprecatedException(msg)
  def initProjectError(msg: String) = throw new InitProjectException(msg)
  def copyResourceError(msg: String) = throw new CopyResourceException(msg)
  def assertionError(msg: String, mode: AssertionMode) = throw new GwenAssertionError(msg, mode)
  def accumulatedAssertionError(error: GwenAssertionError) = throw new AccumulatedAssertionError(error)
  def interruptException(cause: Throwable) = throw new GwenInterruptException(cause)
  def unsupportedFileError(file: File, fileType: String, validExtensions: String) = throw new UnsupportedFileException(file, fileType, validExtensions)
  def unsupportedJsonStructureError(cause: Throwable) = throw new UnsupportedJsonStructureException(cause)
  def missingJSArgumentError(jsRef: String, argIndex: Int) = throw new MissingJSArgumentException(jsRef, argIndex)
  def invalidReferenceOrFunctionError(msg: String) = throw new InvalidReferenceOrFunctionException(msg)
  def illegalNestedParallelExceutionError(sourceRef: Option[SourceRef]) = throw new IllegalNestedParallelExecutionException(sourceRef)
  def malformedDataSourceError(dataFile: File, cause: Throwable) = throw new MalformedDataSourceException(dataFile, cause)
  def illegalDurationAnnotationError(targetName: String, annotation: String) = throw new IllegalDurationAnnotationExcpetion(targetName, annotation)
  def immutableModificationError(name: String, annotation: Annotations) = throw new ImmutableModificationException(name, annotation)

  def at(sourceRef: Option[SourceRef]): String = at(sourceRef.map(_.toString).getOrElse(""))
  private def at(file: Option[File], line: Option[Long], column: Option[Long]): String = at(SourceRef.toString(file, line, column))
  private def at(location: String): String = if (location.length > 0) s" [at $location]" else ""
  
  /** Base exception\. */
  class GwenException (msg: String, cause: Throwable = null) extends RuntimeException(Formatting.stripZeroChar(msg), cause)

  /** Signals a step that failed to execute. */
  class StepException(step: Step, msg: String, cause: Throwable = null) extends GwenException(s"$msg${if(msg.endsWith(at(step.sourceRef))) "" else at(step.sourceRef)}", cause)

  /** Signals a StepDef that failed to execute. */
  class StepDefException(stepDef: Scenario, msg: String, cause: Throwable = null) extends GwenException(s"$msg${at(stepDef.sourceRef)}", cause)

  /** Thrown when a parsing error occurs. */
  class SyntaxError(msg: String, file: Option[File], line: Option[Long], col: Option[Long]) extends GwenException(s"Syntax error${at(file, line, col)}: $msg")

  class ParserException(val parseError: ParseError) extends GwenException(s"Parser errors:\n${parseError.getMessage}")

  /** Thrown when an ambiguous condition is detected. */
  class AmbiguousCaseException(msg: String) extends GwenException(msg)

  /** Thrown when an unsupported or undefined step is encountered. */
  class UndefinedStepException(step: Step) extends StepException(step, "Unsupported or undefined step")

  /** Thrown when an illegal step is detected. */
  class IllegalStepException(msg: String) extends GwenException(msg)

  /** Thrown when an illegal condition is detected. */
  class IllegalConditionException(condition: String) extends GwenException(s"Illegal condition literal: $condition (predicate function or reference expected)")

  /** Thrown when a step is disabled. */
  class DisabledStepException(step: Step) extends StepException(step, "Step is disabled")

  abstract class UnboundAttributeException(val name: String, val scope: Option[String], msg: String, val cause: Option[Throwable]) extends GwenException(msg, cause.orNull)

  /** Thrown when an attribute cannot be found in a scope. */
  class UnboundReferenceException(override val name: String, scope: Option[String], cause: Option[Throwable]) extends UnboundAttributeException(name, scope, s"Unbound reference${scope.map(x => s" in $x scope")getOrElse ""}: $name", cause)

  /** Thrown when a boolean attribute is invalid or unbound. */
  class UnboundBooleanReferenceException(name: String, value: String) extends UnboundAttributeException(name, None, s"Boolean literal or evaluation expected but found '$value' in reference: $name", None)

  /** Thrown when a setting is not found. */
  class MissingSettingException(name: String) extends GwenException(s"Setting not found: $name")

  /** Thrown when one or more settings files are missing. */
  class SettingsNotFoundException(files: List[File]) extends GwenException(s"Settings file${if (files.size > 1) "s" else ""} not found: ${files.mkString(", ")}")

  /** Thrown when a property setting that does not support masking is masked. */
  class UnsupportedMaskedPropertyException(msg: String) extends GwenException(msg)

  /** Thrown when a an operation requires a license that could not be resolved. */
  class LicenseException(msg: String) extends GwenException(msg)
  
  /** Thrown when a property file setting is invalid. */
  class InvalidPropertyException(entry: String, propertyFile: File) extends GwenException(s"Invalid setting entry '$entry' found in file: $propertyFile (name=value expected)")

  /** Thrown when a property file setting contains an invalid or unspported value. */
  class IllegalSettingException(name: String, value: String, validValues: String) extends GwenException(s"Invalid or illegal setting: $name = $value (valid values include: $validValues)")

  /** Thrown when a property file setting contains an invalid attirbute name.. */
  class IllegalSettingAttributeException(name: String, parent: String, validNames: String) extends GwenException(s"Invalid or illegal $parent setting attribute: $name (valid attributes include: $validNames)")

  /** Thrown when an annotation is found in an illegal step position. */
  class IllegalStepAnnotationException(sourceRef: Option[SourceRef], msg: String) extends GwenException(s"Invalid or illegal step annotation${at(sourceRef)}: $msg")
  
  /** Thrown when a property setting fails to load. */
  class PropertyLoadException(name: String, cause: Throwable) extends GwenException(s"Failed to load setting: $name", cause)

  /** Thrown when an invalid tag is detected. */
  class InvalidTagException(annotation: String) extends GwenException(s"Invalid annotation: $annotation")

  /** Thrown when an invalid annotation is detected. */
  class InvalidAnnotationException(sourceRef: Option[SourceRef], msg: String) extends GwenException(s"Invalid or illegal annotation${at(sourceRef)}: $msg")

  /** Thrown when a regex error occurs. */
  class RegexException(msg: String) extends GwenException(msg)

  /** Thrown when a system process fails. */
  class SystemProcessException(msg: String, cause: Throwable) extends GwenException(s"$msg: ${cause.getMessage}", cause)

  /** Thrown when a xpath evaluation fails. */
  class XPathException(msg: String) extends GwenException(msg)
  
  /** Thrown when a JSON path evaluation fails. */
  class JsonPathException(msg: String) extends GwenException(msg)

  /** Throw when any evaluation error occurs in the interpreter. */
  class EvaluationException(msg: String) extends GwenException(msg)
  
  /** Throw when there is an error in invoking gwen. */
  class InvocationException(msg: String) extends GwenException(msg)
  
  /** Signals a step that failed to evaluate. */
  class StepEvaluationException(step: Step, val cause: Throwable) extends StepException(step, cause.getMessage, cause)
  
  /** Signals an infinite recursive StepDef. */
  class RecursiveStepDefException(stepDef: Scenario) extends StepDefException(stepDef, s"Illegal recursive call to StepDef")

  /** Thrown when a decoding error occurs. */
  class DecodingException(msg: String) extends GwenException(msg)
  
  /** Thrown when an invalid StepDef is detected. */
  class InvalidStepDefException(stepDef: Scenario, msg: String) extends StepDefException(stepDef, s"Invalid StepDef: $msg")
  
  /** Thrown when an expected file is missing. */
  class MissingFileException(category: String, file: File) extends GwenException(s"$category not found: $file")

  /** Thrown when an import file is not found. */
  class MissingOrInvalidImportFileException(importAnnotation: Tag) extends GwenException(s"Missing or invalid file detected in $importAnnotation annotation${at(importAnnotation.sourceRef)}")

  /** Thrown when an unsupported import file is detected. */
  class UnsupportedImportException(importAnnotation: Tag) extends GwenException(s"Unsupported file type detected in $importAnnotation annotation${at(importAnnotation.sourceRef)} (only .meta files can be imported)")

  /** Thrown when a data lookup fails. */
  class DataLookupException(file: File, name: String) extends GwenException(s"No such data in file $file having name: $name")

  /** Thrown when a data file doesn't have a header record. */
  class DataHeaderNotFoundException(file: File) extends GwenException(s"Header not found in data file: $file)")

  /** Thrown when a data field name spans multiple lines. */
  class MultilineDataFieldNameException(name: String, file: Option[File]) extends GwenException(s"Illegal mutiline data field name${file.map(f => s" found in data file $f").getOrElse("")}: $name)")
  
  /** Thrown when a recursive import is detected. */
  class RecursiveImportException(importAnnotation: Tag) extends GwenException(s"Recursive (cyclic) $importAnnotation detected${at(importAnnotation.sourceRef)}") {
    override def fillInStackTrace(): RecursiveImportException = this
  }
  
  /** Thrown when an SQL error is detected. */
  class SQLException(msg: String) extends GwenException(msg)

  /** Thrown when a data table error is detected. */
  class DataTableException(msg: String) extends GwenException(msg)

  /** Thrown when a JavaScript function error is detected. */
  class FunctionException(func: String, msg: String, cause: Option[Throwable]) extends GwenException(s"$msg: $func", cause.orNull)

  /** Thrown when a template match error is detected. */
  class TemplateMatchException(msg: String) extends GwenException(msg)

  /** Thrown when an attempt is made to set a non Gwen local setting. */
  class UnsupportedLocalSettingException(name: String) extends GwenException(s"Unsupported thread-local setting '$name': only Gwen settings that start with 'gwen.' are supported here")

  /** Thrown when an invalid setting is provided. */
  class InvalidSettingException(name: String, value: String, msg: String) extends GwenException(s"Invalid setting $name=$value: $msg")

  /** Thrown when an invalid type conversion is detected. */
  class InvalidTypeException(msg: String) extends GwenException(msg)

  /** Thrown when an imperative step is detected in a feature when declarative mode is enabled. */
  class ImperativeStepException(step: Step) extends StepException(step, "Declarative feature mode violation: DSL step not permitted in feature (set your gwen.feature.mode setting to imperative to disable this check)")

  /** Thrown when an imperative step defenition is detected in a feature when declarative mode is enabled. */
  class ImperativeStepDefException(stepDef: Scenario) extends StepDefException(stepDef, "Declarative feature mode violation: StepDef not permitted in feature (set your gwen.feature.mode setting to imperative to disable this check)")

  /** Thrown in strict rules mode when Given-When-Then order is not satisfied in a scenario or background */
  class ImproperBehaviorException(node: GherkinNode) 
    extends GwenException(s"Strict behavior rules violation: Given-When-Then order not satisfied by steps in ${node.nodeType.toString}${at(node.sourceRef)} (set your gwen.behavior.rules setting to lenient to disable this check)")

  /** Thrown in strict rules mode when a step' behavior type does not match its Given, When, or Then position. */
  class UnexpectedBehaviorException(step: Step, expected: BehaviorType, actual: BehaviorType) 
    extends StepException(step, s"Strict behavior rules violation: $actual behavior not permitted where ${expected.toString.toLowerCase} is expected. StepDef has @$actual annotation${at(step.stepDef.flatMap(_.behaviorTag.flatMap(_.sourceRef)))}. (set your gwen.behavior.rules setting to lenient to disable this check)")

  /** Thrown in strict rules mode when a step def does not declare a Given, When or Then tag. */
  class UndefinedStepDefBehaviorException(stepDef: Scenario) 
    extends StepDefException(stepDef, s"Strict behavior rules violation: Missing @Context, @Action, or @Assertion annotation on StepDef (set your gwen.behavior.rules setting to lenient to disable this check)")

  /** Thrown when a keyword is unknown for a given language dialect. */
  class KeywordDialectException(language: String, keyword: String) extends GwenException(s"Unsupported or unknown keyword: $keyword (language=$language)")

  /** Thrown when an explicit dialiect directive is found in a meta spec. */
  class MetaDialectException(language: String, specFile: File) extends GwenException(s"Language '$language' not supported in meta file at: $specFile (English 'en' supported for meta only)")

  /** Thrown when a file cannot be attached. */
  class FileAttachException(file: File, msg: String) extends GwenException(s"Failed to attach file $file: $msg")

  /** Thrown when a service health check fails. */
  class ServiceHealthCheckException(msg: String, cause: Throwable) extends GwenException(msg, cause)

  /** Thrown when a multiline parameter or property is substituted into an invalid position in a step expression. */
  class MultilineSubstitutionException(msg: String) extends GwenException(msg)

  /** Thrown when a timeout error occurs. */
  class WaitTimeoutException(timeoutSecs: Long, reason: String, cause: Throwable) extends GwenException(s"Timed out after $timeoutSecs second(s) $reason", cause)

  /** Thrown when an invalid binding type is detected in a comparison operation. */
  class InvalidBindingPathTypeException(bindingType: BindingType) extends GwenException(s"Invalid path binding type: $bindingType (only ${BindingType.xpath} or ${BindingType.`json path`} supported)")

  /** Signals usage of a deprecated feature that is no longer supported. */
  class DeprecatedException(msg: String) extends GwenException(msg)

  /** Throw when there is an error tryig to initialise a Gwen project directory. */
  class InitProjectException(msg: String) extends GwenException(msg)

  /** Throw when there is an error tryig to copy a resource. */
  class CopyResourceException(msg: String) extends GwenException(msg)

  /** Thrown when an assertion fails. */
  class GwenAssertionError(val msg: String, val mode: AssertionMode) extends AssertionError(Formatting.stripZeroChar(msg))

  /** Thrown when an accumulated assertion error is raised. */
  class AccumulatedAssertionError(error: GwenAssertionError) extends GwenAssertionError(error.msg, error.mode)

  /** Throw when there is a user interrupt error (usually due to cntl-c being pressed). */
  class GwenInterruptException(cause: Throwable) extends GwenException(s"Gwen interrupted", cause)

  /** Thrown when a file is not supported. */
  class UnsupportedFileException(file: File, fileType: String, validExtensions: String) extends GwenException(s"Unsupported $fileType file ($validExtensions expected): $file")

  /** Thrown when a data file is not supported. */
  class MalformedDataSourceException(dataFile: File, cause: Throwable) extends GwenException(s"Malformed ${dataFile.extension.toUpperCase} data in $dataFile file: ${cause.getMessage}", cause)

  /** Thrown when a non JSON string array is detected. */
  class UnsupportedJsonStructureException(cause: Throwable) extends GwenException(s"Unsupported or inconsistent JSON input data structure", cause)

  /** Thrown when a JS function argument is missing. */
  class MissingJSArgumentException(jsRef: String, argIndex: Int) extends GwenException(s"arguments[$argIndex] placeholder expected (for parameter ${argIndex + 1}) but not defined in JS function binding: $jsRef")

  /** Thrown when an unbound refrence or invalid function is detected. */
  class InvalidReferenceOrFunctionException(msg: String) extends GwenException(msg)

  /** Thrown when a nested parallel execution is detected. */
  class IllegalNestedParallelExecutionException(sourceRef: Option[SourceRef]) extends GwenException(s"Illegal nested parallel executon at${at(sourceRef)}")

  /** Thrown when an illegal duration pattern is detected in a Wait or Delay annotation. */
  class IllegalDurationAnnotationExcpetion(targetName: String, annotation: String) extends GwenException(s"Illegal or malformed $annotation annotation. Valid examples include: @${targetName}('10s'), @${targetName}('1m30s'), @${targetName}('2m')")

  /** Thrown when an invalid CSV result field or file reference detected. */
  class CSVResultsReferenceException(errors: List[String]) extends GwenException(errors.map(err => s" - $err").mkString("\n"))

  /** Thrown when an attempt to mutate a constant binding is detected. */
  class ImmutableModificationException(name: String, annotation: Annotations) extends GwenException(s"Cannot modify read only ${annotation.toString.toLowerCase}: ${name.takeWhile(_ != '/')}")

}
