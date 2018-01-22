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

/**
 * Defines methods for raising various kinds of errors (exceptions).
 */
package gwen {

  import gwen.dsl.{Position, Scenario}
  import java.io.File

  import gherkin.ParserException
  
  package object errors {

    import gwen.dsl.Step
    import gwen.dsl.Tag

    def syntaxError(msg: String) = throw new GherkinSyntaxError(msg, None, None)
    def syntaxError(msg: String, line: Int) = throw new GherkinSyntaxError(msg, Some(line), None)
    def syntaxError(msg: String, line: Int, col: Int) = throw new GherkinSyntaxError(msg, Some(line), Some(col))
    def syntaxError(cause: ParserException) = Option(cause.location) match {
      case Some(loc) =>
        throw new GherkinSyntaxError(cause.getMessage, Some(loc.getLine), Some(loc.getColumn))
      case _ => throw new GherkinSyntaxError(cause.getMessage, None, None)
    }
    def ambiguousCaseError(msg: String) = throw new AmbiguousCaseException(msg)
    def undefinedStepError(step: Step) = throw new UndefinedStepException(step)
    def unboundAttributeError(name: String) = throw new UnboundAttributeException(name, None)
    def unboundAttributeError(name: String, scope: String) = throw new UnboundAttributeException(name, Some(scope))
    def missingPropertyError(name: String) = throw new MissingPropertyException(name)
    def invalidPropertyError(entry: String, propertyFile: File) = throw new InvalidPropertyException(entry, propertyFile)
    def propertyLoadError(name: String, cause: Throwable) = throw new PropertyLoadException(name, cause)
    def propertyLoadError(name: String, cause: String) = throw new PropertyLoadException(s"$name, cause: $cause", null)
    def invalidTagError(msg: String) = throw new InvalidTagException(msg)
    def regexError(msg: String) = throw new RegexException(msg)
    def systemProcessError(msg: String) = throw new SystemProcessException(msg)
    def xPathError(msg: String) = throw new XPathException(msg)
    def jsonPathError(msg: String) = throw new JsonPathException(msg)
    def evaluationError(msg: String) = throw new EvaluationException(msg)
    def invocationError(msg: String) = throw new InvocationException(msg)
    def stepEvaluationError(step: Step, cause: Throwable) = throw new StepEvaluationException(step, cause)
    def recursiveStepDefError(stepDef: Scenario, step: Step) = throw new RecursiveStepDefException(stepDef, step)
    def decodingError(msg: String) = throw new DecodingException(msg)
    def invalidStepDefError(stepDef: Scenario, msg: String) = throw new InvalidStepDefException(stepDef, msg)
    def missingOrInvalidImportFileError(importTag: Tag, specFile: Option[File]) = throw new MissingOrInvalidImportFileException(importTag, specFile)
    def unsupportedImportError(importTag: Tag, specFile: File) = throw new UnsupportedImportException(importTag, specFile)
    def unsupportedDataFileError(dataTag: Tag, specFile: Option[File]) = throw new UnsupportedDataFileException(dataTag, specFile)
    def recursiveImportError(importTag: Tag, specFile: File) = throw new RecursiveImportException(importTag, specFile)
    def sqlError(msg: String) = throw new SQLException(msg)
    def dataTableError(msg: String) = throw new DataTableException(msg)
    def scriptError(language: String, script: String, cause: Throwable) = throw new ScriptException(language, script, cause)
    def javaScriptError(javascript: String, cause: Throwable) = throw new ScriptException("JavaScript", javascript, cause)

    /** Base exception\. */
    class GwenException (msg: String, cause: Throwable = null) extends RuntimeException(msg, cause)

    /** Signals a step that failed to execute. */
    class StepFailure(step: Step, cause: Throwable) extends GwenException(s"Failed step [at line ${step.pos.line}]: $step: ${cause.getMessage}", cause)

    /** Thrown when a Gherkin parsing error occurs. */
    class GherkinSyntaxError(msg: String, line: Option[Int], col: Option[Int]) extends GwenException(s"Gherkin syntax error${line.map(l => s" at line $l").getOrElse("")}${col.map(c => s" col $c").getOrElse("")}: $msg")

    /** Thrown when an ambiguous condition is detected. */
    class AmbiguousCaseException(msg: String) extends GwenException(msg)

    /** Thrown when an unsupported or undefined step is encountered. */
    class UndefinedStepException(step: Step) extends GwenException(s"Unsupported or undefined step: $step")

    /** Thrown when an attribute cannot be found in a scope. */
    class UnboundAttributeException(name: String, scope: Option[String]) extends GwenException(s"Unbound reference${scope.map(x => s" in $x scope")getOrElse ""}: $name")
    
    /** Thrown when a property setting is not found. */
    class MissingPropertyException(name: String) extends GwenException(s"Property not found: $name")
    
    /** Thrown when a property file setting is invalid. */
    class InvalidPropertyException(entry: String, propertyFile: File) extends GwenException(s"Invalid property entry '$entry' found in file: $propertyFile (name=value expected)")

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
    class StepEvaluationException(step: Step, val cause: Throwable) extends GwenException(s"Failed step [at line ${step.pos.line}]: $step: ${cause.getMessage}", cause)
    
    /** Signals an infinite recursive StepDef. */
    class RecursiveStepDefException(stepDef: Scenario, step: Step) extends GwenException(s"StepDef ${stepDef.name} is infinitely recursive at [line ${step.pos.line}]: $step")

    /** Thrown when a decoding error occurs. */
    class DecodingException(msg: String) extends GwenException(msg)
    
    /** Thrown when an invalid StepDef is detected. */
    class InvalidStepDefException(stepDef: Scenario, msg: String) extends GwenException(s"Invalid StepDef: $stepDef: $msg")
    
    /** Thrown when an import file is not found. */
    class MissingOrInvalidImportFileException(importTag: Tag, specFile: Option[File]) extends GwenException(s"Missing or invalid file detected in $importTag${specFile.map(f => s" declared in $f").getOrElse("")}")

    /** Thrown when an unsupported import file is detected. */
    class UnsupportedImportException(importTag: Tag, specFile: File) extends GwenException(s"Unsupported file type detected in $importTag declared in $specFile (only .meta files can be imported)")

    /** Thrown when an unsupported data table file is detected. */
    class UnsupportedDataFileException(dataTag: Tag, specFile: Option[File]) extends GwenException(s"Unsupported file type detected in $dataTag${specFile.map(f => s" declared in $f").getOrElse("")}: only .csv data files supported")
    
    /** Thrown when a recursive import is detected. */
    class RecursiveImportException(importTag: Tag, specFile: File) extends GwenException(s"Recursive (cyclic) $importTag detected in $specFile") {
      override def fillInStackTrace(): RecursiveImportException = this
    }
    
    /** Thrown when an SQL error is detected. */
    class SQLException(msg: String) extends GwenException(msg)

    /** Thrown when a data table error is detected. */
    class DataTableException(msg: String) extends GwenException(msg)

    /** Thrown when a script evaluation error is detected. */
    class ScriptException(language: String, script: String, cause: Throwable) extends GwenException(s"Failed to execute $language: ${if (language == "JavaScript" && script.startsWith("return ")) script.substring(7) else script}", cause)

  }
}