/*
 * Copyright 2014-2015 Branko Juric, Brady Wood
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

  import gwen.dsl.Scenario
  import java.io.File
  
  package object errors {

    import gwen.dsl.Step

    def parsingError(msg: String) = throw new ParsingException(msg)
    def ambiguousCaseError(msg: String) = throw new AmbiguousCaseException(msg)
    def undefinedStepError(step: Step) = throw new UndefinedStepException(step)
    def unboundAttributeError(name: String) = throw new UnboundAttributeException(name, None)
    def unboundAttributeError(name: String, scope: String) = throw new UnboundAttributeException(name, Some(scope))
    def missingPropertyError(name: String) = throw new MissingPropertyException(name)
    def invalidPropertyError(entry: String, propertyFile: File) = throw new InvalidPropertyException(entry, propertyFile)
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

    /** Thrown when a parsing error occurs. */
    class ParsingException(msg: String) extends Exception(msg)

    /** Thrown when an ambiguous condition is detected. */
    class AmbiguousCaseException(msg: String) extends Exception(msg)

    /** Thrown when an unsupported or undefined step is encountered. */
    class UndefinedStepException(step: Step) extends Exception(s"Unsupported or undefined step: ${step}")

    /** Thrown when an attribute cannot be found in a scope. */
    class UnboundAttributeException(name: String, scope: Option[String]) extends Exception(s"Unbound reference${scope.map(x => s" in ${x} scope")getOrElse("")}: ${name}")
    
    /** Thrown when a property setting is not found. */
    class MissingPropertyException(name: String) extends Exception(s"Property not found: ${name}")
    
    /** Thrown when a property file setting is invalid. */
    class InvalidPropertyException(entry: String, propertyFile: File) extends Exception(s"Invalid property entry '${entry}' found in file: ${propertyFile} (name=value expected)")

    /** Thrown when an invalid tag (annotation) is detected. */
    class InvalidTagException(tagString: String) extends Exception(s"Invalid tag: ${tagString}")

    /** Thrown when a regex error occurs. */
    class RegexException(msg: String) extends Exception(msg)

    /** Thrown when a system process fails. */
    class SystemProcessException(msg: String) extends Exception(msg)

    /** Thrown when a xpath evaluation fails. */
    class XPathException(msg: String) extends Exception(msg)
    
    /** Thrown when a JSON path evaluation fails. */
    class JsonPathException(msg: String) extends Exception(msg)

    /** Throw when any evaluation error occurs in the interpreter. */
    class EvaluationException(msg: String) extends Exception(msg)
    
    /** Throw when there is an error in invoking gwen. */
    class InvocationException(msg: String) extends Exception(msg)
    
    /** Signals a step that failed to evaluate. */
    class StepEvaluationException(step: Step, val cause: Throwable) extends RuntimeException(s"Failed step [at line ${step.pos.line}]: ${step}: ${cause.getMessage()}", cause)
    
    /** Signals an infinite recursive StepDef. */
    class RecursiveStepDefException(stepDef: Scenario, step: Step) extends RuntimeException(s"StepDef ${stepDef.name} is infinitely recursive at [line ${step.pos.line}]: ${step}")

    /** Thrown when a decoding error occurs. */
    class DecodingException(msg: String) extends Exception(msg)
    
    /** Thrown when an invalid StepDef is detected. */
    class InvalidStepDefException(stepDef: Scenario, msg: String) extends Exception(s"Invalid StepDef: ${stepDef}: $msg")

  }
}