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

  package object errors {

    import gwen.dsl.Step

    def parsingError(msg: String) = throw new ParsingException(msg)
    def ambiguousCaseError(msg: String) = throw new AmbiguousCaseException(msg)
    def undefinedStepError(step: Step) = throw new UndefinedStepException(step)
    def unboundAttributeError(name: String) = throw new UnboundAttributeException(name, None)
    def unboundAttributeError(name: String, scope: String) = throw new UnboundAttributeException(name, Some(scope))
    def missingPropertyError(name: String) = throw new MissingPropertyException(name)
    def invalidTagError(msg: String) = throw new InvalidTagException(msg)
    def regexError(msg: String) = throw new RegexException(msg)
    def systemProcessError(msg: String) = throw new SystemProcessException(msg)
    def xPathError(msg: String) = throw new XPathException(msg)
    def evaluationError(msg: String) = throw new EvaluationException(msg)
    def invocationError(msg: String) = throw new InvocationException(msg)

    /** Thrown when a parsing error occurs. */
    class ParsingException(msg: String) extends Exception(msg)

    /** Thrown when an ambiguous condition is detected. */
    class AmbiguousCaseException(msg: String) extends Exception(msg)

    /** Thrown when an unsupported or undefined step is encountered. */
    class UndefinedStepException(step: Step) extends Exception(s"Unsupported or undefined step: ${step}")

    /** Thrown when an attribute cannot be found in a scope. */
    class UnboundAttributeException(name: String, scope: Option[String]) extends Exception(s"Unbound reference${scope.map(x => s" in ${x} scope")getOrElse("")}: ${name}")
    
    /** Thrown when an attribute cannot be found in a scope. */
    class MissingPropertyException(name: String) extends Exception(s"Property not found: ${name}")

    /** Thrown when an invalid tag (annotation) is detected. */
    class InvalidTagException(tagString: String) extends Exception(s"Invalid tag: ${tagString}")

    /** Thrown when a regex error occurs. */
    class RegexException(msg: String) extends Exception(msg)

    /** Thrown when a system process fails. */
    class SystemProcessException(msg: String) extends Exception(msg)

    /** Thrown when a xpath evaluation fails. */
    class XPathException(msg: String) extends Exception(msg)

    /** Throw when any evaluation error occurs in the interpreter. */
    class EvaluationException(msg: String) extends Exception(msg)
    
    /** Throw when there is an error in invoking gwen. */
    class InvocationException(msg: String) extends Exception(msg)

  }
}