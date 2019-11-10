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

package gwen.dsl

import gwen.errors._

import io.cucumber.gherkin.Gherkin
import io.cucumber.messages.Messages.GherkinDocument

import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

import java.util.Collections
import java.util.stream.Collectors
import io.cucumber.gherkin.ParserException

/**
  *  Parses a Gherkin feature specification.
  * 
  *  The parsers defined in this class accept gherkin text as input to 
  *  produce an abstract syntax tree in the form of a [[FeatureSpec]] object.  
  *  Any input that does not conform to the standard Gherkin grammar results 
  *  in a parser error.  The error contains a the location of the error and 
  *  a description of the violation.
  *   
  *  The following example shows a valid feature that conforms to the
  *  grammar and will parse successfully (whether or not it evaluates 
  *  is the responsibility of the [[gwen.eval.EvalEngine evaluation]] 
  *  layer):
  *  
  *  {{{
  *  
  *     Feature: Gwen
  *
  *  Background: The butterfly effect
  *        Given a deterministic nonlinear system
  *         When a small change is initially applied
  *         Then a large change will eventually result
  *       
  *    Scenario: Evaluation
  *        Given a software behavior
  *         When expressed in Gherkin
  *         Then Gwen can evaluate it
  *      
  *  }}}
  *      
  *  @author Branko Juric
  */
trait GherkinParser {

  /** Produces a complete feature spec tree (this method is used to parse entire feature files). */
  def parseFeatureSpec(feature: String): Try[FeatureSpec] = Try {
    FeatureSpec(parseDocument(feature))
  }

  /** Produces a step node (this method is used by the REPL to read in invididual steps only) */
  def parseStep(step: String): Try[Step] = {
    Try {
      Try(parseDocument(s"Feature:\nScenario:\n$step")) match {
        case Success(ast) =>
          Option(ast.getFeature)
          .map(_.getChildrenList)
          .filter(!_.isEmpty)
          .map(_.get(0).getScenario.getStepsList)
          .filter(!_.isEmpty)
          .map(steps => Step(steps.get(0)))
          .map(step => Step(step, Position(1, step.pos.column)))
          .getOrElse(syntaxError(s"'Given|When|Then|And|But <expression>' expected", 1))
        case Failure(e) =>
          syntaxError(s"'Given|When|Then|And|But <expression>' expected: ${e.getMessage}", 1)
      }
    }
  }

  private def parseDocument(feature: String): GherkinDocument = {
    val envelope = Gherkin.makeSourceEnvelope(feature, "")
    val envelopes = Gherkin.fromSources(Collections.singletonList(envelope), false, true, false).collect(Collectors.toList())
    val result = envelopes.get(0)
    if (result.hasAttachment()) {
      throw new RuntimeException(s"Parser errors:\n${result.getAttachment().getData()}");
    } else {
      result.getGherkinDocument()
    }
  }

}