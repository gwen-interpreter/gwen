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

import scala.language.postfixOps
import gherkin.Parser
import gherkin.AstBuilder
import scala.util.Try
import gherkin.Token
import gherkin.TokenMatcher
import gherkin.GherkinLine
import gherkin.Parser.RuleType
import scala.collection.JavaConversions._
import gwen.errors._

/**
  *  Uses the [[https://github.com/cucumber/gherkin3 Gherkin 3]] parser to  
  *  produce an abstract syntax tree. Althouth the entire Gherkin grammar can 
  *  be parsed, only the following minimal subset is supported by Gwen (we can 
  *  add support for the remaining subsets in the future if required).
  *  
  *  {{{
  *     
  *     spec        = feature, 
  *                   [background], 
  *                   {scenario}
  *     feature     = {tag}, 
  *                   "Feature:", name
  *                   [narrative]
  *     narrative     {expression}
  *     background  = "Background:", name
  *                   {step}
  *     scenario    = {tag}, 
  *                   "Scenario:", name
  *                   {step}
  *     tag         = "@", name
  *     step        = keyword, expression
  *     keyword     = "Given" | "When" | "Then" | "And" | "But"
  *     name        = expression
  *     comment     = "#", expression
  *     expression  = character, {character}
  *  
  *  }}}
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
    val parser = new Parser[gherkin.ast.Feature](new AstBuilder())
    FeatureSpec(parser.parse(feature))
  }

  /** Produces a step node (this method is used by the REPL to read in invididual steps only) */
  def parseStep(step: String): Try[Step] = {
    val parser = new Parser[gherkin.ast.Feature](new AstBuilder())
    Try(parser.parse(s"Feature:\nScenario:\n${step}"))
      .map(_.getScenarioDefinitions)
      .filter(!_.isEmpty)
      .map(_.get(0).getSteps)
      .filter(!_.isEmpty)
      .map(steps => Step(steps.get(0)))
      .map(step => Step(step, Position(1, step.pos.column)))
      .orElse(parsingError("'Given|When|Then|And|But <expression>' expected"))
  }

}