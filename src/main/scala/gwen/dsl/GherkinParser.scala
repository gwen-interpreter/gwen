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
  *  Feature spec DSL parser and associated combinators. The following 
  *  subset of the 
  *  [[https://github.com/cucumber/cucumber/wiki/Gherkin Gherkin]] grammar 
  *  is supported:
  *  
  *  {{{
  *     
  *     spec        = feature, 
  *                   [background], 
  *                   {scenario}
  *     feature     = {tag}, 
  *                   "Feature:", name
  *                   [narrative]
  *     narrative     "As ", expression
  *                   "I want ", expression
  *                   ["So that ", expression]
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
  *  Any input that does not conform to the grammar results in a parser error.  
  *  The error contains a the location of the error and a description of the 
  *  violation.
  *   
  *  Whitespace and comments are permitted anywhere.  Comments can be single
  *  line comments starting with either '#' or '//', or multi-line comments
  *  enclosed with '/*' and '*/'.  Embedded comments are not supported.
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

  /** Produces a complete feature spec tree. */
  def parseFeatureSpec(feature: String): Try[FeatureSpec] = Try {
    val parser = new Parser[gherkin.ast.Feature](new AstBuilder())
    FeatureSpec(parser.parse(feature))
  }

  /** Produces a step node. */
  def parseStep(step: String): Try[Step] = {
    val parser = new Parser[gherkin.ast.Feature](new AstBuilder())
    val feature = Try(parser.parse(s"Feature:\nScenario:\n${step.trim}"))
    feature
      .map(_.getScenarioDefinitions)
      .filter(!_.isEmpty)
      .map(_.get(0).getSteps)
      .filter(!_.isEmpty)
      .map(steps => Step(steps.get(0)))
      .orElse(parsingError("'Given|When|Then|And|But <expression>' expected"))
  }

}