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
import scala.util.parsing.combinator.JavaTokenParsers

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
  *     expression  = character, {character}
  *     name        = {character}
  *     comment     = "#", expression
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
trait SpecParser extends JavaTokenParsers {

  /**
    * White space, hash or double slash line comments, and multiple-line comments 
    * will be ignored by all parsers.
    */
  protected override val whiteSpace = """(\s|#.*)+""".r

  /** Produces a complete feature spec tree. */
  def spec: Parser[FeatureSpec] = feature ~ (background?) ~ (scenario*) ^^ {
    case feature ~ background ~ scenarios => 
      FeatureSpec(
        feature, 
        background, 
        scenarios
      )
  }

  /** Produces a feature node with optional narrative. */
  def feature: Parser[Feature] = positioned {
    (tag*) ~ (token("Feature") ~ ":" ~> name) ~ ((as ~ iWant ~ soThat)?) ^^ {
      case tags ~ name ~ narrative => narrative match {
        case None => new Feature(tags.toSet, trim(name), Nil)
        case Some(asA ~ iWant ~ soThat) => 
          new Feature(tags.toSet, trim(name), List(trim(asA), trim(iWant), trim(soThat)).filter(_ != ""))
      }
    } withFailureMessage ("Invalid feature declaration, expected >> Feature: name<eol>, [As ..<eol> I want ..<eol> [So that ..<eol>]]")
  }
  
  private def as = """\s*As .+""".r withFailureMessage ("As ..<eol> expected")
  private def iWant = """\s*I want .+""".r withFailureMessage ("I want ..<eol> expected")
  private def soThat = """\s*((So that .+)|^$)""".r withFailureMessage ("So that ..<eol> expected") 
  
  /** Produces a background node. */
  def background: Parser[Background] = positioned {
    token("Background") ~ ":" ~> name ~ (step*) ^^ {
      case name ~ steps =>
        Background(trim(name), steps)
    }
  }

  /** Produces a scenario node. */
  def scenario: Parser[Scenario] = positioned {
    (tag*) ~ (token("Scenario") ~ ":" ~> name) ~ (step*) ^^ {
      case tags ~ name ~ steps =>
        Scenario(tags.toSet, trim(name), None, steps)
    }
  }
  
  /** Produces a tag node. */
  def tag: Parser[Tag] = positioned {
    "@" ~> """(\S)+""".r ^^ { 
      case (name: String) => Tag(trim(name))
    }
  }
  
  /** Produces a step node. */
  def step: Parser[Step] = positioned {
    keyword ~ expression ^^ {
      case (keyword: StepKeyword.Value) ~ expr => Step(keyword, trim(expr))
    }
  }

  /**
    * Produces a step keyword.
    * Reports an error if the given keyword literal is invalid or is not
    * followed by an expression line.
    */
  def keyword: Parser[StepKeyword.Value] = ident ^? (
    { case name if StepKeyword.names.contains(name) => StepKeyword.names(name) }
  ) withFailureMessage s"'${StepKeyword.values.mkString("|")}' expected"

  /** Parses an expression line to ensure it is not empty. */
  private def expression = ".+".r withFailureMessage ("incomplete expression")
  
  /** Parses a name string (allows blanks). */
  private def name = ".*".r
  
  /**
    * Helper method for parsing `Feature`, `Background`, and `Scenario` 
    * token strings. If the token string does not match one of these three, 
    * then an appropriate error is reported listing all valid options. 
    */
  private def token(name: String) = ident ^? (
    { case token if (s"$name".equals(token)) => token }
  ) withFailureMessage(name match {
    case "Background" => "'Background|Scenario' expected"
    case "Scenario" => s"'Scenario|${StepKeyword.values.mkString("|")}' expected" 
    case _ => s"'${name}' expected"
  })
    
  /** Trims a string to remove leading and trailing white space. */
  private val trim = (s: String) => s.replaceAll("""^\s+|\s+$""", "")

}