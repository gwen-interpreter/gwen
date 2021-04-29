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

package gwen.model.gherkin

import gwen._
import gwen.model._
import gwen.model.gherkin.Dialect
import gwen.model.gherkin.Specification
import gwen.model.gherkin.Step

import scala.io.Source
import scala.util.{Failure, Success, Try}

import io.cucumber.gherkin.Gherkin
import io.cucumber.messages.IdGenerator
import io.cucumber.messages.{Messages => Cucumber }

import java.io.File
import java.{util => ju}

/**
  *  Parses a Gherkin feature specification.
  * 
  *  The parsers defined in this class accept gherkin text as input to 
  *  produce an abstract syntax tree in the form of a [[Specification]] object.  
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

  private val languageSyntax = """(?s)\s*#\s*language:\s*(\S+).*""".r

  /** Produces a complete feature spec tree (this method is used to parse entire feature files). */
  def parseFeatureFile(specFile: File): Try[Specification] = {
    try {
      val spec = Source.fromFile(specFile).mkString
      val isMeta = FileIO.isMetaFile(specFile)
      val featureStr = spec match {
        case languageSyntax(lang) =>
          if (isMeta && lang != "en") {
            Errors.metaDialectError(lang, specFile)
          } else {
            spec
          }
        case _ =>
          val language = if (isMeta) "en" else GwenSettings.`gwen.feature.dialect`
          if (FileIO.isFeatureFile(specFile) && language != "en") {
            SourceRef.setLineOffset(-1)
            s"""|# language: $language
                |$spec""".stripMargin
          } else {
            spec
          }
      }
      parseSpecification(featureStr, Some(specFile))
    } finally {
      SourceRef.setLineOffset(0)
    }
  }
  
  /** Produces a complete feature spec tree (this method is used to parse entire features). */
  def parseSpecification(feature: String, specFile: Option[File] = None): Try[Specification] = Try {
    Specification(specFile.map(_.getPath).getOrElse(""), parseDocument(feature), specFile)
  }

  /** Produces a step node (this method is used by the REPL to read in invididual steps only) */
  def parseStep(step: String): Try[Step] = {
    val language = if(StepKeyword.values.exists(k => step.trim.startsWith(s"${k.toString} "))) {
      "en"
    } else {
      Dialect.instance.getLanguage
    }
    Try {
      Dialect.withLanguage(language) {
        Try(parseDocument(s"${if (language != "en") s"# language: ${language}\n${FeatureKeyword.nameOf(FeatureKeyword.Feature)}" else FeatureKeyword.Feature.toString}:\n${FeatureKeyword.nameOf(FeatureKeyword.Scenario)}:\n$step")) match {
          case Success(ast) =>
            Option(ast.getFeature)
            .map(_.getChildrenList)
            .filter(!_.isEmpty)
            .map(_.get(0).getScenario.getStepsList)
            .filter(!_.isEmpty)
            .map(steps => Step("", steps.get(0)))
            .map(_.copy(withSourceRef = None))
            .getOrElse(Errors.syntaxError(s"'${StepKeyword.names.mkString("|")} <expression>' expected", 1))
          case Failure(e) =>
            Errors.syntaxError(s"'${StepKeyword.names.mkString("|")} <expression>' expected: ${e.getMessage}", 1)
        }
      }
    }
  }

  private def parseDocument(feature: String): Cucumber.GherkinDocument = {
    val idGenerator = new IdGenerator.Incrementing()
    val envelope = Gherkin.makeSourceEnvelope(feature, "")
    val envelopes = Gherkin.fromSources(ju.Collections.singletonList(envelope), false, true, false, idGenerator).collect(ju.stream.Collectors.toList())
    val result = envelopes.get(0)
    if (result.hasParseError()) {
      throw new RuntimeException(s"Parser errors:\n${result.getParseError().getMessage()}");
    } else {
      result.getGherkinDocument()
    }
  }

}