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

package gwen.core.node.gherkin

import gwen.core._
import gwen.core.node.SourceRef

import scala.io.Source
import scala.util.{Failure, Success, Try}

import io.cucumber.gherkin.{GherkinParser => CucumberParser}
import io.cucumber.messages.{ types => cucumber }

import java.io.File
import java.{util => ju}

import scala.jdk.OptionConverters._

/**
  *  Parses a Gherkin feature specification.
  */

trait GherkinParser {

  private val languageSyntax = """(?s)\s*#\s*language:\s*(\S+).*""".r

  /** Parses a Gherkin feature specification */
  def parseSpec(specFile: File, verbatim: Boolean = false): Try[Spec] = {
    try {
      val spec = Source.fromFile(specFile).mkString
      if (spec.trim.isEmpty) Errors.syntaxError(s"Empty specification in file: $specFile")
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
      parseSpec(featureStr, Some(specFile), verbatim)
    } finally {
      SourceRef.setLineOffset(0)
    }
  }
  
  /** Produces a complete feature spec tree (this method is used to parse entire features). */
  def parseSpec(feature: String): Try[Spec] = {
    parseSpec(feature, None, verbatim = false)
  }

  /** Produces a complete feature spec tree (this method is used to parse entire features). */
  private def parseSpec(feature: String, specFile: Option[File], verbatim: Boolean): Try[Spec] = Try {
    Spec(specFile, parseDocument(feature, specFile), verbatim)
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
        Try(parseDocument(s"${if (language != "en") s"# language: ${language}\n${FeatureKeyword.nameOf(FeatureKeyword.Feature)}" else FeatureKeyword.Feature.toString}:\n${FeatureKeyword.nameOf(FeatureKeyword.Scenario)}:\n$step", None)) match {
          case Success(ast) =>
            ast.getFeature.toScala
            .map(_.getChildren)
            .filter(!_.isEmpty)
            .flatMap(_.get(0).getScenario.toScala)
            .map(_.getSteps)
            .filter(!_.isEmpty)
            .map(steps => Step(None, steps.get(0)))
            .map(_.copy(withSourceRef = None))
            .getOrElse(Errors.syntaxError(s"'${StepKeyword.names.mkString("|")} <expression>' expected", 1))
          case Failure(e) =>
            Errors.syntaxError(s"'${StepKeyword.names.mkString("|")} <expression>' expected: ${e.getMessage}", 1)
        }
      }
    }
  }

  private def parseDocument(feature: String, file: Option[File]): cucumber.GherkinDocument = {
    val input = cucumber.Envelope.of(
      new cucumber.Source(
        file.map(_.getPath).getOrElse(""), 
        feature, 
        cucumber.SourceMediaType.TEXT_X_CUCUMBER_GHERKIN_PLAIN)
    )
    val result = CucumberParser.builder
        .includeSource(false)
        .includePickles(false)
        .build
        .parse(input)
        .findFirst.get
    result.getParseError.toScala map { error => 
      Errors.parseError(error)
    } getOrElse {
      result.getGherkinDocument.get
    }
  }

}