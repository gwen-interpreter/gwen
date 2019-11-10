/*
 * Copyright 2014-2019 Branko Juric, Brady Wood
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

package gwen.eval

import java.io.File

import scala.io.Source
import scala.language.postfixOps
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import com.typesafe.scalalogging.LazyLogging
import gwen.GwenInfo
import gwen.GwenSettings
import gwen.Predefs.Kestrel
import gwen.Predefs.RegexContext
import gwen.Predefs.FileIO._
import gwen.dsl._
import gwen.errors._
import java.util.Date

import com.github.tototoshi.csv.CSVReader
import io.cucumber.gherkin.ParserException

/**
  * Interprets incoming feature specs by parsing and evaluating
  * them.  All parsing is performed in the inherited [[gwen.dsl.GherkinParser]].
  * All evaluation is dispatched to a mixed in [[gwen.eval.EvalEngine]].
  * 
  * @author Branko Juric
  */
class GwenInterpreter[T <: EnvContext] extends GwenInfo with GherkinParser with SpecNormaliser with LazyLogging {
  engine: EvalEngine[T] =>

  /**
    * Initialises the interpreter by creating the environment context
    * 
    * @param options command line options
    */
  private[eval] def initialise(options: GwenOptions): T = {
    engine.init(options, new ScopedDataStack()) tap { env =>
      logger.info("Environment context initialised")
    }
  }
  
  /**
    * Closes the given environment context.
    * 
    * @param env the environment context to close
    */
  private[eval] def close(env: T) {
    logger.info("Closing environment context")
    env.close()
  }
  
  /**
    * Resets the given environment context without closing it so it can be reused.
    * 
    * @param env the environment context to reset
    */
  private[eval] def reset(env: T) {
    logger.info("Resetting environment context")
    env.reset()
  }
  
  /**
    * Interprets a single step and dispatches it for evaluation.
    *
    * @param input the input step
    * @param env the environment context
    * @return the evaluated step (or an exception if a runtime error occurs)
    */
  private[eval] def interpretStep(input: String, env: T): Try[Step] = 
    parseStep(input).map(engine.evaluateStep(_, env))
  
  /**
    * Interprets an incoming feature.
    *
    * @param unit the feature unit to execute
    * @param tagFilters user provided tag filters (includes:(tag, true) and excludes:(tag, false))
    * @param env the environment context
    * @param started the started time (default is current date)
    * @return the evaluated feature or nothing if the feature does not 
    *         satisfy specified tag filters
    */
  private[eval] def interpretFeature(unit: FeatureUnit, tagFilters: List[(Tag, Boolean)], env: T, started: Date = new Date()): Option[FeatureResult] =
    (Option(unit.featureFile).filter(_.exists()) map { (featureFile: File) =>
      val dataRecord = unit.dataRecord
      dataRecord foreach { rec =>
        env.featureScope.set("data record number", rec.recordNo.toString)
      }
      parseFeatureSpec(Source.fromFile(featureFile).mkString) match {
        case Success(featureSpec) =>
          if (featureFile.getName.endsWith(".meta")) {
            val metaResults = loadMetaImports(featureSpec, featureFile, tagFilters, env)
            Some(evaluateFeature(normalise(featureSpec, Some(featureFile), dataRecord), metaResults, env, new Date()))
          } else {
            TagsFilter.filter(featureSpec, tagFilters) match {
              case Some(fspec) =>
                val metaResults = loadMetaImports(featureSpec, featureFile, tagFilters, env) ++ loadMeta(unit.metaFiles, tagFilters, env)
                env.loadedMeta = Nil
                env.featureScope.set("gwen.feature.file.name", featureFile.getName)
                env.featureScope.set("gwen.feature.file.path", featureFile.getPath)
                env.featureScope.set("gwen.feature.file.absolutePath", featureFile.getAbsolutePath)
                env.featureScope.set("gwen.feature.name", fspec.feature.name)
                Some(evaluateFeature(normalise(fspec, Some(featureFile), dataRecord), metaResults, env, started))
              case None => 
                logger.info(s"Feature file skipped (does not satisfy tag filters): $featureFile")
                None
            }
          }
        case Failure(e) =>
          e match {
            case pe: ParserException => syntaxError(pe)
            case _ => syntaxError(e.getMessage)
          }
      }
    }).getOrElse(None tap { _ => logger.warn(s"Skipped missing feature file: ${unit.featureFile.getPath}") })
  
  /**
    * Evaluates a given Gwen feature.
    * 
    * @param featureSpec the Gwen feature to evaluate
    * @param metaResults the evaluated meta results (Nil if featureSpec is a meta file)
    * @param env the environment context
    * @param started the started time
    * @return the evaluated Gwen feature result
    */
  private def evaluateFeature(featureSpec: FeatureSpec, metaResults: List[FeatureResult], env: T, started: Date): FeatureResult = {
    val specType = featureSpec.featureFile.collect { case f if isMetaFile(f) => SpecType.meta } getOrElse SpecType.feature
    (if(SpecType.meta.equals(specType)) "Loading" else "Evaluating") tap {action =>
      logger.info("")
      logger.info(s"$action $specType: ${featureSpec.feature.name}${featureSpec.featureFile.map(file => s" [file: $file]").getOrElse("")}")
    }
    val resultSpec = FeatureSpec(
      featureSpec.feature, 
      None, 
      evaluateScenarios(featureSpec.scenarios, specType, env),
      evaluateRules(featureSpec.rules, specType, env),
      featureSpec.featureFile,
      metaResults.map(_.spec)
    )
    resultSpec.featureFile foreach { _ =>
      logger.info(s"${if (SpecType.meta.equals(specType)) "Loaded" else "Evaluated"} $specType: ${featureSpec.feature.name}${featureSpec.featureFile.map(file => s" [file: $file]").getOrElse("")}")
    }
    logger.debug(prettyPrint(resultSpec))
    new FeatureResult(resultSpec, None, metaResults, started, new Date()) tap { result =>
      if(SpecType.meta != specType) {
        logStatus(specType.toString, resultSpec.toString, resultSpec.evalStatus)
      } else {
        logger.info(result.toString)
      }
    }
  }

  private def evaluateScenarios(scenarios: List[Scenario], specType: SpecType.Value, env: T): List[Scenario] = {
    scenarios.map(s => if (s.isOutline) expandCSVExamples(s, env) else s).foldLeft(List[Scenario]()) {
      (acc: List[Scenario], scenario: Scenario) =>
        evaluateScenario(scenario, specType, acc, env) :: acc
    } reverse
  }

  private def evaluateScenario(scenario: Scenario, specType: SpecType.Value, acc: List[Scenario], env: T): Scenario = {
    if (SpecType.feature.equals(specType) && !scenario.isStepDef) {
      env.featureScope.set("gwen.scenario.name", scenario.name)
    }
    EvalStatus(acc.map(_.evalStatus)) match {
      case status @ Failed(_, error) =>
        val isAssertionError = status.isAssertionError
        val isSoftAssert = env.evaluate(false) { isAssertionError && GwenSettings.`gwen.assertion.mode` == AssertionMode.soft }
        val failfast = env.evaluate(false) { GwenSettings.`gwen.feature.failfast` }
        val exitOnFail = env.evaluate(false) { GwenSettings.`gwen.feature.failfast.exit` }
        if (failfast && !exitOnFail && !isSoftAssert) {
          scenario.skip
        } else if (exitOnFail && !isSoftAssert) {
          scenario
        } else {
          engine.evaluateScenario(scenario, env)
        }
      case _ =>
        engine.evaluateScenario(scenario, env)
    }
  }

  private def evaluateRules(rules: List[Rule], specType: SpecType.Value, env: T): List[Rule] = {
    rules.foldLeft(List[Rule]()) {
      (acc: List[Rule], rule: Rule) =>
        evaluateRule(rule, specType, acc, env) :: acc
    } reverse
  }

  private def evaluateRule(rule: Rule, specType: SpecType.Value, acc: List[Rule], env: T): Rule = {
    env.featureScope.set("gwen.rule.name", rule.name)
    EvalStatus(acc.map(_.evalStatus)) match {
      case status @ Failed(_, error) =>
        val isAssertionError = status.isAssertionError
        val isSoftAssert = env.evaluate(false) { isAssertionError && GwenSettings.`gwen.assertion.mode` == AssertionMode.soft }
        val failfast = env.evaluate(false) { GwenSettings.`gwen.feature.failfast` }
        val exitOnFail = env.evaluate(false) { GwenSettings.`gwen.feature.failfast.exit` }
        if (failfast && !exitOnFail && !isSoftAssert) {
          rule.skip
        } else if (exitOnFail && !isSoftAssert) {
          rule
        } else {
          logger.info(s"Evaluating ${Rule.keyword}: $rule")
          Rule(rule, evaluateScenarios(rule.scenarios, specType, env)) tap { r =>
            logStatus(Rule.keyword, r.toString, r.evalStatus)
          }
        }
      case _ =>
        logger.info(s"Evaluating ${Rule.keyword}: $rule")
        Rule(rule, evaluateScenarios(rule.scenarios, specType, env)) tap { r =>
          logStatus(Rule.keyword, r.toString, r.evalStatus)
        }
    }
  }

  /**
    * Loads meta imports.
    * 
    * @param featureSpec the current feature spec
    * @param featureFile the current feature file
    * @param tagFilters user provided tag filters (includes:(tag, true) and excludes:(tag, false))
    * @param env the environment context
    */
  private[eval] def loadMetaImports(featureSpec: FeatureSpec, featureFile: File, tagFilters: List[(Tag, Boolean)], env: T): List[FeatureResult] =
    getMetaImports(featureSpec, featureFile) flatMap { metaFile => 
      try {
        loadMetaFile(metaFile, tagFilters, env)
      } catch {
        case _: StackOverflowError =>
          recursiveImportError(Tag(s"""Import("$featureFile")"""), metaFile)
      }
    }
  
  private def getMetaImports(featureSpec: FeatureSpec, specFile: File): List[File] = featureSpec.feature.tags.flatMap { tag =>
    tag.name.trim match {
      case r"""Import\("(.*?)"$filepath\)""" =>
        val file = new File(filepath)
        if (!file.exists()) missingOrInvalidImportFileError(tag, Some(specFile))
        if (!file.getName.endsWith(".meta")) unsupportedImportError(tag, specFile)
        if (file.getCanonicalPath.equals(specFile.getCanonicalPath)) {
          recursiveImportError(tag, specFile)
        }
        Some(file)
      case r"""(?:I|i)mport\(.*""" =>
        invalidTagError(s"""Invalid import syntax: $tag - correct syntax is @Import("filepath")""")
      case _ => None
    }
  }
  
  /**
    * Loads the meta.
    * 
    * @param metaFiles the meta files to load
    * @param tagFilters user provided tag filters (includes:(tag, true) and excludes:(tag, false))
    * @param env the environment context
    */
  private[eval] def loadMeta(metaFiles: List[File], tagFilters: List[(Tag, Boolean)], env: T): List[FeatureResult] =
    metaFiles.flatMap(loadMetaFile(_, tagFilters, env))
  
  private def loadMetaFile(metaFile: File, tagFilters: List[(Tag, Boolean)], env: T): Option[FeatureResult] = 
    if (!env.loadedMeta.contains(metaFile)) {
      interpretFeature(FeatureUnit(metaFile, Nil, None), tagFilters, env) tap {
        case Some(metaResult) =>
          val meta = metaResult.spec
          meta.evalStatus match {
            case Passed(_) | Loaded =>
              env.loadedMeta = meta.featureFile.get :: env.loadedMeta
            case Failed(_, error) =>
              evaluationError(s"Failed to load meta: $meta: ${error.getMessage}")
            case _ =>
              evaluationError(s"Failed to load meta: $meta")
          }
        case _ => Nil
      }
    } else None

  /**
    * Loads the CSV examples for every Examples(file.csv) tag on the given outline and expands them.
    *
    * @param outline the scenario outline
    * @return a new scenario outline containing the loaded examples data
    *         or the unchanged outline if no csv data is specified or if incoming scenario is not an outline
    */
  private def expandCSVExamples(outline: Scenario, env: T): Scenario =
    outline.tags.flatMap { tag =>
      tag.name.trim match {
        case r"""Examples\("(.*?)"$filestr\)""" =>
          val filepath = env.interpolate(filestr)(env.getBoundReferenceValue)
          val importTag = Tag(s"""Examples("$filepath")""")
          val file = new File(filepath)
          if (!file.exists()) missingOrInvalidImportFileError(importTag, None)
          if (!file.getName.toLowerCase.endsWith(".csv")) unsupportedDataFileError(importTag, None)
          val table = CSVReader.open(file).iterator.toList.zipWithIndex map { case (row, idx) => (idx + 1, row.toList) }
          Some(Examples(Nil, s"Data file: $filepath", Nil, table, Nil))
        case r"""(?:E|e)xamples\(.*""" =>
          invalidTagError(s"""Invalid Examples tag syntax: $tag - correct syntax is @Examples("path/file.csv")""")
        case _ => None
      }
    } match {
      case Nil => outline
      case csvExamples =>
        val examples = expandScenarioOutline(Scenario(outline, csvExamples), outline.background).examples
        Scenario(outline, outline.examples ++ examples)
    }
  
}

