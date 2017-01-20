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
import gwen.dsl.Background
import gwen.dsl.EvalStatus
import gwen.dsl.Failed
import gwen.dsl.FeatureSpec
import gwen.dsl.GherkinParser
import gwen.dsl.Loaded
import gwen.dsl.Passed
import gwen.dsl.Scenario
import gwen.dsl.Skipped
import gwen.dsl.SpecNormaliser
import gwen.dsl.SpecType
import gwen.dsl.Step
import gwen.dsl.Tag
import gwen.dsl.prettyPrint
import gwen.errors._
import scala.concurrent.duration.Duration
import java.util.Date

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
    logger.info("Initialising environment context")
    engine.init(options,  new ScopedDataStack()) tap { env =>
      logger.info(s"${env.getClass().getSimpleName()} initialised")
    }
  }
  
  /**
    * Closes the given environment context.
    * 
    * @param env the environment context to close
    */
  private[eval] def close(env: T) {
    logger.info("Closing environment context")
    env.close();
  }
  
  /**
    * Resets the given environment context without closing it so it can be reused.
    * 
    * @param env the environment context to reset
    */
  private[eval] def reset(env: T) {
    logger.info("Resetting environment context")
    env.reset();
  }
  
  /**
    * Interprets a single step and dispatches it for evaluation.
    *
    * @param input the input step
    * @param env the environment context
    * @return the evaluated step (or an exception if a runtime error occurs)
    * @throws gwen.errors.ParsingException if the given step fails to parse
    */
  private[eval] def interpretStep(input: String, env: T): Try[Step] = 
    parseStep(input).map(engine.evaluateStep(_, env))
  
  /**
    * Interprets an incoming feature.
    *
    * @param featureUnit the feature unit to execute
    * @param tagFilters user provided tag filters (includes:(tag, true) and excludes:(tag, false))
    * @param env the environment context
    * @param started the started time (default is current date)
    * @return the evaluated feature or nothing if the feature does not 
    *         satisfy specified tag filters
    * @throws gwen.errors.ParsingException if the given feature fails to parse
    */
  private[eval] def interpretFeature(unit: FeatureUnit, tagFilters: List[(Tag, Boolean)], env: T, started: Date = new Date()): Option[FeatureResult] = 
    (Option(unit.featureFile).filter(_.exists()) map { (featureFile: File) =>
      val dataRecord = unit.dataRecord 
      parseFeatureSpec(Source.fromFile(featureFile).mkString) match {
        case Success(featureSpec) =>
          if (featureFile.getName().endsWith(".meta")) {
            val metaResults = loadMetaImports(featureSpec, featureFile, tagFilters, env)
            Some(evaluateFeature(normalise(featureSpec, Some(featureFile), dataRecord), metaResults, env, new Date()))
          } else {
            TagsFilter.filter(featureSpec, tagFilters) match {
              case Some(fspec) =>
                val metaResults = loadMetaImports(featureSpec, featureFile, tagFilters, env) ++ loadMeta(unit.metaFiles, tagFilters, env)
                env.loadedMeta = Nil
                Some(evaluateFeature(normalise(fspec, Some(featureFile), dataRecord), metaResults, env, started))
              case None => 
                logger.info(s"Feature file skipped (does not satisfy tag filters): $featureFile")
                None
            }
          }
        case Failure(e) =>
          parsingError(e.toString)
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
    val specType = featureSpec.featureFile.collect { case f if(isMetaFile(f)) => SpecType.meta } getOrElse SpecType.feature
    (if(SpecType.meta.equals(specType)) "Loading" else "Evaluating") tap {action =>
      logger.info("");
      logger.info(s"${action} ${specType}: ${featureSpec.feature.name}${featureSpec.featureFile.map(file => s" [file: ${file}]").getOrElse("")}")
    }
    val resultSpec = FeatureSpec(
      featureSpec.feature, 
      None, 
      featureSpec.scenarios.foldLeft(List[Scenario]()) {
        (acc: List[Scenario], scenario: Scenario) => 
          (EvalStatus(acc.map(_.evalStatus)) match {
            case Failed(_, _) =>
              val failfast = env.execute(GwenSettings.`gwen.feature.failfast`).getOrElse(false)
              val exitOnFail = env.execute(GwenSettings.`gwen.feature.failfast.exit`).getOrElse(false)
              if (failfast && !exitOnFail) {
                Scenario(
                  scenario, 
                  scenario.background.map(bg => Background(bg, bg.steps.map(step => Step(step, Skipped, step.attachments)))),
                  scenario.steps.map(step => Step(step, Skipped, step.attachments))
                )
              } else if (exitOnFail) {
                scenario
              } else {
                evaluateScenario(scenario, env)
              }
            case _ => 
              evaluateScenario(scenario, env)
          }) :: acc
      } reverse,
      featureSpec.featureFile,
      metaResults.map(_.spec)
    )
    resultSpec.featureFile foreach { file =>
      logger.info(s"${(if(SpecType.meta.equals(specType)) "Loaded" else "Evaluated")} ${specType}: ${featureSpec.feature.name}${featureSpec.featureFile.map(file => s" [file: ${file}]").getOrElse("")}")
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
  
  /**
    * Evaluates a given scenario.
    * 
    * @param scenario the scenario to evaluate
    * @param env the environment context
    * @return the evaluated scenario
    */
  private def evaluateScenario(scenario: Scenario, env: T): Scenario = {
    if (scenario.isStepDef) {
      logger.info(s"Loading StepDef: ${scenario.name}")
      env.addStepDef(scenario) 
      Scenario(scenario, None, scenario.steps map { step =>
        Step(step, Loaded, step.attachments)
      }) tap { scenario =>
        logStatus("StepDef", scenario.toString, scenario.evalStatus)
      }
    } else {
      logger.info(s"Evaluating Scenario: $scenario")
      (scenario.background map(evaluateBackground(_, env)) match {
        case None => 
          Scenario(scenario, None, engine.evaluateSteps(scenario.steps, env))
        case Some(background) => 
          Scenario(
            scenario,
            Some(background),
            background.evalStatus match {
              case Passed(_) => engine.evaluateSteps(scenario.steps, env)
              case Skipped if (background.steps.isEmpty) => engine.evaluateSteps(scenario.steps, env)
              case _ => scenario.steps map { step =>
                Step(step, Skipped, step.attachments)
              }
            })
      }) tap { scenario =>
        logStatus("Scenario", scenario.toString, scenario.evalStatus)
      } 
    }
  }
  
  /**
    * Evaluates a given background.
    * 
    * @param background the background to evaluate
    * @param env the environment context
    * @return the evaluated background
    */
  private def evaluateBackground(background: Background, env: T): Background = {
    logger.info(s"Evaluating Background: $background")
    Background(background, engine.evaluateSteps(background.steps, env)) tap { bg =>
      logStatus("Background", bg.toString, bg.evalStatus)
    }
  }
  
  /**
    * Loads meta imports.
    * 
    * @param featureSpec the current feature spec
    * @param featureFile the current feature file
    * @param tagFilters user provided tag filters (includes:(tag, true) and excludes:(tag, false))
    * @param env the environment context
    * @throws gwen.errors.ParsingException if the given meta fails to parse
    */
  private[eval] def loadMetaImports(featureSpec: FeatureSpec, featureFile: File, tagFilters: List[(Tag, Boolean)], env: T): List[FeatureResult] =  
    getMetaImports(featureSpec, featureFile) flatMap { metaFile => 
      try {
        loadMetaFile(metaFile, tagFilters, env)
      } catch {
        case e: StackOverflowError =>
          recursiveImportError(Tag(s"""Import("$featureFile")"""), metaFile)
      }
    }
  
  private def getMetaImports(featureSpec: FeatureSpec, specFile: File): List[File] = featureSpec.feature.tags.toList.flatMap { tag =>
    tag.name.trim match {
      case r"""Import\("(.*?)"$filepath\)""" =>
        val file = new File(filepath)
        if (!file.getName().endsWith(".meta")) unsupportedImportError(tag, specFile)
        if (!file.exists()) missingImportError(tag, specFile)
        if (file.getCanonicalPath().equals(specFile.getCanonicalPath())) {
          recursiveImportError(tag, specFile)
        }
        Some(file)
      case r"""(?:I|i)mport\(.*""" =>
        syntaxError(s"""Invalid import syntax: $tag - correct syntax is @Import("filepath")""")
      case _ => None
    }
  }
  
  /**
    * Loads the meta.
    * 
    * @param metaFiles the meta files to load
    * @param tagFilters user provided tag filters (includes:(tag, true) and excludes:(tag, false))
    * @param env the environment context
    * @throws gwen.errors.ParsingException if the given meta fails to parse
    */
  private[eval] def loadMeta(metaFiles: List[File], tagFilters: List[(Tag, Boolean)], env: T): List[FeatureResult] =
    metaFiles.flatMap(loadMetaFile(_, tagFilters, env))
  
  private def loadMetaFile(metaFile: File, tagFilters: List[(Tag, Boolean)], env: T): Option[FeatureResult] = 
    if (!env.loadedMeta.contains(metaFile)) {
      interpretFeature(new FeatureUnit(metaFile, Nil, None), tagFilters, env) tap { metas =>
        metas match {
          case Some(metaResult) =>
            val meta = metaResult.spec
            meta.evalStatus match {
              case Passed(_) | Loaded =>
                env.loadedMeta = meta.featureFile.get :: env.loadedMeta
              case Failed(_, error) =>
                evaluationError(s"Failed to load meta: $meta: ${error.getMessage()}")
              case _ =>
                evaluationError(s"Failed to load meta: $meta")
            }
          case _ => Nil
        }
      }
    } else None
  
}

