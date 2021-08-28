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

package gwen.eval

import gwen._
import gwen.dsl._

import scala.concurrent._
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext
import scala.jdk.CollectionConverters._
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import com.github.tototoshi.csv.CSVReader
import com.typesafe.scalalogging.LazyLogging
import io.cucumber.gherkin.ParserException
import org.apache.log4j.PropertyConfigurator

import java.io.File
import java.util.Date
import java.net.URL
import java.util.concurrent.CopyOnWriteArrayList

/**
  * Interprets incoming feature specs by parsing and evaluating
  * them.  All parsing is performed in the inherited [[gwen.dsl.GherkinParser]].
  * All evaluation is dispatched to a mixed in [[gwen.eval.EvalEngine]].
  * 
  * @author Branko Juric
  */
class GwenInterpreter[T <: EnvContext] extends GwenInfo with GherkinParser with SpecNormaliser with LazyLogging {
  engine: EvalEngine[T] =>

  def addLifecycleEventListener(listener: LifecycleEventListener): Unit = {
    engine.lifecycle.addListener(listener)
  }

  def removeLifecycleEventListener(listener: LifecycleEventListener): Unit = {
    engine.lifecycle.removeListener(listener)
  }

  /**
    * Initialises the interpreter by creating the environment context
    * 
    * @param options command line options
    */
  private[eval] def initialise(options: GwenOptions): T = {
    Settings.getOpt("log4j.configuration").orElse(Settings.getOpt("log4j.configurationFile")).foreach { config => 
      if (config.toLowerCase.trim startsWith "file:") {
        PropertyConfigurator.configure(new URL(config));
      } else {
        PropertyConfigurator.configure(config); 
      }
    }
    engine.init(options) tap { env =>
      Dialect.instance
      logger.info("Environment context initialised")
    }
  }
  
  /**
    * Interprets a single step and dispatches it for evaluation.
    *
    * @param input the input step
    * @param env the environment context
    * @return the evaluated step (or an exception if a runtime error occurs)
    */
  private[eval] def interpretStep(input: String, env: T): Try[Step] = {
    parseStep(input).map { step => 
      engine.evaluateStep(Root, step, 0, env)
    }
  }
  
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
  private[eval] def interpretFeature(unit: FeatureUnit, tagFilters: List[(Tag, Boolean)], env: T, started: Date = new Date()): Option[FeatureResult] = {
    (Option(unit.featureFile).filter(_.exists()) map { (featureFile: File) =>
      val isMeta = FileIO.isMetaFile(featureFile)
      val dataRecord = unit.dataRecord
      dataRecord foreach { rec =>
        env.topScope.set("data record number", rec.recordNo.toString)
      }
      val result = parseFeatureFile(featureFile) match {
        case Success(featureSpec) =>
          if (featureFile.getName.endsWith(".meta")) {
            val metaResults = loadMetaImports(unit, featureSpec, tagFilters, env)
            Some(evaluateFeature(unit.ancestor, normalise(featureSpec, Some(featureFile), dataRecord), metaResults, env, new Date()))
          } else {
            TagsFilter.filter(featureSpec, tagFilters) match {
              case Some(fspec) =>
                lifecycle.beforeUnit(unit, env.scopes)
                val metaResults = loadMetaImports(unit, featureSpec, tagFilters, env) ++ loadMeta(Some(unit), unit.metaFiles, tagFilters, env)
                env.loadedMeta = Nil
                env.topScope.set("gwen.feature.file.name", featureFile.getName)
                env.topScope.set("gwen.feature.file.path", featureFile.getPath)
                env.topScope.set("gwen.feature.file.absolutePath", featureFile.getAbsolutePath)
                env.topScope.set("gwen.feature.name", fspec.feature.name)
                Dialect.withLanguage(fspec.feature.language) {
                  Some(evaluateFeature(unit, normalise(fspec, Some(featureFile), dataRecord), metaResults, env, started))
                }
              case None => 
                logger.info(s"Feature file skipped (does not satisfy tag filters): $featureFile")
                None
            }
          }
        case Failure(e) =>
          e match {
            case pe: ParserException => Errors.syntaxError(unit.uri, pe)
            case _ => Errors.syntaxError(e.getMessage)
          }
      }
      result tap { _ => 
        if (!isMeta) {
          result foreach { res =>
            lifecycle.afterUnit(FeatureUnit(unit.ancestor, unit, res), env.scopes)
          }
        }
      }
    }).getOrElse(None tap { _ => logger.warn(s"Skipped missing feature file: ${unit.featureFile.getPath}") })
  }
  
  /**
    * Evaluates a given Gwen feature.
    */
  private def evaluateFeature(parent: Identifiable, featureSpec: FeatureSpec, metaResults: List[FeatureResult], env: T, started: Date): FeatureResult = {
    val specType = featureSpec.specType
    env.topScope.pushObject(SpecType.toString, specType)
    try {
      lifecycle.beforeFeature(parent, featureSpec, env.scopes)
      (if(featureSpec.isMeta) "Loading" else "Evaluating") tap {action =>
        logger.info("")
        logger.info(s"$action $specType: ${featureSpec.feature.name}${featureSpec.featureFile.map(file => s" [file: $file]").getOrElse("")}")
      }
      val resultSpec = featureSpec.copy(
        withBackground = None,
        withScenarios = evaluateScenarios(featureSpec, featureSpec.scenarios, env),
        withRules = evaluateRules(featureSpec, featureSpec.rules, env),
        withMetaSpecs = metaResults.map(_.spec)
      )
      resultSpec.featureFile foreach { _ =>
        logger.info(s"${if (resultSpec.isMeta) "Loaded" else "Evaluated"} $specType: ${featureSpec.feature.name}${featureSpec.featureFile.map(file => s" [file: $file]").getOrElse("")}")
      }
      logger.debug(prettyPrint(resultSpec))
      new FeatureResult(resultSpec, None, metaResults, started, new Date()) tap { result =>
        if(!featureSpec.isMeta) {
          logStatus(resultSpec)
        } else {
          logger.info(result.toString)
        }
        lifecycle.afterFeature(result, env.scopes)
      }
    } finally {
      featureSpec.featureFile foreach { _ =>
        env.topScope.popObject(SpecType.toString)
      }
    }
  }

  private def evaluateScenarios(parent: Identifiable, scenarios: List[Scenario], env: T): List[Scenario] = {
    val input = scenarios.map(s => if (s.isOutline) expandCSVExamples(s, env) else s)
    if (env.isParallelScenarios && SpecType.isFeature(env.specType) && StateLevel.scenario.equals(env.stateLevel)) {
      val stepDefOutput = input.filter(_.isStepDef).foldLeft(List[Scenario]()) {
        (acc: List[Scenario], scenario: Scenario) =>
          evaluateScenario(parent, scenario, acc, env) :: acc
      }
      val executor = ParallelExecutors.scenarioInstance
      implicit val ec = ExecutionContext.fromExecutorService(executor)
      val acc = new CopyOnWriteArrayList[Scenario](stepDefOutput.asJavaCollection)
      val outputFutures = input.filter(!_.isStepDef).to(LazyList).map { scenario =>
        Future {
          val envClone = env.clone(engine)
          try {
            evaluateScenario(parent, scenario, acc.asScala.toList, envClone) tap { s =>
              acc.add(s)
            }
          } finally {
            envClone.close()
          }
        }
      }
      Await.result(
        Future.sequence(outputFutures.force),
        Duration.Inf
      )
      acc.asScala.toList.sortBy(_.sourceRef.map(_.pos.line).getOrElse(0))
    } else {
      (input.foldLeft(List[Scenario]()) {
        (acc: List[Scenario], scenario: Scenario) =>
          evaluateScenario(parent, scenario, acc, env) :: acc
      }).reverse
    }
  }

  private def evaluateScenario(parent: Identifiable, scenario: Scenario, acc: List[Scenario], env: T): Scenario = {
    if (SpecType.isFeature(env.specType) && !scenario.isStepDef) {
      if (StateLevel.scenario.equals(env.stateLevel)) {
        env.reset(StateLevel.scenario)
      }
      env.topScope.set("gwen.scenario.name", scenario.name)
    }
    EvalStatus(acc.map(_.evalStatus)) match {
      case status @ Failed(_, error) =>
        val isAssertionError = status.isAssertionError
        val isSoftAssert = env.evaluate(false) { isAssertionError && AssertionMode.isSoft }
        val failfast = env.evaluate(false) { GwenSettings.`gwen.feature.failfast` }
        val exitOnFail = env.evaluate(false) { GwenSettings.`gwen.feature.failfast.exit` }
        if (failfast && !exitOnFail && !isSoftAssert) {
          lifecycle.transitionScenario(parent, scenario, Skipped, env.scopes)
        } else if (exitOnFail && !isSoftAssert) {
          lifecycle.transitionScenario(parent, scenario, scenario.evalStatus, env.scopes)
        } else {
          engine.evaluateScenario(parent, scenario, env)
        }
      case _ =>
        engine.evaluateScenario(parent, scenario, env)
    }
  }

  private def evaluateRules(spec: FeatureSpec, rules: List[Rule], env: T): List[Rule] = {
    rules.foldLeft(List[Rule]()) {
      (acc: List[Rule], rule: Rule) =>
        evaluateRule(spec, rule, acc, env) :: acc
    } reverse
  }

  private def evaluateRule(parent: Identifiable, rule: Rule, acc: List[Rule], env: T): Rule = {
    env.topScope.set("gwen.rule.name", rule.name)
    EvalStatus(acc.map(_.evalStatus)) match {
      case status @ Failed(_, error) =>
        val isAssertionError = status.isAssertionError
        val isSoftAssert = env.evaluate(false) { isAssertionError && AssertionMode.isSoft }
        val failfast = env.evaluate(false) { GwenSettings.`gwen.feature.failfast` }
        val exitOnFail = env.evaluate(false) { GwenSettings.`gwen.feature.failfast.exit` }
        if (failfast && !exitOnFail && !isSoftAssert) {
          lifecycle.transitionRule(parent, rule, Skipped, env.scopes)
        } else if (exitOnFail && !isSoftAssert) {
          lifecycle.transitionRule(parent, rule, rule.evalStatus, env.scopes)
        } else {
          lifecycle.beforeRule(parent, rule, env.scopes)
          logger.info(s"Evaluating ${rule.keyword}: $rule")
          rule.copy(
            withScenarios = evaluateScenarios(rule, rule.scenarios, env)
          ) tap { r =>
            logStatus(r)
            lifecycle.afterRule(r, env.scopes)
          }
        }
      case _ =>
        lifecycle.beforeRule(parent, rule, env.scopes)
        logger.info(s"Evaluating ${rule.keyword}: $rule")
        rule.copy(
          withScenarios = evaluateScenarios(rule, rule.scenarios, env)
        ) tap { r =>
          logStatus(r)
          lifecycle.afterRule(r, env.scopes)
        }
    }
  }

  /**
    * Loads meta imports.
    */
  private[eval] def loadMetaImports(unit: FeatureUnit, featureSpec: FeatureSpec, tagFilters: List[(Tag, Boolean)], env: T): List[FeatureResult] =
    getMetaImports(featureSpec, unit.featureFile) flatMap { metaFile => 
      try {
        loadMetaFile(Some(unit), metaFile, tagFilters, env)
      } catch {
        case _: StackOverflowError =>
          Errors.recursiveImportError(Tag(ReservedTags.Import, unit.featureFile.getPath))
      }
    }
  
  private def getMetaImports(featureSpec: FeatureSpec, specFile: File): List[File] = {
    featureSpec.feature.tags.flatMap { tag =>
      tag match {
        case Tag(_, name, Some(filepath)) =>
          if (name == ReservedTags.Import.toString) {
            val file = new File(filepath)
            if (!file.exists()) Errors.missingOrInvalidImportFileError(tag)
            if (!file.getName.endsWith(".meta")) Errors.unsupportedImportError(tag)
            if (file.getCanonicalPath.equals(specFile.getCanonicalPath)) {
              Errors.recursiveImportError(tag)
            }
            Some(file)
          } else if (name.equalsIgnoreCase(ReservedTags.Import.toString)) {
            Errors.invalidTagError(s"""Invalid import syntax: $tag - correct syntax is @Import("filepath")""")
          } else {
            None
          }
        case _ => None
      }
    }
  }
  
  /**
    * Loads the meta.
    * 
    * @param metaFiles the meta files to load
    * @param tagFilters user provided tag filters (includes:(tag, true) and excludes:(tag, false))
    * @param env the environment context
    */
  private[eval] def loadMeta(unit: Option[FeatureUnit], metaFiles: List[File], tagFilters: List[(Tag, Boolean)], env: T): List[FeatureResult] =
    metaFiles.flatMap(metaFile => loadMetaFile(unit, metaFile, tagFilters, env))
  
  private def loadMetaFile(unit: Option[FeatureUnit], metaFile: File, tagFilters: List[(Tag, Boolean)], env: T): Option[FeatureResult] = 
    if (!env.loadedMeta.contains(metaFile)) {
      val metaUnit = FeatureUnit(unit.getOrElse(Root), metaFile, Nil, None)
      interpretFeature(metaUnit, tagFilters, env) tap {
        case Some(metaResult) =>
          val meta = metaResult.spec
          meta.evalStatus match {
            case Passed(_) | Loaded =>
              env.loadedMeta = meta.featureFile.get :: env.loadedMeta
            case Failed(_, error) =>
              Errors.evaluationError(s"Failed to load meta: $meta: ${error.getMessage}")
            case _ =>
              Errors.evaluationError(s"Failed to load meta: $meta")
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
  private def expandCSVExamples(outline: Scenario, env: T): Scenario = {
    val csvExamples = outline.tags.flatMap { tag =>
      tag match {
        case Tag(_, name, Some(fileValue)) =>
          if (name == ReservedTags.Examples.toString) {
            val filepath = env.interpolate(fileValue)(env.getBoundReferenceValue)
            val examplesTag = tag.copy(withValue = Some(filepath))
            val file = new File(filepath)
            if (!file.exists()) Errors.missingOrInvalidImportFileError(examplesTag)
            if (!file.getName.toLowerCase.endsWith(".csv")) Errors.unsupportedDataFileError(examplesTag)
            val table = CSVReader.open(file).iterator.toList.zipWithIndex map { case (row, idx) => (idx + 1, row.toList) }
            Some(Examples(tag.sourceRef, Nil, FeatureKeyword.nameOf(FeatureKeyword.Examples), s"Data file: $filepath", Nil, table, Nil))
          } else if (name.equalsIgnoreCase(ReservedTags.Examples.toString)) {
            Errors.invalidTagError(s"""Invalid Examples tag syntax: $tag - correct syntax is @Examples("path/file.csv")""")
          } else {
            None
          }
        case _ => None
      }
    }
    csvExamples match {
      case Nil => outline
      case _ =>
        val outLinePath = outline.sourceRef.flatMap(_.nodePath).getOrElse("/")
        val examples = expandScenarioOutline(
            outline.copy(withExamples = csvExamples),
            outline.background
          ).examples
        outline.copy(
          withExamples = outline.examples ++ examples
        ).withNodePath(outLinePath)
    }
  }
  
}

