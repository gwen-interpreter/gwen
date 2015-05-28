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
import scala.Option.option2Iterable
import scala.annotation.migration
import scala.io.Source
import scala.language.postfixOps
import scala.util.{Failure => TryFailure}
import scala.util.{Success => TrySuccess}
import scala.util.Try
import com.typesafe.scalalogging.slf4j.LazyLogging
import gwen.GwenSettings
import gwen.Predefs.Kestrel
import gwen.dsl.Background
import gwen.dsl.EvalStatus
import gwen.dsl.Failed
import gwen.dsl.FeatureSpec
import gwen.dsl.Loaded
import gwen.dsl.Passed
import gwen.dsl.Scenario
import gwen.dsl.Skipped
import gwen.dsl.SpecNormaliser
import gwen.dsl.SpecParser
import gwen.dsl.SpecType
import gwen.dsl.Step
import gwen.dsl.Tag
import gwen.dsl.prettyPrint
import gwen.GwenInfo

/**
  * Interprets incoming feature specs by parsing and evaluating
  * them.  All parsing is performed in the inherited [[gwen.dsl.SpecParser]].
  * All evaluation is dispatched to a mixed in [[gwen.eval.EvalEngine]].
  * 
  * @author Branko Juric
  */
class GwenInterpreter[T <: EnvContext] extends GwenInfo with SpecParser with SpecNormaliser with LazyLogging {
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
    */
  private[eval] def interpretStep(input: String, env: T): Try[Step] = Try {
    parseAll(step, input) match {
      case success @ Success(step, _) => 
        evaluateStep(step, env)
      case failure: NoSuccess => 
        sys.error(failure.toString)
    }
  }
  
  /**
    * Interprets an incoming feature.
    *
    * @param featureFile the feature file
    * @param metaFiles the meta files to load
    * @param tagFilters user provided tag filters (includes:(tag, true) and excludes:(tag, false))
    * @param env the environment context
    * @return the evaluated feature or nothing if the feature does not 
    *         satisfy specified tag filters
    */
  private[eval] def interpretFeature(featureFile: File, metaFiles: List[File], tagFilters: List[(Tag, Boolean)], env: T): List[FeatureSpec] = {
    parseAll(spec, Source.fromFile(featureFile).mkString) match {
      case success @ Success(featureSpec, _) =>
        if (featureFile.getName().endsWith(".meta")) {
          evaluateFeature(normalise(featureSpec, Some(featureFile)), Nil, env, SpecType.meta)
        } else {
          TagsFilter.filter(featureSpec, tagFilters) match {
            case Some(fspec) =>
              val metaResults = loadMeta(metaFiles, tagFilters, env)
              evaluateFeature(normalise(fspec, Some(featureFile)), metaResults, env, SpecType.feature)
            case None => 
              logger.info(s"Feature file skipped (does not satisfy tag filters): $featureFile")
              Nil
          }
        }
      case failure: NoSuccess =>
        sys.error(failure.toString)
    }
  }
  
  /**
    * Executes the given options.
    * 
    * @param options the command line options
    * @param optEnv optional environment context (None to have Gwen create an env context for each feature unit, 
    *               Some(env) to reuse an environment context for all, default is None)
    * @param executor implicit executor
    */
  def execute(options: GwenOptions, optEnv: Option[T] = None)(implicit executor: GwenExecutor[T] = new GwenExecutor(this)) = 
    executor.execute(options, optEnv)
  
  /**
    * Evaluates a given Gwen feature.
    * 
    * @param featureSpec the Gwen feature to evaluate
    * @param metaResults the evaluated meta results (Nil if featureSpec is a meta file)
    * @param env the environment context
    * @param specType the spec type
    * @return the evaluated Gwen feature
    */
  private def evaluateFeature(featureSpec: FeatureSpec, metaResults: List[FeatureSpec], env: T, specType: SpecType.Value): List[FeatureSpec] = {
    (if(SpecType.meta.equals(specType)) "Loading" else "Evaluating") tap {action =>
      logger.info("");
      featureSpec.featureFile.foreach { file =>
        logger.info(s"${action} ${specType} file: ${file}")
      }
      logger.info(s"${action} ${specType}: ${featureSpec.feature.name}")
    }
    val resultSpec = FeatureSpec(
      featureSpec.feature, 
      None, 
      featureSpec.scenarios.foldLeft(List[Scenario]()) {
        (acc: List[Scenario], scenario: Scenario) => 
          (EvalStatus(acc.map(_.evalStatus)) match {
            case Failed(_, _) =>
              if (GwenSettings.`gwen.feature.failfast`) {
                Scenario(
                  scenario, 
                  scenario.background.map(bg => Background(bg, bg.steps.map(step => Step(step, Skipped, step.attachments)))),
                  scenario.steps.map(step => Step(step, Skipped, step.attachments))
                )
              } else {
                  evaluateScenario(scenario, env)
              }
            case _ => 
              evaluateScenario(scenario, env)
          }) :: acc
      } reverse,
      featureSpec.featureFile 
    )
    (resultSpec::metaResults) tap { results =>
      logStatus(specType.toString, resultSpec.toString, resultSpec.evalStatus)
      resultSpec.featureFile foreach { file =>
        logger.info(s"${(if(SpecType.meta.equals(specType)) "Loaded" else "Evaluated")} ${specType} file: ${file}")
      }
      logger.debug(prettyPrint(resultSpec))
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
          Scenario(scenario, None, evaluateSteps(scenario.steps, env))
        case Some(background) => 
          Scenario(
            scenario,
            Some(background),
            background.evalStatus match {
              case Passed(_) => evaluateSteps(scenario.steps, env)
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
    Background(background, evaluateSteps(background.steps, env)) tap { bg =>
      logStatus("Background", bg.toString, bg.evalStatus)
    }
  }
  
  /**
    * Evaluates a list of steps.
    * 
    * @param steps the steps to evaluate
    * @param env the environment context
    * @return the list of evaluated steps
    */
  private def evaluateSteps(steps: List[Step], env: T): List[Step] = steps.foldLeft(List[Step]()) {
    (acc: List[Step], step: Step) => 
      (EvalStatus(acc.map(_.evalStatus)) match {
        case Failed(_, _) => Step(step, Skipped, step.attachments)
        case _ => evaluateStep(step, env)
      }) :: acc
  } reverse
  
  /**
    * Evaluates a given step.
    * 
    * @param step the step to evaluate
    * @param env the environment context
    * @return the evaluated step
    */
  private def evaluateStep(step: Step, env: T): Step = {
    def evaluateParsedStep(step: Step, env: T): Step = {
      logger.info(s"Evaluating Step: $step")
      val result = env.getStepDef(step.expression) match {
        case None =>
          doEvaluate(step, env) { step =>  
            Try {
              engine.evaluate(step, env)
              step
            }
          }
        case (Some(stepDef)) =>
          logger.info(s"Evaluating StepDef: ${stepDef.name}")
          val steps = evaluateSteps(stepDef.steps, env)
          Step(step, EvalStatus(steps.map(_.evalStatus)), steps.flatMap(_.attachments)) tap { step =>
            logger.info(s"StepDef evaluated: ${stepDef.name}")
          }
      }
      result tap { step =>
        logStatus("Step", step.toString, step.evalStatus)
      }
    }
    evaluateParsedStep(env.parse(step), env);
  }
  
  /**
    * Evaluates a step and captures the result.
    * 
    * @param step the step to evaluate
    * @param env the environment context
    * @param evalFunction the step evaluation function
    */
  private def doEvaluate(step: Step, env: T)(evalFunction: (Step) => Try[Step]): Step = {
    val start = System.nanoTime
    (evalFunction(step) match {
      case TrySuccess(step) => 
        Step(step, Passed(System.nanoTime - start), env.attachments)
      case TryFailure(error) =>
        val failure = Failed(System.nanoTime - start, new StepFailure(step, error))
        env.fail(failure)
        Step(step, failure, env.attachments)
    }) tap { step =>
      env.resetAttachments
    }
  }
  
  /**
    * Loads the meta.
    * 
    * @param metaFiles the meta files to load
    * @param tagFilters user provided tag filters (includes:(tag, true) and excludes:(tag, false))
    * @param env the environment context
    */
  private[eval] def loadMeta(metaFiles: List[File], tagFilters: List[(Tag, Boolean)], env: T): List[FeatureSpec] =
    metaFiles flatMap { metaFile =>
      interpretFeature(metaFile, Nil, tagFilters, env) tap { metas =>
        metas match {
          case meta::Nil =>
            meta.evalStatus match {
              case Passed(_) | Loaded =>
              case Failed(_, error) =>
                sys.error(s"Failed to load meta: $meta: ${error.getMessage()}")
              case _ =>
                sys.error(s"Failed to load meta: $meta")
            }
          case _ => Nil
        }
      } 
    }
  
  /**
    * Logs the evaluation status of the given node.
    * 
    * @param node the node to log the evaluation status of
    * @param name the name of the node that failed
    * @param status the evaluation status
    * @return the logged status message
    */
  private def logStatus(node: String, name: String, status: EvalStatus) = {
      logStatusMsg(s"$status $node: $name", status)
  }
  
  private def logStatusMsg(msg: String, status: EvalStatus) = status match {
    case Passed(_) | Loaded => 
      logger.info(msg)
    case Failed(_, _) => 
      logger.error(msg)
    case _ => 
      logger.warn(msg)
  }
  
}

/** Signals a step that failed to execute. */
class StepFailure(step: Step, cause: Throwable) extends RuntimeException(s"Failed step: ${step}: ${cause.getMessage()}", cause)

