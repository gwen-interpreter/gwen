/*
 * Copyright 2014-2024 Branko Juric, Brady Wood
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
import gwen.core.node._
import gwen.core.behavior.BehaviorRules
import gwen.core.data.DataRecord
import gwen.core.status.Pending

import scala.io.Source
import scala.util.chaining._

import java.io.File
import java.util.regex.Matcher

/**
  * Normalises a parsed feature spec by expanding scenarios and scenario outlines in preparation for evaluation.
  * If the feature has a background, then the background is copied to each expanded scenario and removed from the
  * top level. Positional information is preserved. The source feature file is also bound (if provided). If a data
  * file is provided, initialisation scenarios are created to initialise each row and the entire feature replicated
  * under each (inline data-driven approach)
  *
  * @author Branko Juric
  */
trait SpecNormaliser extends BehaviorRules {

  /**
    * Normalises a parsed feature.
    *
    * @param spec the feature spec
    * @param dataRecord optional feature level data record
    * @param options command line options
    */
  def normaliseSpec(spec: Spec, dataRecord: Option[DataRecord], options: GwenOptions): Spec = {
    val interpolator = DataRecord.interpolateLenient(dataRecord)
    val scenarios = noDuplicateStepDefs(spec.scenarios, spec.specFile)
    validate(spec.background, scenarios, spec.specType)
    val normalisedSpec = Spec(
      (
        dataRecord map { record =>
          spec.feature.copy(
            withName = s"${spec.feature.name}${if (spec.isMeta) "" else s" [${record.descriptor}]"}",
          )
        } getOrElse spec.feature
      ).interpolate(interpolator),
      None,
      dataRecord.map(expandDataScenarios(scenarios, _, spec.background, options)).getOrElse(expandScenarios(scenarios, spec.background, dataRecord, options)),
      spec.rules map { rule =>
        validate(rule.background, rule.scenarios, spec.specType)
        rule.copy(
          withBackground = None,
          withScenarios = expandScenarios(rule.scenarios, rule.background.orElse(spec.background), dataRecord, options)
        ).interpolate(interpolator)
      },
      Nil
    )
    if (!normalisedSpec.isMeta && normalisedSpec.steps(expanded = false).isEmpty) Errors.syntaxError(s"No steps found in feature${normalisedSpec.specFile.map(f => s" file: $f").getOrElse("")}")
    normaliseSteps(normalisedSpec, options)
  }

  private def validate(background: Option[Background], scenarios: List[Scenario], specType: SpecType): Unit = {
    background foreach { bg =>
      checkBackgroundRules(bg, specType)
      Step.validate(bg.steps)
    }
    scenarios foreach { s =>
      checkScenarioRules(s, specType)
      Step.validate(s.steps)
    }
  }

  private def expandDataScenarios(scenarios: List[Scenario], dataRecord: DataRecord, background: Option[Background], options: GwenOptions): List[Scenario] = {
    val dataBg = dataBackground(dataRecord.data, background, dataRecord.recordNo, dataRecord.totalRecs, Some(dataRecord.dataSource.dataFile), dataRecord.interpolateLenient)
    expandScenarios(scenarios, Some(dataBg), Some(dataRecord), options)
  }

  private def expandScenarios(scenarios: List[Scenario], background: Option[Background], dataRecord: Option[DataRecord], options: GwenOptions): List[Scenario] =
    scenarios.map { scenario =>
      if (scenario.isOutline) normaliseScenarioOutline(scenario, background, dataRecord, options)
      else expandScenario(scenario, background, dataRecord)
    }

  private def expandScenario(scenario: Scenario, background: Option[Background], dataRecord: Option[DataRecord]): Scenario = {
    val interpolator = DataRecord.interpolateLenient(dataRecord)
    background.map { _ =>
      scenario.copy(
        withBackground = if (scenario.isStepDef) {
          None
        } else {
          background map { bg => 
            bg.copy(
              withSteps = bg.steps.map(_.copy())
            ).interpolate(interpolator)
          }
        },
        withExamples = Nil
      ).interpolate(interpolator)
    } getOrElse scenario
  }


  def normaliseScenarioOutline(outline: Scenario, background: Option[Background], dataRecord: Option[DataRecord], options: GwenOptions): Scenario = {
    val interpolator = DataRecord.interpolateLenient(dataRecord)
    val normalisedOutline = outline.copy(
      withTags = filterParallelTags(outline.tags, options),
      withBackground = None,
      withExamples = outline.examples.zipWithIndex map { case (exs, tableIndex) =>
        val names = exs.table.head._2
        exs.copy(
          withTags = filterParallelTags(exs.tags, options),
          withScenarios = exs.table.tail.zipWithIndex.map { case ((rowLineNo, values), tableIndex) =>
            val params: List[(String, String)] = names zip values
            val normalisedBackground = {
              if (GwenSettings`gwen.auto.bind.tableData.outline.examples`) {
                Some(dataBackground(params, background, tableIndex + 1, exs.table.tail.size, exs.dataFile, interpolator))
              } else background
            }
            val outlineTags = outline.tags.filter(t => t.name != Annotations.StepDef.toString && !t.name.startsWith(Annotations.Examples.toString)).map(_.interpolate(interpolator))
            val tags = (if (exs.isParallel && !outline.isParallel) List(Tag(Annotations.Parallel)) else Nil) ++ outlineTags
            new Scenario(
              outline.sourceRef map { sref =>
                SourceRef(sref.file, rowLineNo)
              },
              filterParallelTags(tags, options),
              if (FeatureKeyword.isScenarioTemplate(outline.keyword)) FeatureKeyword.nameOf(FeatureKeyword.Example) else FeatureKeyword.nameOf(FeatureKeyword.Scenario),
              s"${resolveParams(interpolator.apply(outline.name), params)._1}${if (exs.name.length > 0) s" -- ${interpolator.apply(exs.name)}" else ""}",
              outline.description map { line =>
                val iLine = interpolator.apply(line)
                resolveParams(iLine, params)._1
              },
              normalisedBackground,
              outline.steps.map { s =>
                val (name, resolvedParams) = resolveParams(s.name, params)
                s.copy(
                  withName = name,
                  withTable = s.table map { case (line, record) => (line, record.map(cell => resolveParams(cell, params)._1)) },
                  withDocString = s.docString map { case (line, content, contentType) => (line, resolveParams(content, params)._1, contentType) },
                  withParams = resolvedParams
                )
              },
              Nil,
              params,
              Nil
            )
          }
        )
      }
    ).interpolate(interpolator)
    normaliseSteps(normalisedOutline, options)
  }

  private def filterParallelTags(tags: List[Tag], options: GwenOptions): List[Tag] = {
    if (options.parallel) tags.filter(_.name != Annotations.Parallel.toString)
    else tags
  }

  private def resolveParams(source: String, params: List[(String, String)]): (String, List[(String, String)]) = {
    def resolveParams(acc: List[(String, String)], source: String, params: List[(String, String)]): (String, List[(String, String)]) = {
      params match {
        case Nil => (source, acc)
        case head :: tail =>
          val (name, value) = head
          val param = if (source.contains(s"<$name>")) List(head) else Nil
          resolveParams(param ++ acc, source.replaceAll(s"<$name>", Matcher.quoteReplacement(value)), tail)
      }
    }
    resolveParams(Nil, source, params)
  }

  /**
    * Returns the given scenarios if they contain no step definitions
    * having the same name.
    *
    * @param scenarios the list of scenarios to conditionally return
    * @param specFile optional file from which scenarios were loaded
    */
  private def noDuplicateStepDefs(scenarios: List[Scenario], specFile: Option[File]): List[Scenario] = {
    scenarios tap { _ =>
      val duplicates = scenarios.filter(_.isStepDef).groupBy(_.name.replaceAll("<.+?>", "<?>")) filter { case (_, stepDefs) => stepDefs.size > 1 }
      val dupCount = duplicates.size
      if (dupCount > 0) {
        val msg = s"Ambiguous condition${if (dupCount > 1) "s" else ""}${specFile.map(f => s" in file $f").getOrElse("")}"
        Errors.ambiguousCaseError(s"$msg: ${duplicates.map { case (name, stepDefs) => s"StepDef '$name' defined ${stepDefs.size} times" }.mkString}")
      }
    }
  }

  private def dataBackground(data: List[(String, String)], background: Option[Background], recordNo: Int, totalRecords: Int, dataFile: Option[File], interpolator: String => String): Background = {
    val noData = background.map(_.isNoData).getOrElse(false)
    val dataTag = if (noData) Tag(Annotations.NoData) else Tag(Annotations.Data)
    val dataSteps = data.zipWithIndex map { case ((name, value), index) =>
      if (Source.fromString(name).getLines().toList.size > 1) {
        Errors.multilineDataFieldNameError(name, dataFile)
      }
      val keyword = if (index == 0 && !noData) StepKeyword.nameOf(StepKeyword.Given) else StepKeyword.nameOf(StepKeyword.And)
      if (Source.fromString(value).getLines().length > 1) 
        Step(None, keyword, s"$name is", Nil, None, Nil, Some((0, value, None)), Pending, Nil, Nil, List(dataTag), None, Nil)
      else {
        Step(None, keyword, s"""$name is "$value"""", Nil, None, Nil, None, Pending, Nil, Nil, List(dataTag), None, Nil)
      }
    }
    val description = dataFile map { file => 
      List(s"Input data file: ${file.getPath}")
    } getOrElse Nil
    val descriptor = {
      if (noData) {
        None 
      }
      else {
        Some(s"${if (dataFile.nonEmpty) "Input data" else "Data table"} record $recordNo of $totalRecords")
      }
    }
    background match {
      case Some(bg) =>
        val bgSteps = bg.steps match {
          case head :: tail if dataSteps.size > 0 && !noData =>
            if (StepKeyword.isGiven(head.keyword)) {
              head.copy(withKeyword = StepKeyword.nameOf(StepKeyword.And)) :: tail
            } else {
              bg.steps
            }
          case _ => bg.steps
        }
        Background(
          bg.sourceRef,
          bg.keyword,
          s"${bg.name}${descriptor.map(d => s" + $d").getOrElse("")}",
          bg.description ++ description,
          if (noData) bgSteps ++ dataSteps else dataSteps ++ bgSteps
        ).interpolate(interpolator)
      case None =>
        Background(
          None,
          FeatureKeyword.nameOf(FeatureKeyword.Background),
          descriptor.getOrElse("No data"),
          description,
          dataSteps.map(_.copy()))
    }
  }

  private def normaliseSteps(spec: Spec, options: GwenOptions): Spec = {
    spec.copy(
      withBackground = spec.background map { bg => 
        bg.copy(
          withSteps = normaliseSteps(bg.steps, options)
        )
      },
      withScenarios = spec.scenarios map { scenario => 
        normaliseSteps(scenario, options)
      },
      withRules = spec.rules map { rule => 
        rule.copy(
          withScenarios = rule.scenarios map { scenario =>
            normaliseSteps(scenario, options)
          }
        )
      }
    )
  }

  private def normaliseSteps(scenario: Scenario, options: GwenOptions): Scenario = {
    scenario.copy(
      withBackground = scenario.background map { bg => 
        bg.copy(
          withSteps = normaliseSteps(bg.steps, options)
        )
      },
      withSteps = normaliseSteps(scenario.steps, options),
      withExamples = scenario.examples map { examples => 
        examples.copy(
          withScenarios = examples.scenarios map { scenario => 
            normaliseSteps(scenario, options)
          }
        )
      }
    )
  }

  private def normaliseSteps(steps: List[Step], options: GwenOptions): List[Step] = {
    if (steps.exists(_.dryValues.nonEmpty)) {
      steps flatMap { step => 
        val dryValues = step.dryValues
        if (dryValues.nonEmpty) {
          if (options.dryRun) {
            if (step.dryValues.size > 1) {
              step.dryValues.zipWithIndex map { (dv, idx) =>
                step.copy(
                  withKeyword = if(idx > 0) StepKeyword.And.toString else step.keyword,
                  withDryValues = List(dv)
                )
              }
            } else {
              List (step)
            }
          } else {
            List(step.copy(withDryValues = Nil))
          }
        } else {
          List(step)
        }
      }
    } else {
      steps
    }
  }

}
