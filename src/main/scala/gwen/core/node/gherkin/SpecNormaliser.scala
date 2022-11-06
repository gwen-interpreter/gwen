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
import gwen.core.node._
import gwen.core.behavior.BehaviorRules
import gwen.core.state.DataRecord
import gwen.core.status.Pending

import scala.util.chaining._

import java.io.File
import java.util.regex.Matcher

/**
  * Normalises a parsed feature spec by expanding scenarios and scenario outlines in preparation for evaluation.
  * If the feature has a background, then the background is copied to each expanded scenario and removed from the
  * top level. Positional information is preserved. The source feature file is also bound (if provided). If a CSV
  * file is provided, initialisation scenarios are created to initialise each row and the entire feature replicated
  * under each (inline data-driven approach)
  *
  * @author Branko Juric
  */
trait SpecNormaliser extends BehaviorRules with Interpolator {

  /**
    * Normalises a parsed feature.
    *
    * @param spec the feature spec
    * @param dataRecord optional feature level data record
    */
  def normaliseSpec(spec: Spec, dataRecord: Option[DataRecord]): Spec = {
    val interpolator: String => Option[String] = dataRecord.map(_.interpolator).getOrElse(String => None)
    val scenarios = noDuplicateStepDefs(spec.scenarios, spec.specFile)
    validate(spec.background, scenarios, spec.specType)
    val normalisedSpec = Spec(
      dataRecord map { record =>
        spec.feature.copy(
          withTags = spec.feature.tags map { tag => 
            Tag(tag.sourceRef, interpolateStringLenient(tag.toString) { interpolator })
          },
          withName = s"${interpolateStringLenient(spec.feature.name) { interpolator }}${if (spec.isMeta) "" else s" [${record.descriptor}]"}",
          withDescription = spec.feature.description map { line => 
            interpolateStringLenient(line) { interpolator }
          }
        )
      } getOrElse spec.feature,
      None,
      dataRecord.map(expandDataScenarios(scenarios, _, spec.background)).getOrElse(expandScenarios(scenarios, spec.background, dataRecord)),
      spec.rules map { rule =>
        validate(rule.background, rule.scenarios, spec.specType)
        rule.copy(
          withName = interpolateStringLenient(rule.name) { interpolator },
          withDescription = rule.description map { line => 
            interpolateStringLenient(line) { interpolator }
          },
          withBackground = None,
          withScenarios = expandScenarios(rule.scenarios, rule.background.orElse(spec.background), dataRecord)
        )
      },
      Nil
    )
    if (!normalisedSpec.isMeta && normalisedSpec.steps.isEmpty) Errors.syntaxError(s"No steps found in feature${normalisedSpec.specFile.map(f => s" file: $f").getOrElse("")}")
    normalisedSpec
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

  private def expandDataScenarios(scenarios: List[Scenario], dataRecord: DataRecord, background: Option[Background]): List[Scenario] = {
    val interpolator = dataRecord.interpolator
    val dataBg = dataBackground(dataRecord.data, background, dataRecord.recordNo, dataRecord.totalRecs, Some(dataRecord.dataFile), interpolator)
    expandScenarios(scenarios, Some(dataBg), Some(dataRecord))
  }

  private def expandScenarios(scenarios: List[Scenario], background: Option[Background], dataRecord: Option[DataRecord]): List[Scenario] =
    scenarios.map { scenario =>
      if (scenario.isOutline) normaliseScenarioOutline(scenario, background, dataRecord)
      else expandScenario(scenario, background, dataRecord)
    }

  private def expandScenario(scenario: Scenario, background: Option[Background], dataRecord: Option[DataRecord]): Scenario = {
    val interpolator: String => Option[String] = dataRecord.map(_.interpolator).getOrElse(String => None)
    background.map { _ =>
      scenario.copy(
        withTags = scenario.tags map { tag => 
          Tag(tag.sourceRef, interpolateStringLenient(tag.toString) { interpolator })
        },
        withName = interpolateStringLenient(scenario.name) { interpolator },
        withDescription = scenario.description map { line => 
          interpolateStringLenient(line) { interpolator }
        },
        withBackground = if (scenario.isStepDef) {
          None
        } else {
          background map { bg => 
            bg.copy(
              withName = interpolateStringLenient(bg.name) { interpolator },
              withDescription = bg.description map { line => 
                interpolateStringLenient(line) { interpolator }
              },
              withSteps = bg.steps.map(_.copy())
            )
          }
        },
        withExamples = Nil
      )
    } getOrElse scenario
  }


  def normaliseScenarioOutline(outline: Scenario, background: Option[Background], dataRecord: Option[DataRecord]): Scenario = {
    val interpolator: String => Option[String] = dataRecord.map(_.interpolator) getOrElse { String => None }
    outline.copy(
      withTags = outline.tags map { tag => 
        Tag(tag.sourceRef, interpolateStringLenient(tag.toString) { interpolator })
      },
      withName = interpolateStringLenient(outline.name) { interpolator },
      withDescription = outline.description map { line => 
        interpolateStringLenient(line) { interpolator }
      },
      withBackground = None,
      withExamples = outline.examples.zipWithIndex map { case (exs, tableIndex) =>
        val names = exs.table.head._2
        exs.copy(
          withTags = exs.tags map { tag => 
            Tag(tag.sourceRef, interpolateStringLenient(tag.toString) { interpolator })
          },
          withName = interpolateStringLenient(exs.name) { interpolator },
          withDescription = exs.description map { line => 
            interpolateStringLenient(line) { interpolator }
          },
          withScenarios = exs.table.tail.zipWithIndex.map { case ((rowLineNo, values), tableIndex) =>
            val params: List[(String, String)] = names zip values
            val normalisedBackground = {
              if (GwenSettings`gwen.auto.bind.tableData.outline.examples`) {
                Some(dataBackground(params, background, tableIndex + 1, exs.table.tail.size, exs.dataFile, interpolator))
              } else background
            }
            new Scenario(
              outline.sourceRef map { sref =>
                SourceRef(sref.file, rowLineNo)
              },
              outline.tags.filter(t => t.name != Annotations.StepDef.toString && !t.name.startsWith(Annotations.Examples.toString)) map { tag => 
                Tag(tag.sourceRef, interpolateStringLenient(tag.toString) { interpolator })
              },
              if (FeatureKeyword.isScenarioTemplate(outline.keyword)) FeatureKeyword.nameOf(FeatureKeyword.Example) else FeatureKeyword.nameOf(FeatureKeyword.Scenario),
              s"${resolveParams(interpolateStringLenient(outline.name) { interpolator }, params)._1}${if (exs.name.length > 0) s" -- ${interpolateStringLenient(exs.name) { interpolator }}" else ""}",
              outline.description map { line =>
                val iLine = interpolateStringLenient(line) { interpolator }
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
    )
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

  private def dataBackground(data: List[(String, String)], background: Option[Background], recordNo: Int, totalRecords: Int, dataFile: Option[File], interpolator: String => Option[String]): Background = {
    val noData = background.map(_.isNoData).getOrElse(false)
    val dataTag = if (noData) Tag(Annotations.NoData) else Tag(Annotations.Data)
    val dataSteps = (data map { case (name, value) => 
        (name, if (GwenSettings.`gwen.auto.trim.data.csv`) value.trim else value)
      }).zipWithIndex map { case ((name, value), index) =>
      val keyword = if (index == 0) StepKeyword.nameOf(StepKeyword.Given) else StepKeyword.nameOf(StepKeyword.And)
      Step(None, keyword, s"""$name is "$value"""", Nil, None, Nil, None, Pending, Nil, Nil, List(dataTag), None)
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
          case head :: tail if dataSteps.size > 0 =>
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
          s"${interpolateStringLenient(bg.name) { interpolator }}${descriptor.map(d => s" + $d").getOrElse("")}",
          (bg.description map { line =>
            interpolateStringLenient(line) { interpolator }
          }) ++ description,
          dataSteps ++ bgSteps
        )
      case None =>
        Background(
          None,
          FeatureKeyword.nameOf(FeatureKeyword.Background),
          descriptor.getOrElse("No data"),
          description,
          dataSteps.map(_.copy()))
    }
  }

}
