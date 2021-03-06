/*
 * Copyright 2014-2017 Branko Juric, Brady Wood
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

import java.io.File

/**
  * Normalises a parsed feature spec by expanding scenarios and scenario outlines in preparation for evaluation.
  * If the feature has a background, then the background is copied to each expanded scenario and removed from the
  * top level. Positional information is preserved. The source feature file is also bound (if provided). If a CSV
  * file is provided, initialisation scenarios are created to initialise each row and the entire feature replicated
  * under each (inline data-driven approach)
  * 
  * @author Branko Juric
  */
trait SpecNormaliser extends EvalRules {
  
  /**
    * Normalises a parsed feature.
    * 
    * @param spec the feature spec
    * @param specFile optional source feature file
    * @param dataRecord optional feature level data record
    */
  def normalise(spec: FeatureSpec, specFile: Option[File], dataRecord: Option[DataRecord]): FeatureSpec = {
    val scenarios = noDuplicateStepDefs(spec.scenarios, specFile)
    validate(spec.background, scenarios, spec.specType)
    val nspec = FeatureSpec(
      dataRecord map { record =>
        spec.feature.copy(
          withName = s"${spec.feature.name} [${record.recordNo}]")
      } getOrElse spec.feature,
      None, 
      dataRecord.map(expandDataScenarios(scenarios, _, spec.background)).getOrElse(expandScenarios(scenarios, spec.background)),
      spec.rules map { rule => 
        validate(rule.background, rule.scenarios, spec.specType)
        rule.copy(
          withBackground = None,
          withScenarios = expandScenarios(rule.scenarios, rule.background.orElse(spec.background)))
      },
      specFile,
      Nil
    )
    nspec.withNodePath(s"/${specFile.map(_.uri).getOrElse("")}")
  }

  private def validate(background: Option[Background], scenarios: List[Scenario], specType: SpecType.Value): Unit = {
    background foreach { bg => 
      checkBackgroundRules(bg, specType)
    }
    scenarios foreach { s =>
      checkScenarioRules(s, specType)
    }
  }
  
  private def expandDataScenarios(scenarios: List[Scenario], dataRecord: DataRecord, background: Option[Background]): List[Scenario] = {
    val steps = dataRecord.data.zipWithIndex map { case ((name, value), index) =>
      val keyword = if (index == 0) StepKeyword.nameOf(StepKeyword.Given) else StepKeyword.nameOf(StepKeyword.And)
      Step(None, keyword, s"""$name is "$value"""", Nil, None, Nil, None, Pending, List((name, value)), Nil)
    }
    val description = s"""@Data(file="${dataRecord.dataFilePath}", record=${dataRecord.recordNo})"""
    val dataBackground = background match {
      case Some(bg) =>
        val bgSteps = bg.steps match {
          case head :: tail if steps.size > 0 =>
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
          s"${bg.name} (plus input data)", 
          bg.description ++ List(description), 
          steps ++ bgSteps
        )
      case None =>
        Background(
          None,
          FeatureKeyword.nameOf(FeatureKeyword.Background), 
          "Input data", 
          List(description), 
          steps.map(_.copy()))
    }
    expandScenarios(scenarios, Some(dataBackground))
  }

  private def expandScenarios(scenarios: List[Scenario], background: Option[Background]): List[Scenario] =
    scenarios.map { scenario =>
      if (scenario.isOutline) expandScenarioOutline(scenario, background)
      else expandScenario(scenario, background)
    }
    
  private def expandScenario(scenario: Scenario, background: Option[Background]): Scenario = {
    background.map { _ => 
      scenario.copy(
        withBackground = if (scenario.isStepDef) {
          None 
        } else { 
          background.map(bg => bg.copy(withSteps = bg.steps.map(_.copy())))
        },
        withExamples = Nil
      )
    } getOrElse scenario
  }


  def expandScenarioOutline(outline: Scenario, background: Option[Background]): Scenario = {
    outline.copy(
      withBackground = None,
      withExamples = outline.examples.zipWithIndex map { case (exs, tableIndex) =>
        val names = exs.table.head._2
        exs.copy(
          withScenarios = exs.table.tail.zipWithIndex.map { case ((lineNo, values), rowIndex) =>
            val params: List[(String, String)] = names zip values
            new Scenario(
              exs.sourceRef map { sref =>
                SourceRef(sref.uri, Position(lineNo, sref.pos.column), None)
              },
              outline.tags.filter(t => t.name != ReservedTags.StepDef.toString && t.name != ReservedTags.Examples.toString),
              if (FeatureKeyword.isScenarioTemplate(outline.keyword)) FeatureKeyword.nameOf(FeatureKeyword.Example) else FeatureKeyword.nameOf(FeatureKeyword.Scenario),
              s"${Formatting.resolveParams(outline.name, params)._1}${if (exs.name.size > 0) s" -- ${exs.name}" else ""}",
              outline.description.map(line => Formatting.resolveParams(line, params)._1),
              if (outline.isStepDef) None 
              else background.map(bg => bg.copy(withSteps = bg.steps.map(_.copy()))), 
              outline.steps.map { s =>
                val (name, resolvedParams) = Formatting.resolveParams(s.name, params)
                s.copy(
                  withName = name,
                  withTable = s.table map { case (line, record) => (line, record.map(cell => Formatting.resolveParams(cell, params)._1)) },
                  withDocString = s.docString map { case (line, content, contentType) => (line, Formatting.resolveParams(content, params)._1, contentType) },
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
  
}