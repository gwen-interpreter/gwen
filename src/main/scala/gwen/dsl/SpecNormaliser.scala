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

package gwen.dsl

import java.io.File

import gwen.Predefs.{Formatting, Kestrel}
import gwen.errors._
import gwen.eval.DataRecord

/**
  * Normalises a parsed feature spec by expanding scenarios and scenario outlines in preparation for evaluation.
  * If the feature has a background, then the background is copied to each expanded scenario and removed from the
  * top level. Positional information is preserved. The source feature file is also bound (if provided). If a CSV
  * file is provided, initialisation scenarios are created to initialise each row and the entire feature replicated
  * under each (inline data-driven approach)
  * 
  * @author Branko Juric
  */
trait SpecNormaliser {
  
  /**
    * Normalises a parsed feature.
    * 
    * @param spec the feature spec
    * @param featureFile optional source feature file
    * @param dataRecord optional feature level data record
    */
  def normalise(spec: FeatureSpec, featureFile: Option[File], dataRecord: Option[DataRecord]): FeatureSpec = {
    val scenarios = noDuplicateStepDefs(spec.scenarios, featureFile) map {scenario =>
      if (scenario.isStepDef && featureFile.exists(_.getName.endsWith(".meta"))) {
        Scenario(scenario, featureFile)
      } else {
        scenario
      }
    }
    FeatureSpec(
      dataRecord.map(record => Feature(spec.feature.tags, s"${spec.feature.name}, [${record.recordNo}] ${record.data.head match {case (name, value) => s"$name=$value${if (record.data.size > 1) ".." else ""}"}}", spec.feature.description)).getOrElse(spec.feature), 
      None, 
      dataRecord.map(expandDataScenarios(scenarios, _, spec.background)).getOrElse(expandScenarios(scenarios, spec.background)),
      featureFile
    )
  }
  
  private def expandDataScenarios(scenarios: List[Scenario], dataRecord: DataRecord, background: Option[Background]): List[Scenario] = {
    val steps = dataRecord.data.zipWithIndex map { case ((name, value), index) =>
      val keyword = if (index == 0) StepKeyword.Given else StepKeyword.And 
      Step(Position(0, 0), keyword, s"""$name is "$value"""")
    }
    val tags = List(Tag(s"""Data(file="${dataRecord.dataFilePath}", record=${dataRecord.recordNo})"""))
    Scenario(tags, s"Bind data attributes", Nil, None, steps, Nil, None) :: expandScenarios(scenarios, background)
  }

  private def expandScenarios(scenarios: List[Scenario], background: Option[Background]): List[Scenario] =
    scenarios.map { scenario =>
      if (scenario.isOutline) expandScenarioOutline(scenario, background)
      else expandScenario(scenario, background)
    }
    
  private def expandScenario(scenario: Scenario, background: Option[Background]): Scenario =
    background.map(_ => Scenario(scenario, if (scenario.isStepDef) None else background, scenario.steps, Nil)).getOrElse(scenario)


  private def expandScenarioOutline(outline: Scenario, background: Option[Background]): Scenario =
    Scenario(
      outline,
      None,
      outline.steps,
      outline.examples.zipWithIndex map { case (exs, index) =>
        val names = exs.table.head._2
        Examples(
          exs,
          exs.table.tail.zipWithIndex.map { case ((_, values), subIndex) =>
            val params: List[(String, String)] = names zip values
            new Scenario(
              outline.tags.filter(_ != Tag.StepDefTag),
              s"${Formatting.resolveParams(outline.name, params)} -- Example ${index + 1}.${subIndex + 1} ${exs.name}",
              outline.description.map(line => Formatting.resolveParams(line, params)),
              if (outline.isStepDef) None else background,
              outline.steps.map { s =>
                new Step(s.pos, s.keyword, Formatting.resolveParams(s.expression, params), s.status, s.attachments, s.stepDef)
              },
              Nil,
              None)
          })
      })
   
  /**
    * Returns the given scenarios if they contain no step definitions 
    * having the same name.
    * 
    * @param scenarios the list of scenarios to conditionally return
    * @param featureFile optional file from which scenarios were loaded
    */
  private def noDuplicateStepDefs(scenarios: List[Scenario], featureFile: Option[File] = None): List[Scenario] = scenarios tap { scenarios =>
    val duplicates = scenarios.filter(_.isStepDef).groupBy(_.name.replaceAll("<.+?>", "<?>")) filter { case (_, stepDefs) => stepDefs.size > 1 }
    val dupCount = duplicates.size
    if (dupCount > 0) {
      val msg = s"Ambiguous condition${if (dupCount > 1) "s" else ""}${featureFile.map(f => s" in file $f").getOrElse("")}" 
      ambiguousCaseError(s"$msg: ${duplicates.map { case (name, stepDefs) => s"StepDef '$name' defined ${stepDefs.size} times" }.mkString}")
    }
  }
  
}