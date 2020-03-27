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

import java.io.File

import gwen.Predefs.{Formatting, Kestrel}
import gwen.dsl._
import gwen.errors._

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
    val scenarios = noDuplicateStepDefs(spec.scenarios, specFile) map {scenario =>
      if (scenario.isStepDef && specFile.exists(_.getName.endsWith(".meta"))) {
        Scenario(scenario, specFile)
      } else {
        scenario
      }
    }
    validate(specFile, spec.background, scenarios)
    FeatureSpec(
      dataRecord map { record =>
        Feature(
          spec.feature.language,
          spec.feature.tags,
          spec.feature.keyword,
          s"${spec.feature.name}, [${record.recordNo}] ${record.data.head match {case (name, value) => s"$name=$value${if (record.data.size > 1) ".." else ""}"}}",
          spec.feature.description)
      } getOrElse spec.feature,
      None, 
      dataRecord.map(expandDataScenarios(scenarios, _, spec.background)).getOrElse(expandScenarios(scenarios, spec.background)),
      spec.rules map { rule => 
        validate(specFile, rule.background, rule.scenarios)
        Rule(
          rule.keyword,
          rule.name,
          rule.description,
          None,
          expandScenarios(rule.scenarios, rule.background.orElse(spec.background))
        )
      },
      specFile
    )
  }

  private def validate(specFile: Option[File], background: Option[Background], scenarios: List[Scenario]) {
    background foreach { bg => 
      checkBackgroundRules(bg, specFile)
    }
    scenarios foreach { s =>
      checkScenarioRules(s, specFile)
    }
  }
  
  private def expandDataScenarios(scenarios: List[Scenario], dataRecord: DataRecord, background: Option[Background]): List[Scenario] = {
    val steps = dataRecord.data.zipWithIndex map { case ((name, value), index) =>
      val keyword = if (index == 0) StepKeyword.nameOf(StepKeyword.Given) else StepKeyword.nameOf(StepKeyword.And)
      Step(keyword, s"""$name is "$value"""")
    }
    val description = s"""@Data(file="${dataRecord.dataFilePath}", record=${dataRecord.recordNo})"""
    val dataBackground = background match {
      case Some(bg) =>
        val bgSteps = bg.steps match {
          case head :: tail if steps.size > 0 =>
            if (StepKeyword.isGiven(head.keyword)) {
              Step(head, StepKeyword.nameOf(StepKeyword.And), head.expression) :: tail
            } else {
              bg.steps
            }
          case _ => bg.steps
        }
        Background(
          bg.keyword,
          s"${bg.name} (plus input data)", 
          bg.description ++ List(description), 
          steps ++ bgSteps
        ) tap { b => 
          b.pos = bg.pos
        }
      case None =>
        Background(
          FeatureKeyword.nameOf(FeatureKeyword.Background), 
          "Input data", 
          List(description), 
          steps)
    }
    expandScenarios(scenarios, Some(dataBackground))
  }

  private def expandScenarios(scenarios: List[Scenario], background: Option[Background]): List[Scenario] =
    scenarios.map { scenario =>
      if (scenario.isOutline) expandScenarioOutline(scenario, background)
      else expandScenario(scenario, background)
    }
    
  private def expandScenario(scenario: Scenario, background: Option[Background]): Scenario =
    background.map(_ => Scenario(scenario, if (scenario.isStepDef) None else background, scenario.steps, Nil)).getOrElse(scenario)


  def expandScenarioOutline(outline: Scenario, background: Option[Background]): Scenario =
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
              outline.tags.filter(t => t != Tag.StepDefTag && !t.name.startsWith("Examples")),
              if (FeatureKeyword.isScenarioTemplate(outline.keyword)) FeatureKeyword.nameOf(FeatureKeyword.Example) else FeatureKeyword.nameOf(FeatureKeyword.Scenario),
              s"${Formatting.resolveParams(outline.name, params)} -- Example ${index + 1}.${subIndex + 1} ${exs.name}",
              outline.description.map(line => Formatting.resolveParams(line, params)),
              if (outline.isStepDef) None else background,
              outline.steps.map { s =>
                new Step(
                  s.keyword,
                  Formatting.resolveParams(s.name, params),
                  s.status,
                  s.attachments,
                  s.stepDef,
                  s.table map { case (line, record) => (line, record.map(cell => Formatting.resolveParams(cell, params))) },
                  s.docString map { case (line, content, contentType) => (line, Formatting.resolveParams(content, params), contentType) }
                ) tap { step => step.pos = s.pos }
              },
              isOutline = false,
              Nil,
              None
            ) tap { s => s.pos = outline.pos }
          })
      })
   
  /**
    * Returns the given scenarios if they contain no step definitions 
    * having the same name.
    * 
    * @param scenarios the list of scenarios to conditionally return
    * @param specFile optional file from which scenarios were loaded
    */
  private def noDuplicateStepDefs(scenarios: List[Scenario], specFile: Option[File] = None): List[Scenario] = scenarios tap { scenarios =>
    val duplicates = scenarios.filter(_.isStepDef).groupBy(_.name.replaceAll("<.+?>", "<?>")) filter { case (_, stepDefs) => stepDefs.size > 1 }
    val dupCount = duplicates.size
    if (dupCount > 0) {
      val msg = s"Ambiguous condition${if (dupCount > 1) "s" else ""}${specFile.map(f => s" in file $f").getOrElse("")}" 
      ambiguousCaseError(s"$msg: ${duplicates.map { case (name, stepDefs) => s"StepDef '$name' defined ${stepDefs.size} times" }.mkString}")
    }
  }
  
}