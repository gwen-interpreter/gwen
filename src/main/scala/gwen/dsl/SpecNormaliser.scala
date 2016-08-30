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

package gwen.dsl

import java.io.File
import gwen.Predefs.Kestrel
import gwen.errors._
import com.github.tototoshi.csv.CSVReader
import gwen.eval.DataRecord

/**
  * Normalises a parsed feature spec in preparation for 
  * [[gwen.eval.EvalEngine evaluation]].
  * 
  * @author Branko Juric
  */
trait SpecNormaliser {
  
  /**
    * Normalises a given [[gwen.dsl.FeatureSpec]].  If the feature has a 
    * background, then the  background is copied to each contained scenario and 
    * removed from the top level.  Positional information is preserved. The 
    * source feature file is also bound (if provided). If a CSV file is provided, 
    * initialisation scenarios are created to initialse each row and the 
    * entire feature replicated under each (inline data-driven approach). 
    * 
    * @param spec the feature spec
    * @param featureFile optional source feature file
    * @param dataRecord optional feature level data record
    */
  def normalise(spec: FeatureSpec, featureFile: Option[File], dataRecord: Option[DataRecord]): FeatureSpec = {
    val scenarios = noDuplicateStepDefs(spec.scenarios, featureFile) map {scenario =>
      if (scenario.isStepDef && featureFile.map(_.getName().endsWith(".meta")).getOrElse(false)) {
        Scenario(scenario, featureFile)
      } else {
        scenario
      }
    }
    FeatureSpec(
      dataRecord.map(record => Feature(spec.feature.tags, s"${spec.feature.name}, [${record.recordNo}] ${record.data.head match {case (name, value) => s"$name=$value${if (record.data.size > 1) ".." else ""}"}}", spec.feature.description)).getOrElse(spec.feature), 
      None, 
      dataRecord.map(dataScenarios(spec, scenarios, _)).getOrElse(featureScenarios(spec, scenarios)),
      featureFile
    )
  }
  
  private def dataScenarios(spec: FeatureSpec, scenarios: List[Scenario], dataRecord: DataRecord): List[Scenario] = {
    val steps = dataRecord.data.zipWithIndex map { case ((name, value), index) =>
      val keyword = if (index == 0) StepKeyword.Given else StepKeyword.And 
      Step(Position(0, 0), keyword, s"""$name is "$value"""")
    }
    val tags = List(Tag(s"""Data(file="${dataRecord.dataFilePath}", record=${dataRecord.recordNo})"""))
    Scenario(tags, s"Bind data attributes", Nil, None, steps.toList, None) :: featureScenarios(spec, scenarios)
  }
    
  private def featureScenarios(spec: FeatureSpec, scenarios: List[Scenario]): List[Scenario] = spec.background match {
    case None => scenarios
    case Some(_) => 
      scenarios map { scenario => 
        Scenario(
          scenario, 
          if (scenario.isStepDef) None else spec.background, 
          scenario.steps)
      }
  }  
   
  /**
    * Returns the given scenarios if they contain no step definitions 
    * having the same name.
    * 
    * @param scenarios the list of scenarios to conditionally return
    * @param featureFile optional file from which scenarios were loaded
    * @throws gwen.errors.AmiguousCaseException if more than one step def 
    *         with the same name is found  
    */
  private def noDuplicateStepDefs(scenarios: List[Scenario], featureFile: Option[File] = None): List[Scenario] = scenarios tap { scenarios =>
    val duplicates = scenarios.filter(_.isStepDef).groupBy(_.name.replaceAll("<.+?>", "<?>")) filter { case (_, stepDefs) => stepDefs.size > 1 }
    val dupCount = duplicates.size
    if (dupCount > 0) {
      val msg = s"Ambiguous condition${if (dupCount > 1) "s" else ""}${featureFile.map(f => s" in file $f").getOrElse("")}" 
      ambiguousCaseError(s"$msg: ${(duplicates.map { case (name, stepDefs) => s"StepDef '$name' defined ${stepDefs.size} times" }).mkString}")
    }
  }
  
}