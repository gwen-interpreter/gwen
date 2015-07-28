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
    * source feature file is also bound (if provided).
    * 
    * @param spec the feature spec
    * @param featureFile optional source feature file
    */
  def normalise(spec: FeatureSpec, featureFile: Option[File] = None): FeatureSpec = {
    val scenarios = noDuplicateStepDefs(spec.scenarios, featureFile)
    FeatureSpec(
      spec.feature, 
      None, 
      spec.background match {
        case None => scenarios
        case Some(_) => 
          scenarios map { scenario => 
            Scenario(scenario, if (scenario.isStepDef) { None } else { spec.background }, scenario.steps)
          }
      },
      featureFile
    )
  }
   
   private def noDuplicateStepDefs(scenarios: List[Scenario], featureFile: Option[File] = None): List[Scenario] = scenarios tap { scenarios =>
     val duplicates = scenarios.filter(_.isStepDef).groupBy(_.name) filter { case (_, stepDefs) => stepDefs.size > 1 }
     val dupCount = duplicates.size
     if (dupCount > 0) {
       val msg = s"Ambiguity error${if (dupCount > 1) "s" else ""}${featureFile.map(f => s" in file $f").getOrElse("")}" 
       ambiguityError(s"$msg: ${(duplicates.map { case (name, stepDefs) => s"StepDef '$name' defined ${stepDefs.size} times" }).mkString}")
     }
   }
  
}