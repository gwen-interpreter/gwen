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
  def normalise(spec: FeatureSpec, featureFile: Option[File] = None): FeatureSpec = 
    FeatureSpec(
      spec.feature, 
      None, 
      spec.background match {
        case None => spec.scenarios
        case Some(_) => 
          spec.scenarios map { scenario => 
            Scenario(scenario, if (scenario.isStepDef) { None } else { spec.background }, scenario.steps)
          }
      },
      featureFile
   )
  
}