/*
 * Copyright 2020-2021 Branko Juric, Brady Wood
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

package gwen.core

import gwen.core.node._
import gwen.core.node.gherkin._
import gwen.core.status._

import java.io.File

trait TestModel {

  object Feature {
    def apply(file: Option[File], name: String, description: List[String]): Feature = 
      new Feature("en", file.map(f => SourceRef(file, 1)), Nil, FeatureKeyword.Feature.toString, name, description)
  }

  object Background {
    def apply(name: String, description: List[String], steps: List[Step]): Background =
      new Background(None, FeatureKeyword.Background.toString, name, description, steps)
  }

  object Scenario {
    def apply(tags: List[Tag], name: String, description: List[String], background: Option[Background], steps: List[Step]): Scenario =
      new Scenario(None, tags.distinct, FeatureKeyword.Scenario.toString, name, description, background, steps, Nil, Nil, Nil)
    def apply(tags: List[Tag], name: String, description: List[String], background: Option[Background], steps: List[Step], examples: List[Examples]): Scenario =
      new Scenario(None, tags.distinct, FeatureKeyword.Scenario.toString, name, description, background, steps, examples, Nil, Nil)
  }

  object Step {
    def apply(name: String, docString: Option[String]): Step = {
      new Step(None, StepKeyword.Given.toString, name, Nil, None, Nil, docString.map(ds => (1, ds, None)), Pending, Nil, Nil, Nil, None, Nil)
    }
    def apply(keyword: String, name: String): Step = {
      new Step(None, keyword, name, Nil, None, Nil, None, Pending, Nil, Nil, Nil, None, Nil)
    }
    def apply(keyword: String, name: String, evalStatus: EvalStatus): Step = {
      new Step(None, keyword, name, Nil, None, Nil, None, evalStatus, Nil, Nil, Nil, None, Nil)
    }
    def apply(line: Int, keyword: String, name: String): Step = {
      new Step(Some(SourceRef(None, line)), keyword, name, Nil, None, Nil, None, Pending, Nil, Nil, Nil, None, Nil)
    }
    def apply(step: Step, sourceRef: SourceRef): Step = {
      new Step(Some(sourceRef), step.keyword, step.name, step.attachments, step.stepDef, step.table, step.docString, Pending, Nil, Nil, Nil, None, Nil)
    }
    def apply(step: Step, keyword: String, name: String, evalStatus: EvalStatus): Step = {
      new Step(step.sourceRef, keyword, name, step.attachments, step.stepDef, step.table, step.docString, evalStatus, Nil, Nil, Nil, None, Nil)
    }
    def apply(step: Step, stepDef: Scenario, attachments: List[(String, File)]): Step = {
      new Step(step.sourceRef, step.keyword, step.name, attachments, Some(stepDef), step.table, step.docString, stepDef.evalStatus, Nil, Nil, Nil, None, Nil)
    }
    def apply(step: Step, evalStatus: EvalStatus): Step = {
      new Step(step.sourceRef, step.keyword, step.name, step.attachments, step.stepDef, step.table, step.docString, evalStatus, Nil, Nil, Nil, None, Nil)
    }
  }
  
}
