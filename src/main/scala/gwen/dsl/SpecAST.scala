/*
 * Copyright 2014 Branko Juric, Brady Wood
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

import scala.util.parsing.input.Positional
import java.io.File

/**
 * Base trait for capturing a feature spec in an abstract syntax tree.  
 * An spec node is the raw output produced by the [[SpecParser]].
 *
 * @author Branko Juric
 */
trait SpecNode {
  /**
   * Returns the evaluation status of this node.
   */
  lazy val evalStatus: EvalStatus = Pending
  
}

/**
 * Abstract syntax tree of a successfully parsed feature.
 * The [[SpecParser]] parses all plain text features into a tree of
 * this type.  The [[gwen.eval.GwenInterpreter interpreter]] normalises 
 * the tree before passing it down to the 
 * [[gwen.eval.EvalEngine evaluation engine]] and lower layers for 
 * processing.
 *
 * @param feature
 * 		the feature
 * @param background
 * 		optional background
 * @param scenarios
 * 		list of scenarios
 * @param featureFile
 * 		optional source feature file
 * @param metaSpecs
 * 		optional list of meta specs
 *
 * @author Branko Juric
 */

case class FeatureSpec(
  feature: Feature, 
  background: Option[Background], 
  scenarios: List[Scenario],
  featureFile: Option[File] = None,
  metaSpecs: List[FeatureSpec] = Nil) extends SpecNode {
  
  /**
   * Gets the list of all steps contained in the feature spec. The list includes
   * all meta steps (if any) and all scenario steps (including any background 
   * steps).
   * 
   * @return a list containing all the steps (or an empty list if none exist)
   */
  def steps: List[Step] = scenarios.flatMap(_.allSteps)
  
  /**
   * Returns the evaluation status of this feature spec.
   */
  override lazy val evalStatus: EvalStatus = {
    val specStatus = EvalStatus(steps.map(_.evalStatus))
    metaSpecs match {
      case Nil => specStatus
      case _ =>
        val totalStatus = EvalStatus((metaSpecs.flatMap(_.steps) ++ steps).map(_.evalStatus))
        specStatus match {
          case Passed(_) => Passed(totalStatus.duration)
          case _ => totalStatus
        }
    }
  }
  
  override def toString = feature.name 
}
object FeatureSpec {
  def apply(spec: FeatureSpec, featureFile: File, metaSpecs: List[FeatureSpec]) =
    new FeatureSpec(
      spec.feature,
      spec.background,
      spec.scenarios,
      Some(featureFile),
      metaSpecs)
}

/**
 * Captures a gherkin feature node.
 *
 * @param tags
 * 			set of tags
 * @param name
 * 			the feature name
 * @param narrative
 * 			optional narrative (As a.. I want.. So that..)
 *
 * @author Branko Juric
 */
case class Feature(tags: Set[Tag], name: String, narrative: List[String]) extends SpecNode with Positional {
  override def toString = name
}
object Feature {
  def apply(name: String) = new Feature(Set(), name, Nil)
  def apply(name: String, narrative: List[String]) = new Feature(Set(), name, narrative)
}

/**
 * Captures a gherkin background node.
 *
 * @param name
 * 			the background name
 * @param steps
 * 			list of background steps
 *
 * @author Branko Juric
 */
case class Background(name: String, steps: List[Step]) extends SpecNode with Positional {
  
  /**
   * Returns the evaluation status of this background.
   */
  override lazy val evalStatus: EvalStatus = EvalStatus(steps.map(_.evalStatus))
  
  override def toString = name
  
}

/**
 * Captures a gherkin scenario.
 *
 * @param tags
 * 			set of tags
 * @param name
 * 			the scenario name
 * @param background
 * 		optional background
 * @param steps
 * 			list of scenario steps
 *
 * @author Branko Juric
 */
case class Scenario(tags: Set[Tag], name: String, background: Option[Background], steps: List[Step]) extends SpecNode with Positional {
  
  /**
   * Returns a list containing all the background steps (if any) followed by 
   * all the scenario steps.
   */
  def allSteps = background.map(_.steps).getOrElse(Nil) ++ steps
  
  def isStepDef = tags.contains(Tag.StepDefTag)
  
  /**
   * Returns the evaluation status of this scenario.
   */
  override lazy val evalStatus: EvalStatus = EvalStatus(allSteps.map(_.evalStatus))

  
  override def toString = name
  
}
object Scenario {
  def apply(tags: Set[Tag], name: String, steps: List[Step]) = new Scenario(tags, name, None, steps)
  def apply(name: String, background: Option[Background], steps: List[Step]) = new Scenario(Set(), name, background, steps)
}

/**
 * Captures a gherkin tag.
 *
 * @param name
 * 			name the tag name
 *    
 * @author Branko Juric
 */
case class Tag(name: String) extends SpecNode with Positional {
  
  /**
   * Returns a string representation of this tag.
   */
  override def toString = s"@$name"
  
}
object Tag {
  
  val StepDefTag = Tag("StepDef")
  private val Regex = """~?@(\w+)""".r
  
  import scala.language.implicitConversions
  implicit def string2Tag(value: String) = value match {
    case Regex(name) => Tag(name)
    case _ => sys.error(s"invalid tag: ${value}")
  }
  
}

/**
 * Captures a gherkin step.
 *
 * @param keyword
 * 			keyword identifier (Given, When, Then, etc..)
 * @param expression
 * 			free format step expression line (that is: the text following the step keyword)
 * @param evalStatus
 * 			optional evaluation status (default = Pending)
 * @param attachments
 * 			file attachments as name-file pairs (default = Nil)
 *    
 * @author Branko Juric
 */
case class Step(
    keyword: StepKeyword.Value, 
    expression: String, 
    status: EvalStatus = Pending, 
    attachments: List[(String, File)] = Nil) extends SpecNode with Positional {
  
  /**
   * Returns the evaluation status of this step definition.
   */
  override lazy val evalStatus: EvalStatus = status
  
  /**
   * Returns a string representation of this step.
   */
  override def toString = s"$keyword $expression"
  
}
