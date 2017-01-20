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
import scala.collection.JavaConverters._

/**
  * Base trait for capturing a feature spec in an abstract syntax tree.  
  * An spec node is the raw output produced by the [[GherkinParser]].
  *
  * @author Branko Juric
  */
trait SpecNode {
  /** Returns the evaluation status of this node. */
  lazy val evalStatus: EvalStatus = Pending
  
}

/** Reperesents a position in the source.*/
case class Position(line: Int, column: Int)

object Position {
  def apply(location: gherkin.ast.Location): Position = Position(location.getLine, location.getColumn)
}

/**
 * Abstract syntax tree of a successfully parsed feature.
 * The [[GherkinParser]] parses all plain text features into a tree of
 * this type.  The [[gwen.eval.GwenInterpreter interpreter]] normalises 
 * the tree before passing it down to the 
 * [[gwen.eval.EvalEngine evaluation engine]] and lower layers for 
 * processing.
 *
 * @param feature the feature
 * @param background optional background
 * @param scenarios list of scenarios
 * @param featureFile optional source feature file
 * @param metaSpecs optional list of meta specs
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
  
  /** Returns the evaluation status of this feature spec. */
  override lazy val evalStatus: EvalStatus = {
    val ss = steps.map(_.evalStatus)
    val specStatus = EvalStatus(ss)
    metaSpecs match {
      case Nil => specStatus
      case _ =>
        val totalStatus = EvalStatus((metaSpecs.flatMap(_.steps) ++ steps).map(_.evalStatus))
        specStatus match {
          case Passed(_) => Passed(totalStatus.nanos)
          case _ => totalStatus
        }
    }
  }
  
  override def toString = feature.name 
}
object FeatureSpec {
  def apply(spec: gherkin.ast.GherkinDocument): FeatureSpec = {
    FeatureSpec(
      Feature(spec.getFeature),
      spec.getFeature.getChildren.asScala.find(_.isInstanceOf[gherkin.ast.Background]).map(_.asInstanceOf[gherkin.ast.Background]).map(b => Background(b)),
      spec.getFeature.getChildren.asScala.toList.filter(_.isInstanceOf[gherkin.ast.Scenario]).map(_.asInstanceOf[gherkin.ast.Scenario]).map(s => Scenario(s)),
      None,
      Nil)
  }
}

/**
  * Captures a gherkin feature node.
  *
  * @param tags list of tags
  * @param name the feature name
  * @param description optional description
  *
  * @author Branko Juric
  */
case class Feature(tags: List[Tag], name: String, description: List[String]) extends SpecNode {
  override def toString = name
}
object Feature {
  def apply(feature: gherkin.ast.Feature): Feature =
    Feature(
      Option(feature.getTags).map(_.asScala.toList).getOrElse(Nil).map(t =>Tag(t)), 
      feature.getName, 
      Option(feature.getDescription).map(_.split("\n").toList.map(_.trim)).getOrElse(Nil).distinct)
  def apply(name: String, description: List[String]): Feature = new Feature(Nil, name, description)
}

/**
  * Captures a gherkin background node.
  *
  * @param name the background name
  * @param description optional background description
  * @param steps list of background steps
  *
  * @author Branko Juric
 */
case class Background(name: String, description: List[String], steps: List[Step]) extends SpecNode {
  
  /** Returns the evaluation status of this background. */
  override lazy val evalStatus: EvalStatus = EvalStatus(steps.map(_.evalStatus))
  
  override def toString = name
  
}

object Background {
  def apply(background: gherkin.ast.Background): Background = 
    Background(
      background.getName,
      Option(background.getDescription).map(_.split("\n").toList.map(_.trim)).getOrElse(Nil),
      Option(background.getSteps).map(_.asScala.toList).getOrElse(Nil).map(s => Step(s)))
  def apply(background: Background, steps: List[Step]): Background = 
    Background(background.name, background.description, steps) 
}

/**
  * Captures a gherkin scenario.
  * @param tags list of tags
  * @param name the scenario name
  * @param description the optional background description
  * @param background optional background
  * @param steps list of scenario steps
  * @param metaFile: optional meta file (required if the scenario is a stepdef)
  *
  * @author Branko Juric
  */
case class Scenario(tags: List[Tag], name: String, description: List[String], background: Option[Background], steps: List[Step], metaFile: Option[File]) extends SpecNode {
  
  /**
    * Returns a list containing all the background steps (if any) followed by 
    * all the scenario steps.
    */
  def allSteps = background.map(_.steps).getOrElse(Nil) ++ steps
  
  def isStepDef = tags.contains(Tag.StepDefTag)
  
  /** Returns the evaluation status of this scenario. */
  override lazy val evalStatus: EvalStatus = EvalStatus(allSteps.map(_.evalStatus))

  
  override def toString = name
  
}
object Scenario {
  def apply(scenario: gherkin.ast.Scenario): Scenario = 
    new Scenario(
      Option(scenario.getTags).map(_.asScala.toList).getOrElse(Nil).map(t => Tag(t)).distinct, 
      scenario.getName, 
      Option(scenario.getDescription).map(_.split("\n").toList.map(_.trim)).getOrElse(Nil),
      None, 
      Option(scenario.getSteps).map(_.asScala.toList).getOrElse(Nil).map(s => Step(s)), 
      None)
  def apply(tags: List[Tag], name: String, description: List[String], background: Option[Background], steps: List[Step]): Scenario = 
    new Scenario(tags.distinct, name, description, background, steps, None)
  def apply(scenario: Scenario, background: Option[Background], steps: List[Step]): Scenario = 
    apply(scenario.tags, scenario.name, scenario.description, background, steps, scenario.metaFile)
  def apply(scenario: Scenario, metaFile: Option[File]): Scenario = 
    new Scenario(scenario.tags, scenario.name, scenario.description, scenario.background, scenario.steps, metaFile)
}

/**
  * Captures a gherkin tag.
  *
  * @param name name the tag name
  *    
  * @author Branko Juric
  */
case class Tag(name: String) extends SpecNode {
  
  /** Returns a string representation of this tag. */
  override def toString = s"@$name"
  
}
object Tag {
  
  val StepDefTag = Tag("StepDef")
  private val Regex = """~?@(\w+)""".r
  
  import scala.language.implicitConversions
  
  /**
    * Implicitly converts a tag string to a tag object.
    * 
    *  @param value the string value to convert
    *  @throws gwen.errors.InvalidTagException if the tag string is invalid 
    */
  implicit def string2Tag(value: String) = value match {
    case Regex(name) => Tag(name)
    case _ => invalidTagError(value)
  }
  
  def apply(tag: gherkin.ast.Tag): Tag =
    if (tag.getName.startsWith("@")) Tag(tag.getName.substring(1))
    else Tag(tag.getName)
  
}

/**
  * Captures a gherkin step.
  *
  * @param pos the location of the node in the source
  * @param keyword keyword identifier (Given, When, Then, etc..)
  * @param expression free format step expression line (that is: the text following the step keyword)
  * @param evalStatus optional evaluation status (default = Pending)
  * @param attachments file attachments as name-file pairs (default = Nil)
  * @param stepDef optional evaluated step def
  *    
  * @author Branko Juric
  */
case class Step(
    pos: Position,
    keyword: StepKeyword.Value, 
    expression: String, 
    status: EvalStatus = Pending, 
    attachments: List[(String, File)] = Nil,
    stepDef: Option[Scenario] = None) extends SpecNode {
  
  /** Returns the evaluation status of this step definition. */
  override lazy val evalStatus: EvalStatus = status
  
  /** Returns a string representation of this step. */
  override def toString = s"$keyword $expression"
  
}

object Step {
  def apply(step: gherkin.ast.Step): Step =
    new Step(Position(step.getLocation), StepKeyword.names(step.getKeyword.trim), step.getText)
  def apply(keyword: StepKeyword.Value, expression: String): Step =
    new Step(Position(1, 1), keyword, expression)
  def apply(keyword: StepKeyword.Value, expression: String, status: EvalStatus): Step =
    new Step(Position(1, 1), keyword, expression, status)
  def apply(step: Step, pos: Position): Step =
    new Step(pos, step.keyword, step.expression, step.status, step.attachments, step.stepDef)
  def apply(step: Step, expression: String): Step =
    new Step(step.pos, step.keyword, expression, step.status, step.attachments)
  def apply(step: Step, stepDef: Scenario): Step =
    new Step(step.pos, step.keyword, step.expression, stepDef.evalStatus, stepDef.steps.flatMap(_.attachments), Some(stepDef))
  def apply(step: Step, status: EvalStatus, attachments: List[(String, File)]): Step =
    new Step(step.pos, step.keyword, step.expression, status, attachments, step.stepDef)
}
