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

import gwen.errors._
import gwen.Predefs.Kestrel

import scala.collection.JavaConverters._

/** Reperesents a position in the source.*/
case class Position(line: Int, column: Int)

object Position {
  def apply(location: gherkin.ast.Location): Position =
    Option(location).map(loc => Position(loc.getLine, loc.getColumn)).getOrElse(Position(0, 0))
}

/**
  * Base trait for capturing a feature spec in an abstract syntax tree.  
  * An spec node is the raw output produced by the [[GherkinParser]].
  *
  * @author Branko Juric
  */
trait SpecNode {
  /** Returns the evaluation status of this node. */
  lazy val evalStatus: EvalStatus = Pending

  /** The position of the node in the source. */
  var pos: Position = Position(0, 0)
  
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

  /** Gets all attachments. */
  def attachments: List[(String, File)] = steps.flatMap(_.attachments)
  
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
  
  override def toString: String = feature.name
}
object FeatureSpec {
  def apply(spec: gherkin.ast.GherkinDocument): FeatureSpec = {
    FeatureSpec(
      Feature(spec.getFeature),
      spec.getFeature.getChildren.asScala.find(_.isInstanceOf[gherkin.ast.Background]).map(_.asInstanceOf[gherkin.ast.Background]).map(b => Background(b)),
      spec.getFeature.getChildren.asScala.toList.filter(x => x.isInstanceOf[gherkin.ast.Scenario] || x.isInstanceOf[gherkin.ast.ScenarioOutline]).flatMap { s =>
        s match {
          case scenario: gherkin.ast.Scenario => List(Scenario(scenario))
          case outline: gherkin.ast.ScenarioOutline => List(Scenario(outline))
        }
      },
      None,
      Nil) tap { f => f.pos = Position(spec.getLocation) }
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
  override def toString: String = name
}
object Feature {
  final val keyword = FeatureKeyword.Feature.toString
  def apply(feature: gherkin.ast.Feature): Feature =
    Feature(
      Option(feature.getTags).map(_.asScala.toList).getOrElse(Nil).map(t =>Tag(t)), 
      feature.getName, 
      Option(feature.getDescription).map(_.split("\n").toList.map(_.trim)).getOrElse(Nil).distinct
    ) tap { f => f.pos = Position(feature.getLocation) }
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
  
  override def toString: String = name
  
}

object Background {
  final val keyword = FeatureKeyword.Background.toString
  def apply(background: gherkin.ast.Background): Background = 
    Background(
      background.getName,
      Option(background.getDescription).map(_.split("\n").toList.map(_.trim)).getOrElse(Nil),
      Option(background.getSteps).map(_.asScala.toList).getOrElse(Nil).map(s => Step(s))
    ) tap { b => b.pos = Position(background.getLocation) }
  def apply(background: Background, steps: List[Step]): Background =
    Background(background.name, background.description, steps) tap { b => b.pos = background.pos }
}

/**
  * Captures a gherkin scenario.
  * @param tags list of tags
  * @param name the scenario name
  * @param description optional description
  * @param background optional background
  * @param steps list of scenario steps
  * @param isOutline true if this is a scenario outline; false otherwise
  * @param examples optional list of examples (scenario outline entries)
  * @param metaFile optional meta file (required if the scenario is a stepdef)
  *
  * @author Branko Juric
  */
case class Scenario(
    tags: List[Tag],
    name: String,
    description: List[String],
    background: Option[Background],
    steps: List[Step],
    isOutline: Boolean,
    examples: List[Examples],
    metaFile: Option[File]) extends SpecNode {

  def keyword: String =
    if(isForEach) Tag.ForEachTag.name
    else if (isStepDef) Tag.StepDefTag.name
    else if (!isOutline) FeatureKeyword.Scenario.toString
    else FeatureKeyword.`Scenario Outline`.toString

  /**
    * Returns a list containing all the background steps (if any) followed by 
    * all the scenario steps.
    */
  def allSteps: List[Step] = background.map(_.steps).getOrElse(Nil) ++ (if (!isOutline) steps else examples.flatMap(_.allSteps))
  
  def isStepDef: Boolean = tags.contains(Tag.StepDefTag)
  def isForEach: Boolean = tags.contains(Tag.ForEachTag)
  def isDataTable: Boolean = tags.exists(_.name.startsWith(Tag.DataTableTag.name))

  def attachments: List[(String, File)] = allSteps.flatMap(_.attachments)
  
  /** Returns the evaluation status of this scenario. */
  override lazy val evalStatus: EvalStatus =
    if (isOutline && examples.flatMap(_.scenarios).isEmpty) Pending else EvalStatus(allSteps.map(_.evalStatus))

  override def toString: String = name
  
}
object Scenario {
  def apply(scenario: gherkin.ast.Scenario): Scenario = {
    new Scenario(
      Option(scenario.getTags).map(_.asScala.toList).getOrElse(Nil).map(t => Tag(t)).distinct,
      scenario.getName,
      Option(scenario.getDescription).map(_.split("\n").toList.map(_.trim)).getOrElse(Nil),
      None,
      Option(scenario.getSteps).map(_.asScala.toList).getOrElse(Nil).map(s => Step(s)),
      isOutline = false,
      Nil,
      None
    ) tap { s => s.pos = Position(scenario.getLocation) }
  }
  def apply(outline: gherkin.ast.ScenarioOutline): Scenario = {
    new Scenario(
      Option(outline.getTags).map(_.asScala.toList).getOrElse(Nil).map(t => Tag(t)).distinct,
      outline.getName,
      Option(outline.getDescription).map(_.split("\n").toList.map(_.trim)).getOrElse(Nil),
      None,
      Option(outline.getSteps).map(_.asScala.toList).getOrElse(Nil).map(s => Step(s)),
      isOutline = true,
      outline.getExamples.asScala.toList.zipWithIndex map { case (examples, index) => Examples(examples, index) },
      None
    ) { s => s.pos = Position(outline.getLocation) }
  }
  def apply(scenario: Scenario, background: Option[Background], steps: List[Step], examples: List[Examples]): Scenario =
    new Scenario(scenario.tags, scenario.name, scenario.description, background, steps, scenario.isOutline, examples, scenario.metaFile) tap { s => s.pos = scenario.pos }
  def apply(scenario: Scenario, metaFile: Option[File]): Scenario =
    new Scenario(scenario.tags, scenario.name, scenario.description, scenario.background, scenario.steps, scenario.isOutline, scenario.examples, metaFile) tap { s => s.pos = scenario.pos }
  def apply(outline: Scenario, examples: List[Examples]): Scenario =
    new Scenario(outline.tags, outline.name, outline.description, outline.background, outline.steps, outline.isOutline, examples, outline.metaFile) tap { s => s.pos = outline.pos }
}

/**
  * Captures a gherkin scenario outline example group.
  *
  * @param name the example name
  * @param description option description lines
  * @param table header and body data (tuple of line position and rows of data)
  * @param scenarios list of expanded scenarios
  */
case class Examples(name: String, description: List[String], table: List[(Int, List[String])], scenarios: List[Scenario]) extends SpecNode {

  /**
    * Returns a list containing all the background steps (if any) followed by
    * all the scenario steps.
    */
  def allSteps: List[Step] = scenarios.flatMap(_.allSteps)

  /** Returns the evaluation status of this examples group. */
  override lazy val evalStatus: EvalStatus = EvalStatus(scenarios.map(_.evalStatus))

  override def toString: String = name
}

object Examples {
  final val keyword = FeatureKeyword.Examples.toString
  def apply(examples: gherkin.ast.Examples, index: Int): Examples = {
    val header = examples.getTableHeader
    if (header == null) parsingError(s"Failed to read table body. Possible syntax error or missing column delimiter in table defined at line ${examples.getLocation.getLine}")
    val body = examples.getTableBody
    if (body == null) parsingError(s"Failed to read table header. Possible syntax error or missing column delimiter in table defined at line ${examples.getLocation.getLine}")
    new Examples(
      examples.getName,
      Option(examples.getDescription).map(_.split("\n").toList.map(_.trim)).getOrElse(Nil),
      (header.getLocation.getLine, header.getCells.asScala.toList.map(_.getValue)) ::
        body.iterator.asScala.toList.map { row =>
          (row.getLocation.getLine, row.getCells.asScala.toList.map(_.getValue))
        },
      Nil
    ) tap { e => e.pos = Position(examples.getLocation) }
  }
  def apply(examples: Examples, scenarios: List[Scenario]): Examples =
    Examples(examples.name, examples.description, examples.table, scenarios) tap { e => e.pos = examples.pos }
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

  def isInBuilt = Tag.InbuiltTags.exists(_.name.startsWith(name))
  
}
object Tag {

  val ImportTag = Tag("Import")
  val StepDefTag = Tag("StepDef")
  val ForEachTag = Tag("ForEach")
  val DataTableTag = Tag("DataTable")

  val InbuiltTags = List(ImportTag, StepDefTag, ForEachTag, DataTableTag)

  private val Regex = """~?@([^\s]+)""".r

  import scala.language.implicitConversions

  /**
    * Implicitly converts a tag string to a tag object.
    *
    *  @param value the string value to convert
    *  @throws gwen.errors.InvalidTagException if the tag string is invalid
    */
  implicit def string2Tag(value: String): Tag = value match {
    case Regex(name) => Tag(name)
    case _ => invalidTagError(value)
  }

  def apply(tag: gherkin.ast.Tag): Tag = {
    (if (tag.getName.startsWith("@")) Tag(tag.getName.substring(1)) else Tag(tag.getName)) tap { t =>
      if (t.name.startsWith(DataTableTag.name)) {
        DataTable.checkTagSyntax(t)
      }
      t.pos = Position(tag.getLocation)
    }
  }
  
}

/**
  * Captures a gherkin step.
  *
  * @param keyword keyword identifier (Given, When, Then, etc..)
  * @param name the step name (that is: the text following the step keyword)
  * @param status optional evaluation status (default = Pending)
  * @param attachments file attachments as name-file pairs (default = Nil)
  * @param stepDef optional evaluated step def
  * @param table data table (List of tuples of line position and rows of data)
  * @param docString optional tuple of line, content, and content type
  *    
  * @author Branko Juric
  */
case class Step(
    keyword: StepKeyword.Value,
    name: String,
    status: EvalStatus = Pending, 
    attachments: List[(String, File)] = Nil,
    stepDef: Option[Scenario] = None,
    table: List[(Int, List[String])] = Nil,
    docString: Option[(Int, String, Option[String])] = None) extends SpecNode {
  
  /** Returns the evaluation status of this step definition. */
  override lazy val evalStatus: EvalStatus = status

  def expression: String = docString map { case (_, content, _) =>
    val lines = content.split("""\r?\n""")
    s"""$name "${lines(0)}${if (lines.length > 1) "..." else ""}""""
  } getOrElse(name)
  
  /** Returns a string representation of this step. */
  override def toString = s"$keyword ${expression}"

  /** Returns a unique string ID value for this object .*/
  lazy val uniqueId = java.util.UUID.randomUUID.toString

  /** Returns the given value if the step has no docString or the docString content otherwise. */
  def orDocString(value: String): String = docString.map(_._2).getOrElse(value)
  
}

object Step {
  def apply(step: gherkin.ast.Step): Step = {
    val dataTable = Option(step.getArgument).filter(_.isInstanceOf[gherkin.ast.DataTable]).map(_.asInstanceOf[gherkin.ast.DataTable]) map { dt =>
      dt.getRows.asScala.toList map { row =>
        (row.getLocation.getLine, row.getCells.asScala.toList.map(_.getValue))
      }
    } getOrElse Nil
    val docString = Option(step.getArgument).filter(_.isInstanceOf[gherkin.ast.DocString]).map(_.asInstanceOf[gherkin.ast.DocString]) map { ds =>
      (ds.getLocation.getLine, ds.getContent, Option(ds.getContentType))
    }
    new Step(
      StepKeyword.withName(step.getKeyword.trim),
      step.getText,
      table = dataTable,
      docString = docString
    ) tap { s => s.pos = Position(step.getLocation) }
  }
  def apply(pos: Position, keyword: StepKeyword.Value, expression: String): Step =
    new Step(keyword, expression) tap { s => s.pos = pos }
  def apply(pos: Position, keyword: StepKeyword.Value, expression: String, status: EvalStatus): Step =
    new Step(keyword, expression, status) tap { s => s.pos = pos }
  def apply(step: Step, pos: Position): Step =
    new Step(step.keyword, step.name, step.status, step.attachments, step.stepDef, step.table, step.docString) tap { s => s.pos = pos}
  def apply(step: Step, expression: String): Step =
    new Step(step.keyword, expression, step.status, step.attachments, table = step.table, docString = step.docString) tap { s => s.pos = step.pos}
  def apply(step: Step, stepDef: Scenario): Step =
    new Step(step.keyword, step.name, stepDef.evalStatus, stepDef.steps.flatMap(_.attachments), Some(stepDef), step.table, step.docString) tap { s => s.pos = step.pos }
  def apply(step: Step, status: EvalStatus, attachments: List[(String, File)]): Step =
    new Step(step.keyword, step.name, status, attachments, step.stepDef, step.table, step.docString) tap { s => s.pos = step.pos }
  def apply(step: Step, status: EvalStatus, attachments: List[(String, File)], foreachStepDef: Scenario): Step =
    new Step(step.keyword, step.name, status, attachments, Some(foreachStepDef), step.table, step.docString) tap { s => s.pos = step.pos }
}

