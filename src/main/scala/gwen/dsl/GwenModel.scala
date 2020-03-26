/*
 * Copyright 2014-2020 Branko Juric, Brady Wood
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

import io.cucumber.messages.Messages.GherkinDocument
import io.cucumber.messages.Messages.Location

import java.io.File

import gwen.errors._
import gwen.Predefs.Kestrel

import scala.collection.JavaConverters._

/** Reperesents a position in the source.*/
case class Position(line: Int, column: Int)

object Position {
  private val lineOffset = new ThreadLocal[Int]() {
    override protected def initialValue: Int = 0
  }
  def setLineOffset(offset: Int) {
    lineOffset.set(offset)
  }
  def apply(location: Location): Position =
    Option(location).map(loc => Position(loc.getLine + lineOffset.get, loc.getColumn)).getOrElse(Position(0, 0))
}

/**
  * Base trait for capturing a feature spec in an abstract syntax tree.  
  * An spec node is the raw output produced by the [[GherkinParser]].
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
  rules: List[Rule],
  featureFile: Option[File] = None,
  metaSpecs: List[FeatureSpec] = Nil) extends SpecNode {
  
  /**
    * Gets the list of all steps contained in the feature spec. The list includes
    * all meta steps (if any) and all scenario steps (including any background 
    * steps).
    * 
    * @return a list containing all the steps (or an empty list if none exist)
    */
  def steps: List[Step] = rules.flatMap(_.allSteps) ++ scenarios.flatMap(_.allSteps)

  def evalScenarios = scenarios.flatMap(_.evalScenarios) ++ rules.flatMap(_.evalScenarios)

  /** Gets all attachments. */
  def attachments: List[(String, File)] = steps.flatMap(_.attachments)

  /** Gets the number of sustained errors. */
  def sustainedCount: Int = steps.flatMap { s1 =>
    s1.stepDef.map { s2 =>
      s2.allSteps.flatMap { s3 =>
        s3.stepDef.map(_.allSteps).getOrElse(List(s3))
      }
    }.getOrElse(List(s1))
  }.count(_.evalStatus.status == StatusKeyword.Sustained)
  
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
  def apply(spec: GherkinDocument): FeatureSpec = {
    FeatureSpec(
      Feature(spec.getFeature),
      spec.getFeature.getChildrenList.asScala.toList.filter(_.hasBackground).headOption.map(x => Background(x.getBackground)),
      spec.getFeature.getChildrenList.asScala.toList.filter(_.hasScenario).map(x => Scenario(x.getScenario)),
      spec.getFeature.getChildrenList.asScala.toList.filter(_.hasRule()).map(x => Rule(x.getRule())),
      None,
      Nil)
  }
}

/**
  * Captures a gherkin feature node.
  *
  * @param language the language identifier (example: en for English)
  * @param tags list of tags
  * @param keyword the Gherkin keyword for this Feature
  * @param name the feature name
  * @param description optional description
  *
  * @author Branko Juric
  */
case class Feature(language: String, tags: List[Tag], keyword: String, name: String, description: List[String]) extends SpecNode {
  override def toString: String = name
}
object Feature {
  def apply(feature: GherkinDocument.Feature): Feature = {
    Feature(
      feature.getLanguage,
      Option(feature.getTagsList).map(_.asScala.toList).getOrElse(Nil).map(t =>Tag(t)), 
      feature.getKeyword,
      feature.getName, 
      Option(feature.getDescription).filter(_.length > 0).map(_.split("\n").toList.map(_.trim)).getOrElse(Nil).distinct
    ) tap { f => f.pos = Position(feature.getLocation) }
  }
}

/**
  * Captures a gherkin background node.
  *
  * @param keyword the Gherkin keyword for this Background
  * @param name the background name
  * @param description optional background description
  * @param steps list of background steps
  *
  * @author Branko Juric
 */
case class Background(keyword: String, name: String, description: List[String], steps: List[Step]) extends SpecNode {

  /** Returns the evaluation status of this background. */
  override lazy val evalStatus: EvalStatus = EvalStatus(steps.map(_.evalStatus))

  def skip = Background(this, steps.map(_.skip))

  def gwtOrder: List[String] = steps.map(_.keyword).filter(k => !StepKeyword.isAnd(k))

  override def toString: String = name
  
}

object Background {
  def apply(background: io.cucumber.messages.Messages.GherkinDocument.Feature.Background): Background = 
    Background(
      background.getKeyword,
      background.getName,
      Option(background.getDescription).filter(_.length > 0).map(_.split("\n").toList.map(_.trim)).getOrElse(Nil),
      Option(background.getStepsList).map(_.asScala.toList).getOrElse(Nil).map(s => Step(s))
    ) tap { b => b.pos = Position(background.getLocation) }
  def apply(background: Background, steps: List[Step]): Background =
    Background(background.keyword, background.name, background.description, steps) tap { b => b.pos = background.pos }
}

/**
  * Captures a gherkin rule.
  * 
  * @param keyword the Gherkin keyword for this Rule
  * @param name the rule name
  * @param description optional description
  * @param background optional background
  * @param scenarios list of scenarios (or examples)
  */
case class Rule(
  keyword: String,
  name: String,
  description: List[String],
  background: Option[Background],
  scenarios: List[Scenario]) extends SpecNode {
  
  /**
    * Gets the list of all steps contained in the rule. The list includes
    * all meta steps (if any) and all scenario steps (including any background 
    * steps).
    * 
    * @return a list containing all the steps (or an empty list if none exist)
    */
  def allSteps: List[Step] = background.map(_.steps).getOrElse(Nil) ++ scenarios.flatMap(_.allSteps)

  def evalScenarios: List[Scenario] = scenarios.flatMap(_.evalScenarios)
  
  /** Returns the evaluation status of this rule. */
  override lazy val evalStatus: EvalStatus = EvalStatus(allSteps.map(_.evalStatus))

  def skip = Rule(this, background.map(_.skip), scenarios.map(_.skip))
  
  override def toString: String = name
}
object Rule {
  def apply(rule: io.cucumber.messages.Messages.GherkinDocument.Feature.FeatureChild.Rule): Rule = {
    new Rule(
      rule.getKeyword,
      rule.getName,
      Option(rule.getDescription).filter(_.length > 0).map(_.split("\n").toList.map(_.trim)).getOrElse(Nil),
      rule.getChildrenList.asScala.toList.filter(_.hasBackground).headOption.map(x => Background(x.getBackground)),
      rule.getChildrenList.asScala.toList.filter(_.hasScenario).map(x => Scenario(x.getScenario))
    ) tap { s => s.pos = Position(rule.getLocation) }
  }
  def apply(rule: Rule, background: Option[Background], scenarios: List[Scenario]): Rule =
    new Rule(rule.keyword, rule.name, rule.description, background, scenarios)
  def apply(rule: Rule, scenarios: List[Scenario]): Rule =
    new Rule(rule.keyword, rule.name, rule.description, rule.background, scenarios)
}

/**
  * Captures a gherkin scenario.
  * @param tags list of tags
  * @param keyword the Gherkin keyword
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
    keyword: String,
    name: String,
    description: List[String],
    background: Option[Background],
    steps: List[Step],
    isOutline: Boolean,
    examples: List[Examples],
    metaFile: Option[File]) extends SpecNode {

  /**
    * Returns a list containing all steps.
    */
  def allSteps: List[Step] = background.map(_.steps).getOrElse(Nil) ++ (if (!isOutline) steps else examples.flatMap(_.allSteps))
  
  def evalScenarios: List[Scenario] = 
    if (isStepDef) Nil
    else if(isOutline) examples.flatMap(_.scenarios)
    else List(this)
  
  def isStepDef: Boolean = tags.contains(Tag.StepDefTag)
  def isForEach: Boolean = tags.contains(Tag.ForEachTag)
  def isDataTable: Boolean = tags.exists(_.name.startsWith(Tag.DataTableTag.name))
  def isSynchronized: Boolean = tags.exists(t => t == Tag.SynchronizedTag || t == Tag.SynchronisedTag)
  
  def attachments: List[(String, File)] = allSteps.flatMap(_.attachments)
  
  /** Returns the evaluation status of this scenario. */
  override lazy val evalStatus: EvalStatus =
    if (isOutline && examples.flatMap(_.scenarios).isEmpty) Pending else EvalStatus(allSteps.map(_.evalStatus), ignoreSustained = !isStepDef)

  def skip = Scenario(this, background.map(_.skip), steps.map(_.skip), examples.map(_.skip))

  def behaviorTag: Option[Tag] = tags.find(tag => BehaviorType.values.exists(_.toString == tag.name))

  override def toString: String = name
  
}
object Scenario {
  def apply(scenario: io.cucumber.messages.Messages.GherkinDocument.Feature.Scenario): Scenario = {
    def tags = Option(scenario.getTagsList).map(_.asScala.toList).getOrElse(Nil).map(t => Tag(t)).distinct
    new Scenario(
      tags,
      keywordFor(tags, scenario.getKeyword),
      scenario.getName,
      Option(scenario.getDescription).filter(_.length > 0).map(_.split("\n").toList.map(_.trim)).getOrElse(Nil),
      None,
      Option(scenario.getStepsList).map(_.asScala.toList).getOrElse(Nil).map(s => Step(s)),
      isOutline = scenario.getExamplesCount() > 0 || tags.exists(_.name.startsWith("Examples(")),
      scenario.getExamplesList.asScala.toList.zipWithIndex map { case (examples, index) => Examples(examples, index) },
      None
    ) tap { s => s.pos = Position(scenario.getLocation) }
  }
  def apply(scenario: Scenario, background: Option[Background], steps: List[Step], examples: List[Examples]): Scenario =
    new Scenario(scenario.tags, keywordFor(scenario), scenario.name, scenario.description, background, steps, scenario.isOutline, examples, scenario.metaFile) tap { s => s.pos = scenario.pos }
  def apply(scenario: Scenario, metaFile: Option[File]): Scenario =
    new Scenario(scenario.tags, keywordFor(scenario), scenario.name, scenario.description, scenario.background, scenario.steps, scenario.isOutline, scenario.examples, metaFile) tap { s => s.pos = scenario.pos }
  def apply(outline: Scenario, examples: List[Examples]): Scenario =
    new Scenario(outline.tags, keywordFor(outline), outline.name, outline.description, outline.background, outline.steps, outline.isOutline, examples, outline.metaFile) tap { s => s.pos = outline.pos }
  def keywordFor(scenario: Scenario): String = keywordFor(scenario.tags, scenario.keyword)
  def keywordFor(tags: List[Tag], keyword: String): String = 
    if(tags.contains(Tag.ForEachTag)) Tag.ForEachTag.name
    else if (tags.contains(Tag.StepDefTag)) Tag.StepDefTag.name
    else keyword.trim
}

/**
  * Captures a gherkin scenario outline example group.
  *
  * @param tags list of tags
  * @param keyword the Gherkin keyword for this Examples clause
  * @param name the example name
  * @param description option description lines
  * @param table header and body data (tuple of line position and rows of data)
  * @param scenarios list of expanded scenarios
  */
case class Examples(tags: List[Tag], keyword: String, name: String, description: List[String], table: List[(Int, List[String])], scenarios: List[Scenario]) extends SpecNode {

  /**
    * Returns a list containing all the background steps (if any) followed by
    * all the scenario steps.
    */
  def allSteps: List[Step] = scenarios.flatMap(_.allSteps)

  /** Returns the evaluation status of this examples group. */
  override lazy val evalStatus: EvalStatus = EvalStatus(scenarios.map(_.evalStatus))

  def skip = Examples(this, scenarios.map { s =>
    Scenario(s, s.background.map(_.skip), s.steps.map(_.skip), s.examples)
  })

  override def toString: String = name
}

object Examples {
  def apply(examples: io.cucumber.messages.Messages.GherkinDocument.Feature.Scenario.Examples, index: Int): Examples = {
    val header = examples.getTableHeader
    if (header == null) {
      syntaxError(
        s"Failed to read table body. Possible syntax error or missing column delimiter in table",
        examples.getLocation.getLine,
        examples.getLocation.getColumn)
    }
    val body = examples.getTableBodyList
    if (body == null) {
      syntaxError(
        s"Failed to read table header. Possible syntax error or missing column delimiter in table",
        examples.getLocation.getLine,
        examples.getLocation.getColumn)
    }
    new Examples(
      Option(examples.getTagsList).map(_.asScala.toList).getOrElse(Nil).map(t =>Tag(t)),
      examples.getKeyword,
      examples.getName,
      Option(examples.getDescription).filter(_.length > 0).map(_.split("\n").toList.map(_.trim)).getOrElse(Nil),
      (header.getLocation.getLine, header.getCellsList.asScala.toList.map(_.getValue)) ::
        body.iterator.asScala.toList.map { row =>
          (row.getLocation.getLine, row.getCellsList.asScala.toList.map(_.getValue))
        },
      Nil
    ) tap { e => e.pos = Position(examples.getLocation) }
  }
  def apply(examples: Examples, scenarios: List[Scenario]): Examples =
    Examples(examples.tags, examples.keyword, examples.name, examples.description, examples.table, scenarios) tap { e => e.pos = examples.pos }
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
  val SynchronisedTag = Tag("Synchronised")
  val SynchronizedTag = Tag("Synchronized")

  val InbuiltTags = List(ImportTag, StepDefTag, ForEachTag, DataTableTag, SynchronisedTag, SynchronizedTag)

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

  def apply(tag: io.cucumber.messages.Messages.GherkinDocument.Feature.Tag): Tag = {
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
    keyword: String,
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
  
  /** Returns a unique string ID value for this object .*/
  lazy val uniqueId = java.util.UUID.randomUUID.toString

  /** Returns the given value if the step has no docString or the docString content otherwise. */
  def orDocString(value: String): String = docString.map(_._2).getOrElse(value)

  def skip = Step(this, Skipped, attachments)

  /** Returns a string representation of this step. */
  override def toString = s"$keyword ${expression}"
}

object Step {
  def apply(step: io.cucumber.messages.Messages.GherkinDocument.Feature.Step): Step = {
    val dataTable = Option(step.getDataTable).map { dt =>
      dt.getRowsList.asScala.toList map { row =>
        (row.getLocation.getLine, row.getCellsList.asScala.toList.map(_.getValue))
      }
    } getOrElse Nil
    val docString = Option(step.getDocString()).filter(_.getContent().trim.length > 0) map { ds =>
      (ds.getLocation.getLine, ds.getContent, Option(ds.getContentType).filter(_.trim.length > 0))
    }
    new Step(
      step.getKeyword.trim,
      step.getText,
      table = dataTable,
      docString = docString
    ) tap { s => s.pos = Position(step.getLocation) }
  }
  def apply(pos: Position, keyword: String, expression: String): Step =
    new Step(keyword, expression) tap { s => s.pos = pos }
  def apply(pos: Position, keyword: String, expression: String, status: EvalStatus): Step =
    new Step(keyword, expression, status) tap { s => s.pos = pos }
  def apply(step: Step, pos: Position): Step =
    new Step(step.keyword, step.name, step.status, step.attachments, step.stepDef, step.table, step.docString) tap { s => s.pos = pos}
  def apply(step: Step, keyword: String, expression: String): Step =
    new Step(keyword, expression, step.status, step.attachments, table = step.table, docString = step.docString) tap { s => s.pos = step.pos}
  def apply(step: Step, stepDef: Scenario): Step =
    new Step(step.keyword, step.name, stepDef.evalStatus, stepDef.steps.flatMap(_.attachments), Some(stepDef), step.table, step.docString) tap { s => s.pos = step.pos }
  def apply(step: Step, status: EvalStatus): Step =
    new Step(step.keyword, step.name, status, step.attachments, step.stepDef, step.table, step.docString) tap { s => s.pos = step.pos }
  def apply(step: Step, status: EvalStatus, attachments: List[(String, File)]): Step =
    new Step(step.keyword, step.name, status, attachments, step.stepDef, step.table, step.docString) tap { s => s.pos = step.pos }
  def apply(step: Step, status: EvalStatus, attachments: List[(String, File)], foreachStepDef: Scenario): Step =
    new Step(step.keyword, step.name, status, attachments, Some(foreachStepDef), step.table, step.docString) tap { s => s.pos = step.pos }
}

