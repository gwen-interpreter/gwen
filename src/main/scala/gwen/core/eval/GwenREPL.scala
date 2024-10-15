/*
 * Copyright 2014-2023 Branko Juric, Brady Wood
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

package gwen.core.eval

import gwen.core._
import gwen.core.behavior.BehaviorMode
import gwen.core.behavior.FeatureMode
import gwen.core.node.GwenNode
import gwen.core.node.FeatureUnit
import gwen.core.node.Root
import gwen.core.node.gherkin.Dialect
import gwen.core.node.gherkin.GherkinKeyword
import gwen.core.node.gherkin.SpecPrinter
import gwen.core.node.gherkin.StepKeyword
import gwen.core.node.gherkin.Step
import gwen.core.state.StateLevel
import gwen.core.status.Failed
import gwen.core.status.EvalStatus
import gwen.core.status.Loaded
import gwen.core.status.Passed

import scala.jdk.CollectionConverters._
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import scala.util.chaining._

import org.fusesource.jansi.Ansi._
import org.jline.builtins.Completers.TreeCompleter
import org.jline.reader.EndOfFileException
import org.jline.reader.LineReader
import org.jline.reader.LineReaderBuilder
import org.jline.reader.impl.DefaultParser
import org.jline.reader.impl.completer.NullCompleter
import org.jline.reader.impl.history.DefaultHistory
import org.jline.terminal.TerminalBuilder
import org.jline.terminal.Terminal
import org.jline.widget.AutosuggestionWidgets

import java.io.File

object GwenREPL {

  lazy val terminal = TerminalBuilder.builder.system(true).build

  val historyFile = new File(GwenSettings.`gwen.outDir`, ".history")

  /** Filters attributes containing or matching given expression (both names and values are checked). */
  def attrFilter(filter: String): PartialFunction[(String, String), Boolean] = {
    case (n, v) => n.contains(filter) || n.matches(filter) || (v != null && (v.contains(filter) || v.matches(filter)))
  }
}

/**
  * Read-Eval-Print-Loop console.
  *
  * @author Branko Juric
  */
class GwenREPL[T <: EvalContext](val engine: EvalEngine[T], ctx: T) extends ImplicitValueKeys {

  private var paste: Option[List[String]] = None
  private var pastingDocString: Boolean = false
  private var debug: Boolean = false

  private val colors = ConsoleColors.isEnabled
  private val printer = new SpecPrinter(deep = false, verbatim = false, colors)
  private def prompt = if (paste.isEmpty) s"${if (colors) ansi.bold else ""}gwen${if (debug) s"@Breakpoint" else ""}> ${if (colors) ansi.reset else ""}" else ""

  private var reader: LineReader = createReader()

  def createReader(): LineReader = {
    val parser = new DefaultParser() {
      setEscapeChars(Array[Char]())
    }
    val tabCompletion = 
      if (GwenSettings.`gwen.console.repl.tabCompletion`) {
        new TreeCompleter(
          (
            List(
              TreeCompleter.node("help"),
              TreeCompleter.node("env", TreeCompleter.node("-a", "-f", """-a "<filter>"""", """-f "<filter>"""", """"<filter>"""")),
              TreeCompleter.node("history"),
              TreeCompleter.node("paste"),
              TreeCompleter.node("load", TreeCompleter.node("<meta-file>")),
              TreeCompleter.node("bye", "exit", "quit", "q"),
            ) ++ (
              ctx.dsl.distinct match {
                case Nil => Nil
                case dsl => 
                  val dslCompleter = TreeCompleter.node(dsl*)
                  StepKeyword.names map { keyword =>
                    TreeCompleter.node(keyword.toString, dslCompleter)
                  }
            })
          ).asJava
        )
      } else NullCompleter.INSTANCE
    LineReaderBuilder.builder()
      .terminal(GwenREPL.terminal)
      .parser(parser)
      .variable(LineReader.HISTORY_FILE, GwenREPL.historyFile)
      .completer(tabCompletion)
      .option(LineReader.Option.HISTORY_TIMESTAMPED, false)
      .option(LineReader.Option.DISABLE_EVENT_EXPANSION, true)
      .build() tap { reader => 
        if (GwenSettings.`gwen.console.repl.autoSuggestions`) {
          val autoSuggest = new AutosuggestionWidgets(reader)
          autoSuggest.enable()
        }
    }
  }

  /** Runs the read-eval-print-loop. */
  def run(): Unit = {
    debug = false
    System.out.println("\nREPL Console")
    System.out.println("\nEnter steps to evaluate or type help for more options..")
    enteringLoop()
    var lines: List[String] = Nil
    try {
      while(
        ((read() tap { ls => lines = ls } zipWithIndex) map { (line, idx) => 
          eval(line) map { output => 
            output tap { _ => 
              if (paste.isEmpty) { 
                if (lines.length > 1) {
                  System.out.println(s"${prompt}${line}") 
                  System.out.println()
                }
                System.out.println(output) 
                if (lines.length > 1 && idx < (lines.length - 1)) {
                  System.out.println()
                }
              } 
            } 
          }
        }).lastOption.map(_.nonEmpty).getOrElse(true)
      ) { }
    } finally {
      exitingLoop()
    }

  }

  /** Runs the read-eval-print-loop in debug mode. */
  def debug(parent: GwenNode, step: Step): Boolean = {
    debug = true
    var continue: Boolean = false
    var quit: Boolean = false
    val verbatimPrinter = new SpecPrinter(deep = false, verbatim = true, colors)
    if (!ctx.isEvaluatingTopLevelStep) System.out.println()
    System.out.println()
    System.out.println(s"Paused at${step.sourceRef.map(sref => s" $sref").getOrElse("")}")
    System.out.println(verbatimPrinter.prettyPrint(parent, step))
    System.out.println("Enter c to continue or q to quit (or type help for more options)..")
    enteringLoop()
    var lines: List[String] = Nil
    try {
      while(
        ((read() tap { ls => lines = ls } zipWithIndex) flatMap { (line, idx) => 
          if (continue || quit) None
          else {
            eval(line) match {
              case None => 
                quit = true
                Some("exit")
              case result =>
                result map { output => 
                  output tap { _ => 
                    if (paste.isEmpty) { 
                      if (output == "continue") {
                        continue = true
                      } else {
                        if (lines.length > 1) {
                          System.out.println(s"${prompt}${line}") 
                          System.out.println()
                        }
                        System.out.println(output) 
                        if (lines.length > 1 && idx < (lines.length - 1)) {
                          System.out.println()
                        }
                      }
                    } 
                  }
                }
            }
          }
        }) filter { output => 
          output == "continue" || output == "exit" 
        } isEmpty
      ) { }
    } finally {
      exitingLoop()
    }
    continue
  }

  private def enteringLoop(): Unit = {
    // repl always runs in imperative and lenient mode
    Settings.setLocal("gwen.feature.mode", FeatureMode.imperative.toString)
    Settings.setLocal("gwen.behavior.rules", BehaviorMode.lenient.toString)

  }
  private def exitingLoop(): Unit = {
    // remove local imperative and lenient mode overrides
    Settings.clearLocal("gwen.feature.mode")
    Settings.clearLocal("gwen.behavior.rules")
  }

  /** Reads all input lines from the command line. */
  private def read(): List[String] = {
    if (paste.isEmpty) System.out.println()
    try {
      val lines = scala.io.Source.fromString(reader.readLine(prompt)).getLines
      (if (lines.hasNext) lines.toList else List("")) tap { _ => 
        if (paste.isEmpty) System.out.println() 
      }
    } catch {
      case _: EndOfFileException => paste.map(_ => List("paste")).getOrElse(List("exit"))
      case e: Throwable => Errors.interruptException(e)
    }
  }

  /**
    * Evaluates a given input string or command.
    *
    * @param input an input step or command
    * @return optional result of the command as a string
    */
  private def eval(input: String): Option[String] = {
    Option(input).getOrElse(paste.map(_ => ":paste").getOrElse("exit")).trim match {
      case "" if paste.isEmpty =>
        Some("  [No-op]")
      case "help" if paste.isEmpty =>
        Some(help)
      case r"""env(.+?)?$$$options""" if paste.isEmpty => 
        Some(env(options))
      case "history" if paste.isEmpty =>
        Some(history)
      case r"""!(\d+)$$$historyValue""" if paste.isEmpty =>
        verifyHistory(historyValue, input)
      case "paste" | ":paste" =>
        pasteMode()
      case "q" | "exit" | "bye" | "quit" if paste.isEmpty =>
        None
      case "c" | "continue" | "resume" if debug =>
        Some("continue")
      case r"""load(.*)$meta""" if paste.isEmpty =>
        loadMeta(meta.trim)
      case _ =>
        evalInput(input)
    }
  }

  private def help = { 
    s"""
      | Gwen REPL commands:
      |
      | help
      |   Displays this help text
      |
      | env [switch] ["filter"]
      |   Lists attributes in the current environment
      |     Only lists visible attributes if no options are specified
      |     switch :
      |       -a : to list all attributes in all scopes
      |       -f : to list all attributes in the feature (global) scope
      |     filter : literal string or regex filter expression
      |
      | :paste|paste
      |   Enters paste mode (for evaluating multiline steps)
      |
      | history
      |   Lists all previously entered commands
      |
      | !<#>
      |   Executes a previously entered command (history bang operator)
      |     # : the history command number
      |
      | load <meta-file>
      |  Loads a meta file to pick up any changes
      |    meta-file : the path to the meta file relative to project root
      | 
      | Given|When|Then|And|But <step>
      |   Evaluates a step
      |     step : the step expression
      |
      | (args) => body
      |   Evaluates a JavaScript arrow function 
      |
      | q|exit|quit|bye
      |   Closes the REPL session and exits
      |
      | ctrl-D
      |   If in paste mode: exits paste mode and interprets provided steps
      |   Otherwise: Closes REPL session and exits${if (GwenSettings.`gwen.console.repl.autoSuggestions`) s"""
      |
      | right-arrow
      |   Press right arrow to accept auto suggestion""".stripMargin else ""}${if (GwenSettings.`gwen.console.repl.tabCompletion`) s"""
      |
      | tab
      |   Press tab key at any time for tab completion""".stripMargin else ""}${if (debug) s"""
      |
      | c|continue|resume
      |   Continue executing from current step (debug mode only)""".stripMargin else ""}
      | """.stripMargin
  }

  private def env(options: String): String = {
    Option(options) match {
      case None => ctx.asString
      case _ => options.trim match {
        case r""""(.+?)"$$$filter""" =>
          ctx.filterAtts(GwenREPL.attrFilter(filter)).asString
        case _ =>
          """Try again using: env ["filter"]"""
      }
    }
  }

  private def history: String = {
    reader.getHistory().iterator().asScala.toList map { entry => 
      s"${entry.index() + 1}: ${entry.line()}"
    } mkString System.lineSeparator()
  }

  private def verifyHistory(historyValue: String, input: String): Option[String] = {
    val history = reader.getHistory()
    val num = historyValue.toInt
    if (num < 1 || num >= history.size()) {
      Some(printError(s"No such history: $input"))
    } else {
      Some(input)
    }
  }

  private def pasteMode(): Option[String] = {
    if (paste.isEmpty) {
      paste = Some(List())
      System.out.println("REPL Console (paste mode)\n\nEnter or paste steps and press ctrl-D on empty line to evaluate..\n")
      reader.setVariable(LineReader.DISABLE_COMPLETION, true)
      Some("")
    } else {
      paste foreach { steps =>
        System.out.println(s"Exiting paste mode, ${if (steps.nonEmpty) "evaluating.." else "nothing pasted"}")
        steps.reverse map { step =>
          System.out.println(s"\n$prompt${Formatting.padTailLines(step, "      ")}\n")
          evaluate(step) tap { output => System.out.println(output) }
        }
      }
      paste = None
      pastingDocString = false
      reader.setVariable(LineReader.DISABLE_COMPLETION, false)
      Some("\nREPL Console\n\nEnter steps to evaluate or type exit to quit..")
    }
  }

  private def loadMeta(metaPath: String): Option[String] = {
    val path = metaPath match {
      case r""""(.+?)$meta"""" => meta
      case _ => metaPath
    }
    val file = new File(path)
    if (path.isEmpty || !FileIO.isMetaFile(file)) Some(printError("Please specify a file with a .meta extension (one only)"))
    else if(!file.exists()) Some(printError("File not found. Please specify an existing meta file (one only)."))
    else {
      val started = System.nanoTime()
      val metaUnit = FeatureUnit(Root, file, Nil, None, ctx.options.tagFilter)
      Try(engine.evaluateUnit(metaUnit, ctx)) match {
        case Success(spec) => 
          reader = createReader()
          spec.map(_.evalStatus) map { status =>
            printStatus(status)
          }
        case Failure(e) => Some(printError(started, e))
      }
    }
  }

  private def evalInput(input: String): Option[String] = {
    if (paste.isEmpty) {
      Some(evaluate(input))
    } else {
      if (!pastingDocString) pastingDocString = input.trim.startsWith("\"\"\"")
      else pastingDocString = input.trim != "\"\"\""
      paste = paste map { entries =>
        if (GherkinKeyword.literals.exists(reserved => input.trim.startsWith(reserved)) && !pastingDocString) {
          input :: entries
        } else {
          entries match {
            case Nil => if (input.trim.nonEmpty) List(input) else Nil
            case head :: tail => s"$head${System.lineSeparator}$input" :: tail
          }
        }
      }
      Some(input)
    }
  }

  private def evaluate(input: String): String = {
    input.trim match {
      case r"^Feature:(.*)$$$name" =>
        ctx.topScope.set(`gwen.feature.name`, name.trim)
        s"[gwen.feature.name = ${name.trim}]"
      case r"^Rule:(.*)$$$name" =>
        ctx.topScope.set(`gwen.rule.name`, name.trim)
        s"[gwen.rule.name = ${name.trim}]"
      case r"^(Scenario|Example):(.*)$$$name" =>
        if (StateLevel.scenario.equals(ctx.stateLevel)) {
          ctx.reset(StateLevel.scenario)
        }
        ctx.topScope.set(`gwen.scenario.name`, name.trim)
        s"[gwen.scenario.name = ${name.trim}]"
      case r"^Scenario(?: (Outline|Template))?:(.*)$$$name" =>
        ctx.topScope.set(`gwen.scenario.name`, name.trim)
        s"[gwen.scenario.name = ${name.trim}]"
      case r"""^#\s*language:\s*(\S+)$$$language""" =>
        Dialect.setLanguage(language)
        s"# language: $language"
      case trimmedInput =>
        evaluateStep(trimmedInput)
    }
  }

  private def evaluateStep(input: String): String = {
    val started = System.nanoTime()
    if (StepKeyword.names.exists(name => input.toLowerCase.startsWith(name.toLowerCase)) || input.contains("=>")) {
      engine.interpretStep(input, ctx) match {
        case Success(step) => printStatus(step.evalStatus)
        case Failure(error) => 
          Try(ctx.parseArrowFunction(input).map(_.apply)) match {
            case Success(result) => result.map(r => s"  ${Formatting.surroundWithQuotes(r)}")
            case Failure(e) => Some(printError(s"  ${e.getMessage}"))
          } getOrElse {
            printError(started, error)
          }
      }
    } else {
      printError(started, s"Unknown input or command")
    }
  }

  private def printError(msg: String): String = {
    printer.printStatus("  ", Failed(0, msg), Some(msg), withIcon = true, withStatusIcon = false)
  }
  private def printError(started: Long, error: Throwable): String = {
    printError(started, error.toString)
  }
  private def printError(started: Long, msg: String): String = {
    printer.printStatus("  ", Failed(System.nanoTime() - started, msg), Some(s"  $msg"), withIcon = true, withStatusIcon = false)
  }
  private def printStatus(status: EvalStatus): String = {
    printer.printStatus("  ", status, Some(s"  ${status.message}"), withIcon = true, withStatusIcon = false)
  }  

}
