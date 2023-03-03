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
import gwen.core.behavior.FeatureMode
import gwen.core.node.GwenNode
import gwen.core.node.FeatureUnit
import gwen.core.node.Root
import gwen.core.node.gherkin.Dialect
import gwen.core.node.gherkin.GherkinKeyword
import gwen.core.node.gherkin.SpecPrinter
import gwen.core.node.gherkin.StepKeyword
import gwen.core.node.gherkin.Step
import gwen.core.report.console.ConsoleReporter
import gwen.core.state.ScopedDataStack
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

import jline.console.completer.AggregateCompleter
import jline.console.ConsoleReader
import jline.console.completer.StringsCompleter
import jline.console.history.FileHistory
import org.fusesource.jansi.Ansi._

import java.io.File
import java.io.PrintWriter

/**
  * Read-Eval-Print-Loop console.
  *
  * @author Branko Juric
  */
class GwenREPL[T <: EvalContext](val engine: EvalEngine[T], ctx: T) {

  private val outDir = GwenSettings.`gwen.outDir`
  private val history = new FileHistory(new File(outDir, ".history").getAbsoluteFile)

  private var paste: Option[List[String]] = None
  private var pastingDocString: Boolean = false
  private var debug: Boolean = false

  private val colors = ConsoleColors.isEnabled
  private val printer = new SpecPrinter(deep = false, colors)
  private def prompt = s"${if (colors) ansi.bold else ""}gwen${if (debug) s"@Breakpoint" else ""}> ${if (colors) ansi.reset else ""}"
  
  private lazy val reader = {
    new ConsoleReader() tap { reader =>
      reader.setHistory(history)
      reader.setBellEnabled(false)
      reader.setExpandEvents(false)
      reader.setPrompt(prompt)
      reader.addCompleter(new StringsCompleter((StepKeyword.names ++ List("help", "env", "history", "exit")).asJava))
      reader.addCompleter(new AggregateCompleter(new StringsCompleter(StepKeyword.names.flatMap(x => ctx.dsl.distinct.map(y => s"$x $y")).asJava)))
    }
  }

  // repl always runs in imperative mode
  Settings.setLocal("gwen.feature.mode", FeatureMode.imperative.toString)

  /** Runs the read-eval-print-loop. */
  def run(): Unit = {
    debug = false
    System.out.println("\nREPL Console")
    System.out.println("\nEnter steps to evaluate or type help for more options..")
    while(eval(read()).map(output => output tap { _ => if (paste.isEmpty) System.out.println(output) } ).nonEmpty) { }
  }

  /** Runs the read-eval-print-loop in debug mode. */
  def debug(parent: GwenNode, step: Step): Boolean = {
    var continue: Boolean = false
    debug = true
    System.out.println(s"\nPaused at${step.sourceRef.map(sref => s" $sref").getOrElse("")}")
    System.out.println(printer.prettyPrint(parent, step))
    System.out.println("\nEnter c to continue or q to quit (or type help for more options)..")
    while(
      eval(read()) map { output => 
        continue = output == "continue"
        output tap { _ => if (paste.isEmpty && !continue) System.out.println(output) } 
        output
      } filter { output => output != "continue" } nonEmpty
    ) { }
    continue
  }


  /** Reads an input string or command from the command line. */
  private def read(): String = {
    if (paste.isEmpty) System.out.println()
    reader.readLine() tap { _ => if (paste.isEmpty) System.out.println() }
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
        Some("[noop]")
      case "help" if paste.isEmpty =>
        Some(helpText())
      case r"""env(.+?)?$$$options""" if paste.isEmpty => 
        Some(env(options))
      case "history" if paste.isEmpty =>
        Some(history.toString)
      case r"""!(\d+)$$$historyValue""" if paste.isEmpty =>
        history(historyValue, input)
      case "paste" | ":paste" =>
        pasteMode()
      case "q" | "exit" | "bye" | "quit" if paste.isEmpty =>
        reader.getHistory.asInstanceOf[FileHistory].flush()
        None
      case "c" | "continue" | "resume" if debug =>
        reader.getHistory.asInstanceOf[FileHistory].flush()
        Some("continue")
      case r"""load(.*)$meta""" if paste.isEmpty =>
        loadMeta(meta.trim)
      case _ =>
        evalInput(input)
    }
  }

  private def helpText() = { 
    val help = """
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
      | q|exit|quit|bye
      |   Closes the REPL session and exits
      |
      | ctrl-D
      |   If in past mode: exits paste mode and interprets provided steps
      |   Otherwise: Closes REPL session and exits
      |
      | <tab>
      |   Press tab key at any time for tab completion
      | """.stripMargin
    if (!debug) help else help ++ s"""
      | c|continue|resume
      |   Continue executing from current step (debug mode only)
      | """.stripMargin
  }

  private def env(options: String): String = {
    Option(options) match {
      case None => ctx.visibleScopes.asString
      case _ => options.trim match {
        case r"""(-f|-a)$switch "(.+?)"$$$filter""" => switch match {
          case "-f" => ScopedDataStack(ctx.topScope.filterAtts(GwenREPL.attrFilter(filter))).asString
          case "-a" => ctx.filterAtts(GwenREPL.attrFilter(filter)).asString
        }
        case r"""(-f|-a)$$$switch""" => switch match {
          case "-f" => ctx.topScope.asString()
          case "-a" => ctx.asString
        }
        case r""""(.+?)"$$$filter""" =>
          ctx.visibleScopes.filterAtts(GwenREPL.attrFilter(filter)).asString
        case _ =>
          """Try again using: env [-a|-f] ["filter"]"""
      }
    }
  }

  private def history(historyValue: String, input: String): Option[String] = {
    val num = historyValue.toInt
    if (num < (history.size() - 1)) {
      history.get(num).toString match {
        case x if input.trim.equals(x) =>
          Some(s"Unable to refer to self history - !$historyValue")
        case s => 
          System.out.println(s"--> $s\n"); 
          eval(s)
      }
    } else {
      Some(s"No such history: !$historyValue")
    }
  }

  private def pasteMode(): Option[String] = {
    if (paste.isEmpty) {
      paste = Some(List())
      reader.setPrompt("")
      System.out.println("REPL Console (paste mode)\n\nEnter or paste steps and press ctrl-D to evaluate..\n")
      Some("")
    } else {
      paste foreach { steps =>
        System.out.println(s"\nExiting paste mode, ${if (steps.nonEmpty) "interpreting now.." else "nothing pasted"}")
        steps.reverse map { step =>
          System.out.println(s"\n$prompt${Formatting.padTailLines(step, "      ")}\n")
          evaluate(step) tap { output => System.out.println(output) }
        }
      }
      reader.setPrompt(prompt)
      paste = None
      pastingDocString = false
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
          spec.map(_.evalStatus) map { status =>
            status match {
              case _: Passed => printStatus(Loaded)
              case _ => printStatus(status)
            }
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
        ctx.topScope.set("gwen.feature.name", name.trim)
        s"[gwen.feature.name = ${name.trim}]"
      case r"^Rule:(.*)$$$name" =>
        ctx.topScope.set("gwen.rule.name", name.trim)
        s"[gwen.rule.name = ${name.trim}]"
      case r"^(Scenario|Example):(.*)$$$name" =>
        if (StateLevel.scenario.equals(ctx.stateLevel)) {
          ctx.reset(StateLevel.scenario)
        }
        ctx.topScope.set("gwen.scenario.name", name.trim)
        s"[gwen.scenario.name = ${name.trim}]"
      case r"^Scenario(?: (Outline|Template))?:(.*)$$$name" =>
        ctx.topScope.set("gwen.scenario.name", name.trim)
        s"[gwen.scenario.name = ${name.trim}]"
      case r"""^#\s*language:\s*(\S+)$$$language""" =>
        Dialect.setLanguage(language)
        s"# language: $language"
      case _ =>
        val started = System.nanoTime()
        engine.interpretStep(input, ctx) match {
          case Success(step) => 
            printStatus(step.evalStatus)
          case Failure(error) => 
            printError(started, s"$error\n\n[non-step]")
        }
    }
  }

  private def printError(msg: String): String = {
    printer.printStatus("  ", Failed(0, msg), withMessage = true)
  }
  private def printError(started: Long, error: Throwable): String = {
    printError(started, error.getMessage)
  }
  private def printError(started: Long, msg: String): String = {
    printer.printStatus("  ", Failed(System.nanoTime() - started, msg), withMessage = true)
  }
  private def printStatus(status: EvalStatus): String = {
    printer.printStatus("  ", status, withMessage = true)
  } 

}

object GwenREPL {
  /** Filters attributes containing or matching given expression (both names and values are checked). */
  def attrFilter(filter: String): PartialFunction[(String, String), Boolean] = {
    case (n, v) => n.contains(filter) || n.matches(filter) || (v != null && (v.contains(filter) || v.matches(filter)))
  }
}
