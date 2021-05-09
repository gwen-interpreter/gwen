/*
 * Copyright 2014-2021 Branko Juric, Brady Wood
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

package gwen

import gwen.core._
import gwen.core.eval.EvalContext
import gwen.core.model.ReservedKeyword
import gwen.core.model.StepKeyword
import gwen.core.model.StateLevel
import gwen.core.model.gherkin.Dialect
import gwen.core.model.state.ScopedDataStack

import scala.jdk.CollectionConverters._
import scala.util.Failure
import scala.util.Success

import jline.console.completer.AggregateCompleter
import jline.console.ConsoleReader
import jline.console.completer.StringsCompleter
import jline.console.history.FileHistory

import java.io.File

/**
  * Read-Eval-Print-Loop console.
  * 
  * @author Branko Juric
  */
class GwenREPL[T <: EvalContext](val interpreter: GwenInterpreter[T], ctx: T) {

  private val history = new FileHistory(new File(".history").getAbsoluteFile)

  private var paste: Option[List[String]] = None
  private var pastingDocString = false

  private lazy val reader = {
    new ConsoleReader() tap { reader =>
      reader.setHistory(history)
      reader.setBellEnabled(false)
      reader.setExpandEvents(false)
      reader.setPrompt("gwen> ")
      reader.addCompleter(new StringsCompleter((StepKeyword.names ++ List("help", "env", "history", "exit")).asJava))
      reader.addCompleter(new AggregateCompleter(new StringsCompleter(StepKeyword.names.flatMap(x => ctx.dsl.distinct.map(y => s"$x $y")).asJava)))
    }
  }

  /** Reads an input string or command from the command line. */
  private def read(): String = {
    if (paste.isEmpty) println()
    reader.readLine() tap { _ => if (paste.isEmpty) println() }
  }

  /**
    * Evaluates a given input string or command.
    *
    * @param input an input step or command
    * @return optional result of the command as a string
    */
  private def eval(input: String): Option[String] = ctx.withEnv { env =>
    Option(input).getOrElse(paste.map(_ => ":paste").getOrElse("exit")).trim match {
      case "" if paste.isEmpty =>
        Some("[noop]")
      case "help" if paste.isEmpty =>
        Some(helpText())
      case r"""env(.+?)?$$$options""" if paste.isEmpty => Option(options) match {
        case None => Some(env.visibleScopes.asString)
        case _ => options.trim match {
          case r"""(-f|-a)$switch "(.+?)"$$$filter""" => switch match {
            case "-f" => Some(ScopedDataStack(env.topScope.filterAtts(GwenREPL.attrFilter(filter))).asString)
            case "-a" => Some(env.filterAtts(GwenREPL.attrFilter(filter)).asString)
          }
          case r"""(-f|-a)$$$switch""" => switch match {
            case "-f" => Some(env.topScope.asString())
            case "-a" => Some(env.asString)
          }
          case r""""(.+?)"$$$filter""" =>
            Some(env.visibleScopes.filterAtts(GwenREPL.attrFilter(filter)).asString)
          case _ =>
            Some("""Try again using: env [-a|-f] ["filter"]""")
        }
      }
      case "history" if paste.isEmpty =>
        Some(history.toString)
      case r"""!(\d+)$$$historyValue""" if paste.isEmpty =>
        val num = historyValue.toInt
        if (num < (history.size() - 1)) {
          history.get(num).toString match {
            case x if input.trim.equals(x) =>
              Some(s"Unable to refer to self history - !$historyValue")
            case s => println(s"--> $s\n"); eval(s)
          }
        } else {
          Some(s"No such history: !$historyValue")
        }
      case "paste" | ":paste" =>
        if (paste.isEmpty) {
          paste = Some(List())
          reader.setPrompt("")
          println("REPL Console (paste mode)\n\nEnter or paste steps and press ctrl-D to evaluate..\n")
          Some("")
        } else {
          paste foreach { steps =>
            println(s"\nExiting paste mode, ${if (steps.nonEmpty) "interpreting now.." else "nothing pasted"}")
            steps.reverse map { step =>
              println(s"\ngwen> ${Formatting.padTailLines(step, "      ")}\n")
              evaluateInput(step) tap { output => println(output) }
            }
          }
          reader.setPrompt("gwen> ")
          paste = None
          pastingDocString = false
          Some("\nREPL Console\n\nEnter steps to evaluate or type exit to quit..")
        }
      case "exit" | "bye" | "quit" if paste.isEmpty =>
        reader.getHistory.asInstanceOf[FileHistory].flush()
        None
      case _ =>
        if (paste.isEmpty) {
          Some(evaluateInput(input))
        } else {
          if (!pastingDocString) pastingDocString = input.trim.startsWith("\"\"\"")
          else pastingDocString = input.trim != "\"\"\""
          paste = paste map { entries =>
            if (ReservedKeyword.literals.exists(reserved => input.trim.startsWith(reserved)) && !pastingDocString) {
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
  }

  private def evaluateInput(input: String): String = ctx.withEnv { env =>
    input.trim match {
      case r"^Feature:(.*)$$$name" =>
        env.topScope.set("gwen.feature.name", name.trim)
        s"[gwen.feature.name = ${name.trim}]"
      case r"^Rule:(.*)$$$name" =>
        env.topScope.set("gwen.rule.name", name.trim)
        s"[gwen.rule.name = ${name.trim}]"
      case r"^(Scenario|Example):(.*)$$$name" =>
        if (StateLevel.scenario.equals(env.stateLevel)) {
          ctx.reset(StateLevel.scenario)
        }
        env.topScope.set("gwen.scenario.name", name.trim)
        s"[gwen.scenario.name = ${name.trim}]"
      case r"^Scenario(?: (Outline|Template))?:(.*)$$$name" =>
        env.topScope.set("gwen.scenario.name", name.trim)
        s"[gwen.scenario.name = ${name.trim}]"
      case r"""^#\s*language:\s*(\S+)$$$language""" =>
        Dialect.setLanguage(language)
        s"# language: $language"
      case _ =>
        interpreter.interpretStep(input, ctx) match {
          case Success(step) => s"\n[${step.evalStatus.status}]"
          case Failure(error) => s"$error\n\n[non-step]"
        }
    }
  }

  /** Runs the read-eval-print-loop. */
  def run(): Unit = {
    println("\nREPL Console\n")
    println("Enter steps to evaluate or type exit to quit..")
    while(eval(read()).map(output => output tap { _ => if (paste.isEmpty) println(output) } ).nonEmpty) { }
  }
  
  private def helpText() = """
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
    | Given|When|Then|And|But <step>
    |   Evaluates a step
    |     step : the step expression
    | 
    | exit|quit|bye
    |   Closes the REPL session and exits
    |
    | ctrl-D
    |   If in past mode: exits paste mode and interprets provided steps
    |   Otherwise: Closes REPL session and exits
    | 
    | <tab> 
    |   Press tab key at any time for tab completion
    | """.stripMargin
}

object GwenREPL {
  /** Filters attributes containing or matching given expression (both names and values are checked). */
  def attrFilter(filter: String): PartialFunction[(String, String), Boolean] = { 
    case (n, v) => n.contains(filter) || n.matches(filter) || (v != null && (v.contains(filter) || v.matches(filter)))
  }
}

