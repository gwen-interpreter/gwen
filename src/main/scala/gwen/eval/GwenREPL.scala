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

package gwen.eval

import java.io.File
import scala.collection.JavaConversions.seqAsJavaList
import scala.util.Failure
import scala.util.Success
import gwen.Predefs.Kestrel
import gwen.Predefs.RegexContext
import gwen.dsl.StepKeyword
import jline.console.ConsoleReader
import jline.console.completer.StringsCompleter
import jline.console.history.FileHistory
import play.api.libs.json.Json
import scala.collection.JavaConversions
import jline.console.completer.AggregateCompleter

/**
  * Read-Eval-Print-Loop console.
  * 
  * @author Branko Juric
  */
class GwenREPL[T <: EnvContext](val interpreter: GwenInterpreter[T], val env: T) {

  private val history = new FileHistory(new File(".history").getAbsoluteFile())
  
  private lazy val reader = new ConsoleReader() tap { reader =>
    reader.setHistory(history)
    reader.setBellEnabled(false)
    reader.setExpandEvents(false)
    reader.setPrompt("gwen>")
    reader.addCompleter(new StringsCompleter(StepKeyword.literals ++ List("help", "env", "history", "exit")))
    reader.addCompleter(new AggregateCompleter(new StringsCompleter(StepKeyword.literals.flatMap(x => env.dsl.distinct.map(y => s"$x $y")))))
  }
  
  /** Reads an input string or command from the command line. */
  private def read(): String = {
    println()
    reader.readLine() tap { input => println() }
  }
  
  /**
    * Evaluates a given input string or command.
    * 
    * @param input an input step or command
    * @return optional result of the command as a string
    */
  private def eval(input: String): Option[String] = input.trim match {
    case "" => Some("[noop]")
    case "help" =>
      Some(helpText())
    case r"""env(.+?)?$$$options""" => Option(options) match {
      case None => Some(Json.prettyPrint(env.visibleScopes.json))
      case _ => options.trim match {
        case r"""(-f|-a)$switch "(.+?)"$$$filter""" => switch match {
          case "-f" => Some(Json.prettyPrint(ScopedDataStack(env.featureScope.filterAtts(GwenREPL.attrFilter(filter))).json))
          case "-a" => Some(Json.prettyPrint(env.filterAtts(GwenREPL.attrFilter(filter)).json))
        }
        case r"""(-f|-a)$$$switch""" => switch match {
          case "-f" => Some(Json.prettyPrint(env.featureScope.json))
          case "-a" => Some(Json.prettyPrint(env.json))
        }
        case r""""(.+?)"$$$filter""" => 
          Some(Json.prettyPrint(env.visibleScopes.filterAtts(GwenREPL.attrFilter(filter)).json))
        case _ =>
          Some("""Try again using: env [-a|-f] ["filter"]""")
      }
    }
    case r"history" =>
      Some(history.toString())
    case r"!(\d+)$$$historyValue" =>
      val num = historyValue.toInt
      if (num < (history.size() - 1)) {
        (history.get(num).toString) match {
          case x if input.equals(x) => 
            Some(s"Unable to refer to self history - !$historyValue")
          case s => println(s"--> $s\n"); eval(s)
        }
      } else {
        Some(s"No such history: !$historyValue")
      }
    case r"exit|bye|quit" => 
      reader.getHistory().asInstanceOf[FileHistory].flush()
      None
    case _ =>
      Some {
        interpreter.interpretStep(input, env) match { 
          case Success(step) => s"\n[${step.evalStatus.status}]"
          case Failure(error) => s"\n${error}\n\n[failure]"
        }
      }
  }
  
  /** Runs the read-eval-print-loop. */
  def run() {
    println()
    println("REPL Console")
    println()
    println("Enter steps to evaluate or type exit to quit..")
    while(eval(read()).map(println).isDefined) { }
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

