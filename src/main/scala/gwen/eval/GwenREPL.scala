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

package gwen.eval

import java.io.File
import scala.collection.JavaConversions.seqAsJavaList
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import gwen.ConsoleWriter
import gwen.Predefs.Kestrel
import gwen.dsl.Step
import gwen.dsl.StepKeyword
import gwen.gwenSetting
import jline.console.history.FileHistory
import jline.console.ConsoleReader
import jline.console.completer.StringsCompleter
import gwen.dsl.prettyPrint
import play.api.libs.json.Json

/**
 * Read-Eval-Print-Loop console.
 * 
 * @author Branko Juric
 */
class GwenREPL[T <: EnvContext](val interpreter: GwenInterpreter[T], val env: T) extends ConsoleWriter {

  private val history = new FileHistory(new File(".history").getAbsoluteFile())
  
  private lazy val reader = new ConsoleReader() tap { reader =>
    reader.setHistory(history)
    reader.setBellEnabled(false)
    reader.setExpandEvents(false)
    reader.setPrompt("gwen>")
    reader.addCompleter(new StringsCompleter(StepKeyword.values.map(_.toString).toList ++ List("env", "exit")))
  }
  
  /**
   * Reads an input string or command from the command line.
   */
  private def read(): String = {
    println()
    reader.readLine() tap { input => println() }
  }
  
  /**
   * Evaluates a given input string or command.
   * 
   * @param input
   * 			an input step or command
   */
  private def eval(input: String): Option[String] = input.trim match {
    case "" => Some("[noop]")
    case "env" | "env -v" | "env -visible" => 
      Some(Json.prettyPrint(env.visibleJson))
    case "env -f" | "env -feature" => 
      Some(Json.prettyPrint(env.featureScope.json))
    case "env -a" | "env -all" => 
      Some(Json.prettyPrint(env.json))
    case "exit" | "bye" | "quit" => 
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
  
  /**
   * Runs the read-eval-print-loop.
   */
  def run() {
    println()
    println("REPL Console")
    println()
    println("Enter steps to evaluate or type exit to quit..")
    while(eval(read()).map(println).isDefined) { }
  }
  
}

