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

import gwen.ConsoleWriter
import gwen.Predefs.Kestrel
import gwen.gwenSetting

/**
 * Gwen interpreter application.
 * 
 * @param interpreter
 * 			the gwen interpreter
 */
class GwenApp[T <: EnvContext](interpreter: GwenInterpreter[T]) extends App with ConsoleWriter {
    
  println("""
   __ ___      _____ _ __     _    
  / _` \ \ /\ / / _ \ '_ \   { \," 
 | (_| |\ V  V /  __/ | | | {_`/   
  \__, | \_/\_/ \___|_| |_|   `    
  |___/                            
""")

  val implVersion = Option(interpreter.getClass.getPackage.getImplementationVersion)
  
  println(s"Welcome to Gwen! ${implVersion.map(version => s" v$version").getOrElse("")}")
  println()
   
  GwenOptions.parse(interpreter.getClass().getName(), args) map { options =>
    try {
      run(options)
      System.exit(0)
    } catch {
      case e: Throwable =>
        e.printStackTrace()
        System.exit(1)
    }
  }
  
  /** 
   * Runs the interpreter with the given options
   * 
   * @param options
   * 			the command line options
   */  
  private[eval] def run(options: GwenOptions) {
    (if (options.batch) None else Some(interpreter.initialise(options))) tap { envOpt =>
      try {
        interpreter.execute(options, envOpt)
        envOpt foreach { runRepl(_) }
      } finally {
        envOpt foreach { interpreter.close(_) }
      }
    }
  }
  
  /**
   * Runs the console REPL.
   * 
   * @param env
   * 			the environment context
   */
  private[eval] def runRepl(env: EnvContext) {
    new GwenREPL((step: String) => interpreter.interpretStep(step, env), () => env.toString) tap { repl =>
      repl.run()
    }
  }
  
}
