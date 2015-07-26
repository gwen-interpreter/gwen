/*
 * Copyright 2015 Branko Juric, Brady Wood
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

/**
  * The context in which all engine instructions execute. The purpose of this 
  * context is to provide a place where common logic can be inserted before, 
  * after, or around an instruction.   
  */
trait ExecutionContext {
  this: EnvContext =>
  
  /**
   * Intercepts an engine instruction and conditionally executes it.
   * 
   * @param instruction the engine instruction
   */
  def execute[T](instruction: => T): Option[T] = 
    if (!isDryRun) Some(instruction) else None
  
}