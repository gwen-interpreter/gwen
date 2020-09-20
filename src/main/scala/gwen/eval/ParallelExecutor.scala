/*
 * Copyright 2020 Branko Juric, Brady Wood
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

import gwen.dsl.StateLevel
import java.util.concurrent.Executors
import java.util.concurrent.ExecutorService

/** 
 * Creates and provides access to an executor for parallel feature or scenario 
 * execution based on the number of available processors.
 */
object ParallelExecutor {

  private val processors = Runtime.getRuntime().availableProcessors()

  /**
    * Creates and returns an executor. The number of threads in the pool is set to the 
    * total number of available processors when executing features in parallel. When 
    * executing scenarios in parallel however, it is set to half that value for both 
    * features and scenarios so that both are able to execute.
    *
    * @param parallelScenarios true for executing parallel scenarios, false for features
    * @return an executor instance
    */
  def createInstance(parallelScenarios: Boolean): ExecutorService = {
    if (StateLevel.isScenario && parallelScenarios) {
      Executors.newWorkStealingPool(processors / 2)
    } else {
      Executors.newWorkStealingPool()
    }
  }

}