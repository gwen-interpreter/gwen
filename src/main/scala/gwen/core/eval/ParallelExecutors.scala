/*
 * Copyright 2020-2021 Branko Juric, Brady Wood
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

import gwen.core.GwenSettings

import com.typesafe.scalalogging.LazyLogging

import java.util.concurrent.Executors
import java.util.concurrent.ExecutorService

/** 
 * Creates and provides access to an executor for parallel feature or scenario 
 * execution.
 */
object ParallelExecutors extends LazyLogging {
  
  lazy val featureInstance = createExecutor()
  lazy val scenarioInstance = createExecutor()

  /**
    * Creates and returns an executor for parallel feature and scenario execution. 
    * The number of threads in the pool is set to the `gwen.parallel.maxThreads` setting
    * value or the number of available processors if not configured.
    *
    * @return an executor instance
    */
  private def createExecutor(): ExecutorService = {
    Executors.newFixedThreadPool(GwenSettings.`gwen.parallel.maxThreads`)
  }

}