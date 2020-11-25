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

package gwen.dsl

import gwen._

import com.typesafe.scalalogging.LazyLogging
import io.cucumber.gherkin.GherkinDialect
import io.cucumber.gherkin.GherkinDialectProvider

object Dialect extends LazyLogging {

  private val dialectHolder = new ThreadLocal[GherkinDialect]() {
    override protected def initialValue: GherkinDialect = {
      logger.info(s"Default Gherkin feature dialect is: $defaultLanguage")
      new GherkinDialectProvider(defaultLanguage).getDefaultDialect
    }
  }

  private def defaultLanguage = GwenSettings.`gwen.feature.dialect`

  def instance: GherkinDialect = dialectHolder.get

  def withLanguage[T](language: String)(body: =>T): T = {
    val prevLanguage = instance.getLanguage()
    if (language != prevLanguage) { 
      setLanguage(language) 
    }
    try {
      body
    } finally {
      if (language != prevLanguage) { 
        setLanguage(prevLanguage)
      }
    }
  }

  def setLanguage(lang: String): Unit = {
    def language = if (lang != null && lang != defaultLanguage) {
      lang
    } else {
      defaultLanguage
    }
    logger.info(s"Setting Gherkin feature dialect to: $language")
    dialectHolder.set(new GherkinDialectProvider(language).getDefaultDialect)
  }
}