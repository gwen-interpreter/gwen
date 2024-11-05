/*
 * Copyright 2019-2024 Brady Wood, Branko Juric
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

package gwen.core

import gwen.core.state.StateLevel

import com.typesafe.config.ConfigFactory
import org.scalatest.prop.TableDrivenPropertyChecks.Table
import org.scalatest.flatspec.AnyFlatSpec
import java.io.File

abstract class BaseTest extends AnyFlatSpec {

  val levels = Table ( StateLevel.feature.toString, StateLevel.scenario.toString )

  def withSetting[T](name: String, value: String)(body: => T):T = {
    Settings.exclusively {
      val original = Settings.getOpt(name)
      try {
        Settings.set(name, value)
        body
      } finally {
        original foreach { v =>
          Settings.set(name, v)
        }
      }
    }
  }
  
}