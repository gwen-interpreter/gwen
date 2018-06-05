/*
 * Copyright 2015-2018 Branko Juric, Brady Wood
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

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import java.util.Properties
import gwen.errors.MissingPropertyException
import gwen.errors.UnsupportedLocalSettingException

class SettingsTest extends FlatSpec with Matchers {

  "user.home system property" should "be available" in {
    Settings.synchronized {
      Settings.getOpt("user.home") should not be None
    }
  }
  
  "inline properties" should "resolve" in {
    val props = new Properties()
    props.put("prop.host", "localhost")
    withSetting("prop.port", "8090") {
      props.put("prop.url", "http://${prop.host}:${prop.port}/howdy")
      Settings.resolve(props.getProperty("prop.url"), props) should be ("http://localhost:8090/howdy")
    }
  }
  
  "nested properties" should "resolve" in {
    val props = new Properties()
    props.put("prop.host", "localhost")
    withSetting("prop.host.port", "${prop.host}:8090") {
      props.put("prop.url", "http://${prop.host.port}/howdy")
      Settings.resolve(props.getProperty("prop.url"), props) should be ("http://localhost:8090/howdy")
    }
  }
  
  "existing system property" should "not be overriden when override flag is false" in {
    withSetting("gwen.web.browser", "firefox") {
      Settings.add("gwen.web.browser", "chrome", overrideIfExists = false)
      Settings.get("gwen.web.browser") should be ("firefox")
    }
  }
  
  "existing system property" should "be overriden when override flag is true" in {
    withSetting("gwen.web.browser", "firefox") {
      Settings.add("gwen.web.browser", "chrome", overrideIfExists = true)
      Settings.get("gwen.web.browser") should be ("chrome")
    }
  }

  "missing nested property" should "should report missing property error" in {
    val props = new Properties()
    props.put("prop.host", "localhost")
    withSetting("prop.host.port", "${prop.host.missing}:8090") {
      props.put("prop.url", "http://${prop.host.port}/howdy")
      intercept[MissingPropertyException] {
        Settings.resolve(props.getProperty("prop.url"), props)
      }
    }
  }

  "Settings.entries" should "return all system and local entries" in {
    withSetting("gwen.web.browser", "chrome") {
      val size = Settings.size
      (size > 0) should be (true)
      Settings.entries.size should be (size)
      try {
        Settings.get("gwen.web.browser") should be ("chrome")
        Settings.entries.filter(_._1 == "gwen.web.browser").values.head should be ("chrome")
        Settings.setLocal("gwen.web.browser", "safari")  // override sys property
        Settings.get("gwen.web.browser") should be ("safari")
        Settings.contains("gwen.web.browser") should be (true)
        Settings.entries.filter(_._1 == "gwen.web.browser").values.head should be ("safari")
        Settings.entries.size should be (size)
        Settings.entries.size should be (Settings.size)
      } finally {
        Settings.clearLocal("gwen.web.browser")
        Settings.get("gwen.web.browser") should be ("chrome") // sys property still thre
        Settings.entries.size should be (size)
      }
    }
  }

  "Settings.names" should "return all system and local names" in {
    withSetting("gwen.web.browser", "chrome") {
      val size = Settings.size
      (size > 0) should be (true)
      Settings.names.size should be (size)
      try {
        Settings.get("gwen.web.browser") should be ("chrome")
        Settings.setLocal("gwen.web.browser", "safari")  // override sys property
        Settings.get("gwen.web.browser") should be ("safari")
        Settings.contains("gwen.web.browser") should be (true)
        Settings.names.size should be (size)
        Settings.names.size should be (Settings.size)
      } finally {
        Settings.clearLocal("gwen.web.browser")
        Settings.get("gwen.web.browser") should be ("chrome") // sys property still thre
        Settings.names.size should be (size)
      }
    }
  }

  "Settings.setLocal" should "not support non gwen settings" in {
    withSetting("host.name", "localhost") {
      intercept[UnsupportedLocalSettingException] {
        Settings.setLocal("host.port", "8080")
      }
    }
  }

  "multi name-value properties" should "should merge" in {
    withSetting("gwen.web.chrome.prefs", "alternate_error_pages.enabled=false,session.length_limit=9999999999,other=?") {
      withSetting("gwen.web.chrome.pref.download.prompt_for_download", "false") {
        withSetting("gwen.web.chrome.pref.download.default_directory", "downloads") {
          val props = Settings.findAllMulti("gwen.web.chrome.prefs", "gwen.web.chrome.pref")
          props("alternate_error_pages.enabled") should be ("false")
          props("session.length_limit") should be ("9999999999")
          props("other") should be ("?")
          props("download.prompt_for_download") should be ("false")
          props("download.default_directory") should be ("downloads")
        }
      }
    }
  }

  "multi value properties" should "should merge" in {
    withSetting("gwen.web.chrome.args", "--touch-events=disabled,--incognito,--other") {
      withSetting("gwen.web.chrome.args.0", "--ignore-certificate-errors") {
        withSetting("gwen.web.chrome.args.1", "--window-size=1920,1080") {
          val values = Settings.findAllMulti("gwen.web.chrome.args")
          values.size should be (5)
          values.contains("--touch-events=disabled") should be (true)
          values.contains("--incognito") should be (true)
          values.contains("--other") should be (true)
          values.contains("--ignore-certificate-errors") should be (true)
          values.contains("--window-size=1920,1080") should be (true)
        }
      }
    }
  }

  private def withSetting[T](name: String, value: String)(f: => T):T = {
    Settings.synchronized {
      val original = Settings.getOpt(name)
      try {
        Settings.set(name, value)
        f
      } finally {
        original.fold(Settings.clear(name)) { v =>
          Settings.set(name, v)
        }
      }
    }
  }

}