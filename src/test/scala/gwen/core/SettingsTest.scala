/*
 * Copyright 2015-2021 Branko Juric, Brady Wood
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

import gwen.core.Errors.MissingSettingException
import gwen.core.Errors.UnsupportedLocalSettingException

import org.scalatest.matchers.should.Matchers

import scala.util.chaining._

import java.io.File
import java.util.Properties

class SettingsTest extends BaseTest with Matchers {

  val targetDir: File = new File("target/SettingsTest") tap { _.mkdirs() }

  "user.home system property" should "be available" in {
    Settings.synchronized {
      Settings.getOpt("user.home") should not be None
    }
  }

  "HOME env var should" should "be available" in {
    Settings.synchronized {
      Settings.getOpt("env.HOME") should be (Some(sys.env("HOME")))
    }
  }
  
  "inline properties" should "resolve" in {
    val propFile = new File(targetDir, "inline.properties")
    propFile.delete()
    propFile.writeText(
      """|prop.host=localhost
         |prop.port=8090
         |prop.url=http://${prop.host}:${prop.port}/howdy
         |""".stripMargin
    )
    Settings.exclusively {
      try {
        Settings.init(List(propFile))
        Settings.get("prop.url") should be ("http://localhost:8090/howdy")
      } finally {
        sys.props -= "prop.host"
        sys.props -= "prop.host.port"
        sys.props -= "prop.url"
      }
    }
  }
  
  "nested properties" should "resolve" in {
    val propFile = new File(targetDir, "nested.properties")
    propFile.delete()
    propFile.writeText(
      """|prop.host=localhost
         |prop.host.port=${prop.host}:8090
         |prop.url=http://${prop.host.port}/howdy
         |""".stripMargin
    )
    Settings.exclusively {
      try {
        Settings.init(List(propFile))
        Settings.get("prop.url") should be ("http://localhost:8090/howdy")
      } finally {
        sys.props -= "prop.host"
        sys.props -= "prop.host.port"
        sys.props -= "prop.url"
      }
    }
  }

  "missing nested property" should "report missing setting error" in {
    val propFile = new File(targetDir, "missing-nested.properties")
    propFile.delete()
    propFile.writeText(
      """|prop.host=localhost
         |prop.host.port=${prop.host.missing}:8090
         |prop.url=http://${prop.host.port}/howdy
         |""".stripMargin
    )
    intercept[MissingSettingException] {
      Settings.exclusively {
        try {
          Settings.init(List(propFile))
        } finally {
          sys.props -= "prop.host"
          sys.props -= "prop.host.port"
          sys.props -= "prop.url"
        }
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

  "multi name-value properties" should "merge" in {
    withSetting("gwen.web.chrome.prefs", "alternate_error_pages.enabled=false,session.length_limit=9999999999,other=?") {
      withSetting("gwen.web.chrome.pref.download.prompt_for_download", "false") {
        withSetting("gwen.web.chrome.pref.download.default_directory", "downloads") {
          val props = Settings.getMap("gwen.web.chrome.prefs", "gwen.web.chrome.pref")
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
          val values = Settings.getList("gwen.web.chrome.args")
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

  "masked conf setting" should "yield masked value" in {
    val confFile = new File(targetDir, "masked.conf")
    confFile.delete()
    confFile.writeText(
      """|my {
         |  secret {
         |    conf {
         |      "setting:masked" = "secret"
         |    }
         |  }
         |}
         |""".stripMargin
    )
    Settings.exclusively {
      Settings.init(List(confFile))
      Settings.get("my.secret.conf.setting").contains("*****") should be (true)
    }
  }

  "nested masked conf setting" should "should resolve" in {
    val confFile = new File(targetDir, "nested-masked.conf")
    confFile.delete()
    confFile.writeText(
      """|my {
         |  website {
         |    "token:masked" = "secret"
         |    url = "https://host?token=${my.website.token}"
         |  }
         |}
         |""".stripMargin
    )
    Settings.exclusively {
      Settings.init(List(confFile))
      Settings.get("my.website.token").contains("*****") should be (true)
      Settings.get("my.website.url").contains("https://host?token=*****") should be (true)
    }
  }

  "masked json setting" should "yield masked value" in {
    val confFile = new File(targetDir, "masked.json")
    confFile.delete()
    confFile.writeText(
      """|{
         |  "my": {
         |    "secret": {
         |      "json": {
         |        "setting:masked": "secret"
         |      }
         |    }
         |  }
         |}
         |""".stripMargin
    )
    Settings.exclusively {
      Settings.init(List(confFile))
      Settings.get("my.secret.json.setting").contains("*****") should be (true)
    }
  }

  "masked system property" should "yield masked value" in {
    withSetting("my.secret.prop:masked", "secret") {
      Settings.init()
      Settings.get("my.secret.prop").contains("*****") should be (true)
    }
  }

  "nested keys in conf file" should "resolve" in {
    val confFile = new File(targetDir, "nestedkeys.conf")
    confFile.delete()
    confFile.writeText(
      """|"a.b"=1
         |"a.b.c"=2
         |""".stripMargin
    )
    Settings.exclusively {
      try {
        Settings.init(List(confFile))
        Settings.get("a.b") should be ("1")
        Settings.get("a.b.c") should be ("2")
      } finally {
        sys.props -= "a.b"
        sys.props -= "a.b.c"
      }
    }
  }

  "nested keys in properties file" should "resolve" in {
    val propFile = new File(targetDir, "nestedkeys.properties")
    propFile.delete()
    propFile.writeText(
      """|d.e=3
         |d.e.f=4
         |""".stripMargin
    )
    Settings.exclusively {
      Settings.init(List(propFile))
      Settings.get("d.e") should be ("3")
      Settings.get("d.e.f") should be ("4")
    }
  }

  "nested keys in system properties" should "resolve" in {
    withSetting("g.h", "5") {
      withSetting("g.h.i", "6") {
        Settings.get("g.h") should be ("5")
        Settings.get("g.h.i") should be ("6")
      }
    }
  }

  "nested keys in properties file through system property" should "resolve" in {
    val propFile = new File(targetDir, "nestedkeys.properties")
    propFile.delete()
    withSetting("j.k", "7") {
      propFile.writeText(
        """|j.k.l=8
           |""".stripMargin
      )
      Settings.exclusively {
        Settings.init(List(propFile))
        Settings.get("j.k") should be ("7")
        Settings.get("j.k.l") should be ("8")
      }
    }
  }

}
