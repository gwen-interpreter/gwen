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

package gwen

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import java.util.Properties

class SettingsTest extends FlatSpec with Matchers {

  "user.home system property" should "be available" in {
    Settings.getOpt("user.home") should not be (None)
  }
  
  "inline properties" should "resolve" in {
    val props = new Properties()
    props.put("prop.host", "localhost")
    sys.props.put("prop.port", "8090")
    props.put("prop.url", "http://${prop.host}:${prop.port}/howdy")
    
    Settings.resolve(props.getProperty("prop.url"), props) should be ("http://localhost:8090/howdy")
  }
  
  "nested properties" should "resolve" in {
    val props = new Properties()
    props.put("prop.host", "localhost")
    sys.props.put("prop.host.port", "${prop.host}:8090")
    props.put("prop.url", "http://${prop.host.port}/howdy")
    
    Settings.resolve(props.getProperty("prop.url"), props) should be ("http://localhost:8090/howdy")
  }
}