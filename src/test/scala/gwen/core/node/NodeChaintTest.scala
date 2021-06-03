/*
 * Copyright 2021 Branko Juric, Brady Wood
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

package gwen.core.node

import gwen.core._
import gwen.core.node.gherkin.GherkinParser
import gwen.core.node.gherkin.SpecNormaliser
import gwen.core.node.gherkin.TagFilter

import gwen.core.state.DataRecord

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatestplus.mockito.MockitoSugar

import java.io.File

class NodeChainTest extends FlatSpec with Matchers with MockitoSugar with SpecNormaliser with GherkinParser with TestModel {

  private val mockTagFilter = mock[TagFilter]

  "Node path" should "be blank on empty chain" in {
    val chain = new NodeChain()
    chain.nodePath should be ("")
  }

  "Node path" should "be generated on chain with Root" in {
    val chain = new NodeChain()
    chain.push(Root)
    chain.nodePath should be ("/")
  }

  "Node path" should "be generated on chain with Unit" in {
    val chain = new NodeChain()
    val unit = FeatureUnit(Root, new File("path/to/file.feature"), Nil, None, mockTagFilter, None)
    chain.push(unit)
    chain.nodePath should be ("/path/to/file.feature")
  }

  "Node path" should "be generated on chain with Root and Unit" in {
    val chain = new NodeChain()
    val unit = FeatureUnit(Root, new File("path/to/file.feature"), Nil, None, mockTagFilter, None)
    chain.push(Root)
    chain.push(unit)
    chain.nodePath should be ("/path/to/file.feature")
  }

    "Node path" should "be generated on chain with Root and Unit with data record" in {
    val chain = new NodeChain()
    val dataRecord = new DataRecord(new File("data.csv"), 2, Nil)
    val unit = FeatureUnit(Root, new File("path/to/file.feature"), Nil, Some(dataRecord), mockTagFilter, None)
    chain.push(Root)
    chain.push(unit)
    chain.nodePath should be ("/path/to/file.feature")
  }

  "Node paths" should "be generared for all nodes in normalised feature with scenario outlines" in {

    val featureString = """
    Feature: Outline

    Background: background
       Given background step 1

    @UnitTest
    Scenario Outline: Joining <string 1> and <string 2> should yield <result>

    Substituting..
    string 1 = <string 1>
    string 2 = <string 2>
    result = <result>

    Given string 1 is "<string 1>"
      And string 2 is "<string 2>"
     When I join the two strings
     Then the result should be "<result>"

    Examples: Compound words

      | string 1 | string 2 | result     |
      | basket   | ball     | basketball |
      | any      | thing    | anything   |

    Examples: Nonsensical compound words

      Words that don't make any sense at all
      (for testing multiple examples)

      | string 1 | string 2 | result   |
      | howdy    | doo      | howdydoo |
      | yep      | ok       | yepok    |

    Examples:

      | string 1 | string 2 | result   |
      | ding     | dong     | dingdong |
    """

    val rootDir: File = new File("target" +  File.separator + this.getClass.getSimpleName)
    val file = new File(rootDir.getPath, "outline.feature")
    file.getParentFile.mkdirs()
    file.createNewFile()
    file.writeText(featureString)

    val spec = normaliseSpec(parseSpec(file).get, None)
    val chain = new NodeChain()
    val unit = FeatureUnit(Root, file, Nil, None, mockTagFilter, None)

    chain.nodePath should be ("")
    chain.push(Root)
    chain.nodePath should be ("/")
    chain.push(unit)
    chain.nodePath should be ("/target/NodeChainTest/outline.feature")
    chain.push(spec)
    chain.nodePath should be ("/target/NodeChainTest/outline.feature/Outline[1]")

    val outline = spec.scenarios(0)

    chain.push(outline)
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]""")
    chain.push(outline.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/string 1 is "<string 1>"[1]""")
    chain.pop()
    chain.push(outline.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/string 2 is "<string 2>"[1]""")
    chain.pop()
    chain.push(outline.steps(2))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/I join the two strings[1]""")
    chain.pop()
    chain.push(outline.steps(3))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/the result should be "<result>"[1]""")
    chain.pop()

    val examples = outline.examples
    val example1 = examples(0)

    chain.push(example1)
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Compound words[1]""")

    val scenario1 = example1.scenarios(0)
    chain.push(scenario1)
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Compound words[1]/Joining basket and ball should yield basketball -- Example 1.1 Compound words[1]""")
    chain.push(scenario1.background.get)
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Compound words[1]/Joining basket and ball should yield basketball -- Example 1.1 Compound words[1]/background[1]""")
    chain.push(scenario1.background.get.steps(0))
    chain.nodePath should be("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Compound words[1]/Joining basket and ball should yield basketball -- Example 1.1 Compound words[1]/background[1]/background step 1[1]""")
    chain.pop()
    chain.pop()
    chain.push(scenario1.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Compound words[1]/Joining basket and ball should yield basketball -- Example 1.1 Compound words[1]/string 1 is "basket"[1]""")
    chain.pop()
    chain.push(scenario1.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Compound words[1]/Joining basket and ball should yield basketball -- Example 1.1 Compound words[1]/string 2 is "ball"[1]""")
    chain.pop()
    chain.push(scenario1.steps(2))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Compound words[1]/Joining basket and ball should yield basketball -- Example 1.1 Compound words[1]/I join the two strings[1]""")
    chain.pop()
    chain.push(scenario1.steps(3))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Compound words[1]/Joining basket and ball should yield basketball -- Example 1.1 Compound words[1]/the result should be "basketball"[1]""")
    chain.pop()
    chain.pop()

    val scenario2 = example1.scenarios(1)
    chain.push(scenario2)
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Compound words[1]/Joining any and thing should yield anything -- Example 1.2 Compound words[1]""")
    chain.push(scenario2.background.get)
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Compound words[1]/Joining any and thing should yield anything -- Example 1.2 Compound words[1]/background[1]""")
    chain.push(scenario2.background.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Compound words[1]/Joining any and thing should yield anything -- Example 1.2 Compound words[1]/background[1]/background step 1[1]""")
    chain.pop()
    chain.pop()
    chain.push(scenario2.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Compound words[1]/Joining any and thing should yield anything -- Example 1.2 Compound words[1]/string 1 is "any"[1]""")
    chain.pop()
    chain.push(scenario2.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Compound words[1]/Joining any and thing should yield anything -- Example 1.2 Compound words[1]/string 2 is "thing"[1]""")
    chain.pop()
    chain.push(scenario2.steps(2))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Compound words[1]/Joining any and thing should yield anything -- Example 1.2 Compound words[1]/I join the two strings[1]""")
    chain.pop()
    chain.push(scenario2.steps(3))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Compound words[1]/Joining any and thing should yield anything -- Example 1.2 Compound words[1]/the result should be "anything"[1]""")
    chain.pop()
    chain.pop()
    chain.pop()

    val example2 = examples(1)
    chain.push(example2)
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Nonsensical compound words[1]""")
    
    val scenario3 = example2.scenarios(0)
    chain.push(scenario3)
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Nonsensical compound words[1]/Joining howdy and doo should yield howdydoo -- Example 2.1 Nonsensical compound words[1]""")
    chain.push(scenario3.background.get)
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Nonsensical compound words[1]/Joining howdy and doo should yield howdydoo -- Example 2.1 Nonsensical compound words[1]/background[1]""")
    chain.push(scenario3.background.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Nonsensical compound words[1]/Joining howdy and doo should yield howdydoo -- Example 2.1 Nonsensical compound words[1]/background[1]/background step 1[1]""")
    chain.pop()
    chain.pop()
    chain.push(scenario3.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Nonsensical compound words[1]/Joining howdy and doo should yield howdydoo -- Example 2.1 Nonsensical compound words[1]/string 1 is "howdy"[1]""")
    chain.pop()
    chain.push(scenario3.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Nonsensical compound words[1]/Joining howdy and doo should yield howdydoo -- Example 2.1 Nonsensical compound words[1]/string 2 is "doo"[1]""")
    chain.pop()
    chain.push(scenario3.steps(2))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Nonsensical compound words[1]/Joining howdy and doo should yield howdydoo -- Example 2.1 Nonsensical compound words[1]/I join the two strings[1]""")
    chain.pop()
    chain.push(scenario3.steps(3))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Nonsensical compound words[1]/Joining howdy and doo should yield howdydoo -- Example 2.1 Nonsensical compound words[1]/the result should be "howdydoo"[1]""")
    chain.pop()
    chain.pop()

    val scenario4 = example2.scenarios(1)
    chain.push(scenario4)
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Nonsensical compound words[1]/Joining yep and ok should yield yepok -- Example 2.2 Nonsensical compound words[1]""")
    chain.push(scenario4.background.get)
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Nonsensical compound words[1]/Joining yep and ok should yield yepok -- Example 2.2 Nonsensical compound words[1]/background[1]""")
    chain.push(scenario4.background.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Nonsensical compound words[1]/Joining yep and ok should yield yepok -- Example 2.2 Nonsensical compound words[1]/background[1]/background step 1[1]""")
    chain.pop()
    chain.pop()
    chain.push(scenario4.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Nonsensical compound words[1]/Joining yep and ok should yield yepok -- Example 2.2 Nonsensical compound words[1]/string 1 is "yep"[1]""")
    chain.pop()
    chain.push(scenario4.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Nonsensical compound words[1]/Joining yep and ok should yield yepok -- Example 2.2 Nonsensical compound words[1]/string 2 is "ok"[1]""")
    chain.pop()
    chain.push(scenario4.steps(2))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Nonsensical compound words[1]/Joining yep and ok should yield yepok -- Example 2.2 Nonsensical compound words[1]/I join the two strings[1]""")
    chain.pop()
    chain.push(scenario4.steps(3))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Nonsensical compound words[1]/Joining yep and ok should yield yepok -- Example 2.2 Nonsensical compound words[1]/the result should be "yepok"[1]""")
    chain.pop()
    chain.pop()
    chain.pop()

    val example3 = examples(2)
    chain.push(example3)
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/""")
    
    val scenario5 = example3.scenarios(0)
    chain.push(scenario5)
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]//Joining ding and dong should yield dingdong -- Example 3.1[1]""")
    chain.push(scenario5.background.get)
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]//Joining ding and dong should yield dingdong -- Example 3.1[1]/background[1]""")
    chain.push(scenario5.background.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]//Joining ding and dong should yield dingdong -- Example 3.1[1]/background[1]/background step 1[1]""")
    chain.pop()
    chain.pop()
    chain.push(scenario5.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]//Joining ding and dong should yield dingdong -- Example 3.1[1]/string 1 is "ding"[1]""")
    chain.pop()
    chain.push(scenario5.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]//Joining ding and dong should yield dingdong -- Example 3.1[1]/string 2 is "dong"[1]""")
    chain.pop()
    chain.push(scenario5.steps(2))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]//Joining ding and dong should yield dingdong -- Example 3.1[1]/I join the two strings[1]""")
    chain.pop()
    chain.push(scenario5.steps(3))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]//Joining ding and dong should yield dingdong -- Example 3.1[1]/the result should be "dingdong"[1]""")
    chain.pop()
    chain.pop()

    chain.pop()
    chain.pop()
    chain.nodePath should be ("/target/NodeChainTest/outline.feature/Outline[1]")
    chain.pop()
    chain.nodePath should be ("/target/NodeChainTest/outline.feature")
    chain.pop()
    chain.nodePath should be ("/")
    chain.pop()
    chain.nodePath should be ("")

  }

}
