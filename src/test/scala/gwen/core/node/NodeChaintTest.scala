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
import gwen.core.eval.EvalEngine
import gwen.core.node.gherkin.GherkinParser
import gwen.core.node.gherkin.SpecNormaliser
import gwen.core.node.gherkin.TagFilter
import gwen.core.state.DataRecord
import gwen.core.state.EnvState

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
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Compound words[1]/Joining basket and ball should yield basketball -- Compound words[1]""")
    chain.push(scenario1.background.get)
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Compound words[1]/Joining basket and ball should yield basketball -- Compound words[1]/background[1]""")
    chain.push(scenario1.background.get.steps(0))
    chain.nodePath should be("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Compound words[1]/Joining basket and ball should yield basketball -- Compound words[1]/background[1]/background step 1[1]""")
    chain.pop()
    chain.nodePath should be("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Compound words[1]/Joining basket and ball should yield basketball -- Compound words[1]/background[1]""")
    chain.pop()
    chain.nodePath should be("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Compound words[1]/Joining basket and ball should yield basketball -- Compound words[1]""")
    chain.push(scenario1.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Compound words[1]/Joining basket and ball should yield basketball -- Compound words[1]/string 1 is "basket"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Compound words[1]/Joining basket and ball should yield basketball -- Compound words[1]""")
    chain.push(scenario1.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Compound words[1]/Joining basket and ball should yield basketball -- Compound words[1]/string 2 is "ball"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Compound words[1]/Joining basket and ball should yield basketball -- Compound words[1]""")
    chain.push(scenario1.steps(2))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Compound words[1]/Joining basket and ball should yield basketball -- Compound words[1]/I join the two strings[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Compound words[1]/Joining basket and ball should yield basketball -- Compound words[1]""")
    chain.push(scenario1.steps(3))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Compound words[1]/Joining basket and ball should yield basketball -- Compound words[1]/the result should be "basketball"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Compound words[1]/Joining basket and ball should yield basketball -- Compound words[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Compound words[1]""")

    val scenario2 = example1.scenarios(1)
    chain.push(scenario2)
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Compound words[1]/Joining any and thing should yield anything -- Compound words[1]""")
    chain.push(scenario2.background.get)
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Compound words[1]/Joining any and thing should yield anything -- Compound words[1]/background[1]""")
    chain.push(scenario2.background.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Compound words[1]/Joining any and thing should yield anything -- Compound words[1]/background[1]/background step 1[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Compound words[1]/Joining any and thing should yield anything -- Compound words[1]/background[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Compound words[1]/Joining any and thing should yield anything -- Compound words[1]""")
    chain.push(scenario2.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Compound words[1]/Joining any and thing should yield anything -- Compound words[1]/string 1 is "any"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Compound words[1]/Joining any and thing should yield anything -- Compound words[1]""")
    chain.push(scenario2.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Compound words[1]/Joining any and thing should yield anything -- Compound words[1]/string 2 is "thing"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Compound words[1]/Joining any and thing should yield anything -- Compound words[1]""")
    chain.push(scenario2.steps(2))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Compound words[1]/Joining any and thing should yield anything -- Compound words[1]/I join the two strings[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Compound words[1]/Joining any and thing should yield anything -- Compound words[1]""")
    chain.push(scenario2.steps(3))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Compound words[1]/Joining any and thing should yield anything -- Compound words[1]/the result should be "anything"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Compound words[1]/Joining any and thing should yield anything -- Compound words[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Compound words[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]""")

    val example2 = examples(1)
    chain.push(example2)
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Nonsensical compound words[1]""")
    
    val scenario3 = example2.scenarios(0)
    chain.push(scenario3)
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Nonsensical compound words[1]/Joining howdy and doo should yield howdydoo -- Nonsensical compound words[1]""")
    chain.push(scenario3.background.get)
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Nonsensical compound words[1]/Joining howdy and doo should yield howdydoo -- Nonsensical compound words[1]/background[1]""")
    chain.push(scenario3.background.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Nonsensical compound words[1]/Joining howdy and doo should yield howdydoo -- Nonsensical compound words[1]/background[1]/background step 1[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Nonsensical compound words[1]/Joining howdy and doo should yield howdydoo -- Nonsensical compound words[1]/background[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Nonsensical compound words[1]/Joining howdy and doo should yield howdydoo -- Nonsensical compound words[1]""")
    chain.push(scenario3.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Nonsensical compound words[1]/Joining howdy and doo should yield howdydoo -- Nonsensical compound words[1]/string 1 is "howdy"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Nonsensical compound words[1]/Joining howdy and doo should yield howdydoo -- Nonsensical compound words[1]""")
    chain.push(scenario3.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Nonsensical compound words[1]/Joining howdy and doo should yield howdydoo -- Nonsensical compound words[1]/string 2 is "doo"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Nonsensical compound words[1]/Joining howdy and doo should yield howdydoo -- Nonsensical compound words[1]""")
    chain.push(scenario3.steps(2))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Nonsensical compound words[1]/Joining howdy and doo should yield howdydoo -- Nonsensical compound words[1]/I join the two strings[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Nonsensical compound words[1]/Joining howdy and doo should yield howdydoo -- Nonsensical compound words[1]""")
    chain.push(scenario3.steps(3))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Nonsensical compound words[1]/Joining howdy and doo should yield howdydoo -- Nonsensical compound words[1]/the result should be "howdydoo"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Nonsensical compound words[1]/Joining howdy and doo should yield howdydoo -- Nonsensical compound words[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Nonsensical compound words[1]""")

    val scenario4 = example2.scenarios(1)
    chain.push(scenario4)
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Nonsensical compound words[1]/Joining yep and ok should yield yepok -- Nonsensical compound words[1]""")
    chain.push(scenario4.background.get)
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Nonsensical compound words[1]/Joining yep and ok should yield yepok -- Nonsensical compound words[1]/background[1]""")
    chain.push(scenario4.background.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Nonsensical compound words[1]/Joining yep and ok should yield yepok -- Nonsensical compound words[1]/background[1]/background step 1[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Nonsensical compound words[1]/Joining yep and ok should yield yepok -- Nonsensical compound words[1]/background[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Nonsensical compound words[1]/Joining yep and ok should yield yepok -- Nonsensical compound words[1]""")
    chain.push(scenario4.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Nonsensical compound words[1]/Joining yep and ok should yield yepok -- Nonsensical compound words[1]/string 1 is "yep"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Nonsensical compound words[1]/Joining yep and ok should yield yepok -- Nonsensical compound words[1]""")
    chain.push(scenario4.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Nonsensical compound words[1]/Joining yep and ok should yield yepok -- Nonsensical compound words[1]/string 2 is "ok"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Nonsensical compound words[1]/Joining yep and ok should yield yepok -- Nonsensical compound words[1]""")
    chain.push(scenario4.steps(2))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Nonsensical compound words[1]/Joining yep and ok should yield yepok -- Nonsensical compound words[1]/I join the two strings[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Nonsensical compound words[1]/Joining yep and ok should yield yepok -- Nonsensical compound words[1]""")
    chain.push(scenario4.steps(3))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Nonsensical compound words[1]/Joining yep and ok should yield yepok -- Nonsensical compound words[1]/the result should be "yepok"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Nonsensical compound words[1]/Joining yep and ok should yield yepok -- Nonsensical compound words[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/Nonsensical compound words[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]""")

    val example3 = examples(2)
    chain.push(example3)
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/""")
    
    val scenario5 = example3.scenarios(0)
    chain.push(scenario5)
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]//Joining ding and dong should yield dingdong[1]""")
    chain.push(scenario5.background.get)
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]//Joining ding and dong should yield dingdong[1]/background[1]""")
    chain.push(scenario5.background.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]//Joining ding and dong should yield dingdong[1]/background[1]/background step 1[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]//Joining ding and dong should yield dingdong[1]/background[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]//Joining ding and dong should yield dingdong[1]""")
    chain.push(scenario5.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]//Joining ding and dong should yield dingdong[1]/string 1 is "ding"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]//Joining ding and dong should yield dingdong[1]""")
    chain.push(scenario5.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]//Joining ding and dong should yield dingdong[1]/string 2 is "dong"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]//Joining ding and dong should yield dingdong[1]""")
    chain.push(scenario5.steps(2))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]//Joining ding and dong should yield dingdong[1]/I join the two strings[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]//Joining ding and dong should yield dingdong[1]""")
    chain.push(scenario5.steps(3))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]//Joining ding and dong should yield dingdong[1]/the result should be "dingdong"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]//Joining ding and dong should yield dingdong[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]/""")

    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline[1]/Joining <string 1> and <string 2> should yield <result>[1]""")
    chain.pop()
    chain.nodePath should be ("/target/NodeChainTest/outline.feature/Outline[1]")
    chain.pop()
    chain.nodePath should be ("/target/NodeChainTest/outline.feature")
    chain.pop()
    chain.nodePath should be ("/")
    chain.pop()
    chain.nodePath should be ("")

  }

  "Node paths" should "be generared for all nodes in evaluated feature with for-each loop" in {

    val featureString = """
      Feature: For each delimited value with if condition

      @StepDef
      Scenario: I click option "<option>" in "<type>"
          Given the option is "$<option>"
            And the type is "$<type>"

      @StepDef
      Scenario: I click checbox in "<type>"
          Given options is "~option1,option2,option3"
            And condition is defined by javascript "true"
          When I click option "${opt}" in "$<type>" for each opt in options delimited by "," if condition

      Scenario: I process options
          Given the type is "my type"
           When I click checbox in "group"
           Then the option should be "option3"
            And the type should be "group"
    """

    val rootDir: File = new File("target" +  File.separator + this.getClass.getSimpleName)
    val file = new File(rootDir.getPath, "for-each.feature")
    file.getParentFile.mkdirs()
    file.createNewFile()
    file.writeText(featureString)

    val unit = FeatureUnit(Root, file, Nil, None, new TagFilter(Nil), None)
    val options = GwenOptions(features = List(file))
    val ctx = EvalEngine.DefaultInstance.init(options, EnvState())
    val result = EvalEngine.DefaultInstance.evaluateUnit(unit, ctx)

    val spec = result.get.spec
    val chain = new NodeChain()
    
    chain.nodePath should be ("")
    chain.push(Root)
    chain.nodePath should be ("/")
    chain.push(unit)
    chain.nodePath should be ("/target/NodeChainTest/for-each.feature")
    chain.push(spec)
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition[1]""")

    val stepDef1 = spec.scenarios(0)
    chain.push(stepDef1)
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition[1]/I click option "<option>" in "<type>"[1]""")
    chain.push(stepDef1.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition[1]/I click option "<option>" in "<type>"[1]/the option is "$<option>"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition[1]/I click option "<option>" in "<type>"[1]""")
    chain.push(stepDef1.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition[1]/I click option "<option>" in "<type>"[1]/the type is "$<type>"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition[1]/I click option "<option>" in "<type>"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition[1]""")

    val stepDef2 = spec.scenarios(1)
    chain.push(stepDef2)
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition[1]/I click checbox in "<type>"[1]""")
    chain.push(stepDef2.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition[1]/I click checbox in "<type>"[1]/options is "~option1,option2,option3"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition[1]/I click checbox in "<type>"[1]""")
    chain.push(stepDef2.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition[1]/I click checbox in "<type>"[1]/condition is defined by javascript "true"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition[1]/I click checbox in "<type>"[1]""")
    chain.push(stepDef2.steps(2))
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition[1]/I click checbox in "<type>"[1]/I click option "${opt}" in "$<type>" for each opt in options delimited by "," if condition[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition[1]/I click checbox in "<type>"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition[1]""")

    val scenario = spec.scenarios(2)
    chain.push(scenario)
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition[1]/I process options[1]""")
    chain.push(scenario.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition[1]/I process options[1]/the type is "my type"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition[1]/I process options[1]""")
    chain.push(scenario.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition[1]/I process options[1]/I click checbox in "group"[1]""")
    chain.push(scenario.steps(1).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition[1]/I process options[1]/I click checbox in "group"[1]/I click checbox in "<type>"[1]""")
    chain.push(scenario.steps(1).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition[1]/I process options[1]/I click checbox in "group"[1]/I click checbox in "<type>"[1]/options is "~option1,option2,option3"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition[1]/I process options[1]/I click checbox in "group"[1]/I click checbox in "<type>"[1]""")
    chain.push(scenario.steps(1).stepDef.get.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition[1]/I process options[1]/I click checbox in "group"[1]/I click checbox in "<type>"[1]/condition is defined by javascript "true"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition[1]/I process options[1]/I click checbox in "group"[1]/I click checbox in "<type>"[1]""")
    chain.push(scenario.steps(1).stepDef.get.steps(2))
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition[1]/I process options[1]/I click checbox in "group"[1]/I click checbox in "<type>"[1]/I click option "${opt}" in "group" for each opt in options delimited by "," if condition[1]""")
    chain.push(scenario.steps(1).stepDef.get.steps(2).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition[1]/I process options[1]/I click checbox in "group"[1]/I click checbox in "<type>"[1]/I click option "${opt}" in "group" for each opt in options delimited by "," if condition[1]/condition[1]""")
    chain.push(scenario.steps(1).stepDef.get.steps(2).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition[1]/I process options[1]/I click checbox in "group"[1]/I click checbox in "<type>"[1]/I click option "${opt}" in "group" for each opt in options delimited by "," if condition[1]/condition[1]/I click option "${opt}" in "group" for each opt in options delimited by ","[1]""")
    chain.push(scenario.steps(1).stepDef.get.steps(2).stepDef.get.steps(0).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition[1]/I process options[1]/I click checbox in "group"[1]/I click checbox in "<type>"[1]/I click option "${opt}" in "group" for each opt in options delimited by "," if condition[1]/condition[1]/I click option "${opt}" in "group" for each opt in options delimited by ","[1]/opt[1]""")
    chain.push(scenario.steps(1).stepDef.get.steps(2).stepDef.get.steps(0).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition[1]/I process options[1]/I click checbox in "group"[1]/I click checbox in "<type>"[1]/I click option "${opt}" in "group" for each opt in options delimited by "," if condition[1]/condition[1]/I click option "${opt}" in "group" for each opt in options delimited by ","[1]/opt[1]/I click option "~option1" in "group"[1]""")
    chain.push(scenario.steps(1).stepDef.get.steps(2).stepDef.get.steps(0).stepDef.get.steps(0).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition[1]/I process options[1]/I click checbox in "group"[1]/I click checbox in "<type>"[1]/I click option "${opt}" in "group" for each opt in options delimited by "," if condition[1]/condition[1]/I click option "${opt}" in "group" for each opt in options delimited by ","[1]/opt[1]/I click option "~option1" in "group"[1]/I click option "<option>" in "<type>"[1]""")
    chain.push(scenario.steps(1).stepDef.get.steps(2).stepDef.get.steps(0).stepDef.get.steps(0).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition[1]/I process options[1]/I click checbox in "group"[1]/I click checbox in "<type>"[1]/I click option "${opt}" in "group" for each opt in options delimited by "," if condition[1]/condition[1]/I click option "${opt}" in "group" for each opt in options delimited by ","[1]/opt[1]/I click option "~option1" in "group"[1]/I click option "<option>" in "<type>"[1]/the option is "~option1"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition[1]/I process options[1]/I click checbox in "group"[1]/I click checbox in "<type>"[1]/I click option "${opt}" in "group" for each opt in options delimited by "," if condition[1]/condition[1]/I click option "${opt}" in "group" for each opt in options delimited by ","[1]/opt[1]/I click option "~option1" in "group"[1]/I click option "<option>" in "<type>"[1]""")
    chain.push(scenario.steps(1).stepDef.get.steps(2).stepDef.get.steps(0).stepDef.get.steps(0).stepDef.get.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition[1]/I process options[1]/I click checbox in "group"[1]/I click checbox in "<type>"[1]/I click option "${opt}" in "group" for each opt in options delimited by "," if condition[1]/condition[1]/I click option "${opt}" in "group" for each opt in options delimited by ","[1]/opt[1]/I click option "~option1" in "group"[1]/I click option "<option>" in "<type>"[1]/the type is "group"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition[1]/I process options[1]/I click checbox in "group"[1]/I click checbox in "<type>"[1]/I click option "${opt}" in "group" for each opt in options delimited by "," if condition[1]/condition[1]/I click option "${opt}" in "group" for each opt in options delimited by ","[1]/opt[1]/I click option "~option1" in "group"[1]/I click option "<option>" in "<type>"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition[1]/I process options[1]/I click checbox in "group"[1]/I click checbox in "<type>"[1]/I click option "${opt}" in "group" for each opt in options delimited by "," if condition[1]/condition[1]/I click option "${opt}" in "group" for each opt in options delimited by ","[1]/opt[1]/I click option "~option1" in "group"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition[1]/I process options[1]/I click checbox in "group"[1]/I click checbox in "<type>"[1]/I click option "${opt}" in "group" for each opt in options delimited by "," if condition[1]/condition[1]/I click option "${opt}" in "group" for each opt in options delimited by ","[1]/opt[1]""")
    chain.push(scenario.steps(1).stepDef.get.steps(2).stepDef.get.steps(0).stepDef.get.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition[1]/I process options[1]/I click checbox in "group"[1]/I click checbox in "<type>"[1]/I click option "${opt}" in "group" for each opt in options delimited by "," if condition[1]/condition[1]/I click option "${opt}" in "group" for each opt in options delimited by ","[1]/opt[1]/I click option "option2" in "group"[1]""")
    chain.push(scenario.steps(1).stepDef.get.steps(2).stepDef.get.steps(0).stepDef.get.steps(1).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition[1]/I process options[1]/I click checbox in "group"[1]/I click checbox in "<type>"[1]/I click option "${opt}" in "group" for each opt in options delimited by "," if condition[1]/condition[1]/I click option "${opt}" in "group" for each opt in options delimited by ","[1]/opt[1]/I click option "option2" in "group"[1]/I click option "<option>" in "<type>"[1]""")
    chain.push(scenario.steps(1).stepDef.get.steps(2).stepDef.get.steps(0).stepDef.get.steps(1).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition[1]/I process options[1]/I click checbox in "group"[1]/I click checbox in "<type>"[1]/I click option "${opt}" in "group" for each opt in options delimited by "," if condition[1]/condition[1]/I click option "${opt}" in "group" for each opt in options delimited by ","[1]/opt[1]/I click option "option2" in "group"[1]/I click option "<option>" in "<type>"[1]/the option is "option2"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition[1]/I process options[1]/I click checbox in "group"[1]/I click checbox in "<type>"[1]/I click option "${opt}" in "group" for each opt in options delimited by "," if condition[1]/condition[1]/I click option "${opt}" in "group" for each opt in options delimited by ","[1]/opt[1]/I click option "option2" in "group"[1]/I click option "<option>" in "<type>"[1]""")
    chain.push(scenario.steps(1).stepDef.get.steps(2).stepDef.get.steps(0).stepDef.get.steps(1).stepDef.get.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition[1]/I process options[1]/I click checbox in "group"[1]/I click checbox in "<type>"[1]/I click option "${opt}" in "group" for each opt in options delimited by "," if condition[1]/condition[1]/I click option "${opt}" in "group" for each opt in options delimited by ","[1]/opt[1]/I click option "option2" in "group"[1]/I click option "<option>" in "<type>"[1]/the type is "group"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition[1]/I process options[1]/I click checbox in "group"[1]/I click checbox in "<type>"[1]/I click option "${opt}" in "group" for each opt in options delimited by "," if condition[1]/condition[1]/I click option "${opt}" in "group" for each opt in options delimited by ","[1]/opt[1]/I click option "option2" in "group"[1]/I click option "<option>" in "<type>"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition[1]/I process options[1]/I click checbox in "group"[1]/I click checbox in "<type>"[1]/I click option "${opt}" in "group" for each opt in options delimited by "," if condition[1]/condition[1]/I click option "${opt}" in "group" for each opt in options delimited by ","[1]/opt[1]/I click option "option2" in "group"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition[1]/I process options[1]/I click checbox in "group"[1]/I click checbox in "<type>"[1]/I click option "${opt}" in "group" for each opt in options delimited by "," if condition[1]/condition[1]/I click option "${opt}" in "group" for each opt in options delimited by ","[1]/opt[1]""")
    chain.push(scenario.steps(1).stepDef.get.steps(2).stepDef.get.steps(0).stepDef.get.steps(2))
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition[1]/I process options[1]/I click checbox in "group"[1]/I click checbox in "<type>"[1]/I click option "${opt}" in "group" for each opt in options delimited by "," if condition[1]/condition[1]/I click option "${opt}" in "group" for each opt in options delimited by ","[1]/opt[1]/I click option "option3" in "group"[1]""")
    chain.push(scenario.steps(1).stepDef.get.steps(2).stepDef.get.steps(0).stepDef.get.steps(2).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition[1]/I process options[1]/I click checbox in "group"[1]/I click checbox in "<type>"[1]/I click option "${opt}" in "group" for each opt in options delimited by "," if condition[1]/condition[1]/I click option "${opt}" in "group" for each opt in options delimited by ","[1]/opt[1]/I click option "option3" in "group"[1]/I click option "<option>" in "<type>"[1]""")
    chain.push(scenario.steps(1).stepDef.get.steps(2).stepDef.get.steps(0).stepDef.get.steps(2).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition[1]/I process options[1]/I click checbox in "group"[1]/I click checbox in "<type>"[1]/I click option "${opt}" in "group" for each opt in options delimited by "," if condition[1]/condition[1]/I click option "${opt}" in "group" for each opt in options delimited by ","[1]/opt[1]/I click option "option3" in "group"[1]/I click option "<option>" in "<type>"[1]/the option is "option3"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition[1]/I process options[1]/I click checbox in "group"[1]/I click checbox in "<type>"[1]/I click option "${opt}" in "group" for each opt in options delimited by "," if condition[1]/condition[1]/I click option "${opt}" in "group" for each opt in options delimited by ","[1]/opt[1]/I click option "option3" in "group"[1]/I click option "<option>" in "<type>"[1]""")
    chain.push(scenario.steps(1).stepDef.get.steps(2).stepDef.get.steps(0).stepDef.get.steps(2).stepDef.get.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition[1]/I process options[1]/I click checbox in "group"[1]/I click checbox in "<type>"[1]/I click option "${opt}" in "group" for each opt in options delimited by "," if condition[1]/condition[1]/I click option "${opt}" in "group" for each opt in options delimited by ","[1]/opt[1]/I click option "option3" in "group"[1]/I click option "<option>" in "<type>"[1]/the type is "group"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition[1]/I process options[1]/I click checbox in "group"[1]/I click checbox in "<type>"[1]/I click option "${opt}" in "group" for each opt in options delimited by "," if condition[1]/condition[1]/I click option "${opt}" in "group" for each opt in options delimited by ","[1]/opt[1]/I click option "option3" in "group"[1]/I click option "<option>" in "<type>"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition[1]/I process options[1]/I click checbox in "group"[1]/I click checbox in "<type>"[1]/I click option "${opt}" in "group" for each opt in options delimited by "," if condition[1]/condition[1]/I click option "${opt}" in "group" for each opt in options delimited by ","[1]/opt[1]/I click option "option3" in "group"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition[1]/I process options[1]/I click checbox in "group"[1]/I click checbox in "<type>"[1]/I click option "${opt}" in "group" for each opt in options delimited by "," if condition[1]/condition[1]/I click option "${opt}" in "group" for each opt in options delimited by ","[1]/opt[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition[1]/I process options[1]/I click checbox in "group"[1]/I click checbox in "<type>"[1]/I click option "${opt}" in "group" for each opt in options delimited by "," if condition[1]/condition[1]/I click option "${opt}" in "group" for each opt in options delimited by ","[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition[1]/I process options[1]/I click checbox in "group"[1]/I click checbox in "<type>"[1]/I click option "${opt}" in "group" for each opt in options delimited by "," if condition[1]/condition[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition[1]/I process options[1]/I click checbox in "group"[1]/I click checbox in "<type>"[1]/I click option "${opt}" in "group" for each opt in options delimited by "," if condition[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition[1]/I process options[1]/I click checbox in "group"[1]/I click checbox in "<type>"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition[1]/I process options[1]/I click checbox in "group"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition[1]/I process options[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition[1]""")
    chain.pop()
    chain.nodePath should be ("/target/NodeChainTest/for-each.feature")
    chain.pop()
    chain.nodePath should be ("/")
    chain.pop()
    chain.nodePath should be ("")
  }

  "Node paths" should "be generared for all nodes in evaluated feature with until loop" in {

    val featureString = """
      Feature: Repeat until example

      @StepDef
      Scenario: I increment counter
          Given counter is defined by javascript "${counter} + 1"
            And counter > 3 is defined by javascript "${counter} > 3"

      Scenario: Increment counter
          Given counter is "0"
          When I increment counter until counter > 3 using no delay
          Then counter should be "4"

      Scenario: Increment counter with if condition
          Given counter is "-1"
            And condition is defined by javascript "true"
          When I increment counter
            And I increment counter until counter > 3 using no delay if condition
          Then counter should be "4"
      """

    val rootDir: File = new File("target" +  File.separator + this.getClass.getSimpleName)
    val file = new File(rootDir.getPath, "until.feature")
    file.getParentFile.mkdirs()
    file.createNewFile()
    file.writeText(featureString)

    val unit = FeatureUnit(Root, file, Nil, None, new TagFilter(Nil), None)
    val options = GwenOptions(features = List(file))
    val ctx = EvalEngine.DefaultInstance.init(options, EnvState())
    val result = EvalEngine.DefaultInstance.evaluateUnit(unit, ctx)

    val spec = result.get.spec
    val chain = new NodeChain()
    
    chain.nodePath should be ("")
    chain.push(Root)
    chain.nodePath should be ("/")
    chain.push(unit)
    chain.nodePath should be ("/target/NodeChainTest/until.feature")
    chain.push(spec)
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]""")
    
    val stepDef1 = spec.scenarios(0)
    chain.push(stepDef1)
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/I increment counter[1]""")
    chain.push(stepDef1.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/I increment counter[1]/counter is defined by javascript "${counter} + 1"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/I increment counter[1]""")
    chain.push(stepDef1.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/I increment counter[1]/counter > 3 is defined by javascript "${counter} > 3"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/I increment counter[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]""")

    val scenario1 = spec.scenarios(1)
    chain.push(scenario1)
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter[1]""")
    chain.push(scenario1.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter[1]/counter is "0"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter[1]""")
    chain.push(scenario1.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter[1]/I increment counter until counter > 3 using no delay[1]""")
    chain.push(scenario1.steps(1).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]""")
    chain.push(scenario1.steps(1).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]/I increment counter[1]""")
    chain.push(scenario1.steps(1).stepDef.get.steps(0).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]/I increment counter[1]/I increment counter[1]""")
    chain.push(scenario1.steps(1).stepDef.get.steps(0).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]/I increment counter[1]/I increment counter[1]/counter is defined by javascript "0 + 1"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]/I increment counter[1]/I increment counter[1]""")
    chain.push(scenario1.steps(1).stepDef.get.steps(0).stepDef.get.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]/I increment counter[1]/I increment counter[1]/counter > 3 is defined by javascript "1 > 3"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]/I increment counter[1]/I increment counter[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]/I increment counter[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]""")
    chain.push(scenario1.steps(1).stepDef.get.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]/I increment counter[2]""")
    chain.push(scenario1.steps(1).stepDef.get.steps(1).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]/I increment counter[2]/I increment counter[1]""")
    chain.push(scenario1.steps(1).stepDef.get.steps(1).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]/I increment counter[2]/I increment counter[1]/counter is defined by javascript "1 + 1"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]/I increment counter[2]/I increment counter[1]""")
    chain.push(scenario1.steps(1).stepDef.get.steps(1).stepDef.get.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]/I increment counter[2]/I increment counter[1]/counter > 3 is defined by javascript "2 > 3"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]/I increment counter[2]/I increment counter[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]/I increment counter[2]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]""")
    chain.push(scenario1.steps(1).stepDef.get.steps(2))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]/I increment counter[3]""")
    chain.push(scenario1.steps(1).stepDef.get.steps(2).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]/I increment counter[3]/I increment counter[1]""")
    chain.push(scenario1.steps(1).stepDef.get.steps(2).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]/I increment counter[3]/I increment counter[1]/counter is defined by javascript "2 + 1"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]/I increment counter[3]/I increment counter[1]""")
    chain.push(scenario1.steps(1).stepDef.get.steps(2).stepDef.get.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]/I increment counter[3]/I increment counter[1]/counter > 3 is defined by javascript "3 > 3"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]/I increment counter[3]/I increment counter[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]/I increment counter[3]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]""")
    chain.push(scenario1.steps(1).stepDef.get.steps(3))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]/I increment counter[4]""")
    chain.push(scenario1.steps(1).stepDef.get.steps(3).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]/I increment counter[4]/I increment counter[1]""")
    chain.push(scenario1.steps(1).stepDef.get.steps(3).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]/I increment counter[4]/I increment counter[1]/counter is defined by javascript "3 + 1"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]/I increment counter[4]/I increment counter[1]""")
    chain.push(scenario1.steps(1).stepDef.get.steps(3).stepDef.get.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]/I increment counter[4]/I increment counter[1]/counter > 3 is defined by javascript "4 > 3"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]/I increment counter[4]/I increment counter[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]/I increment counter[4]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter[1]/I increment counter until counter > 3 using no delay[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]""")

    val scenario2 = spec.scenarios(2)
    chain.push(scenario2)
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter with if condition[1]""")
    chain.push(scenario2.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter with if condition[1]/counter is "-1"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter with if condition[1]""")
    chain.push(scenario2.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter with if condition[1]/condition is defined by javascript "true"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter with if condition[1]""")
    chain.push(scenario2.steps(2))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter with if condition[1]/I increment counter[1]""")
    chain.push(scenario2.steps(2).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter with if condition[1]/I increment counter[1]/I increment counter[1]""")
    chain.push(scenario2.steps(2).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter with if condition[1]/I increment counter[1]/I increment counter[1]/counter is defined by javascript "-1 + 1"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter with if condition[1]/I increment counter[1]/I increment counter[1]""")
    chain.push(scenario2.steps(2).stepDef.get.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter with if condition[1]/I increment counter[1]/I increment counter[1]/counter > 3 is defined by javascript "0 > 3"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter with if condition[1]/I increment counter[1]/I increment counter[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter with if condition[1]/I increment counter[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter with if condition[1]""")
    chain.push(scenario2.steps(3))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter with if condition[1]/I increment counter until counter > 3 using no delay if condition[1]""")
    chain.push(scenario2.steps(3).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter with if condition[1]/I increment counter until counter > 3 using no delay if condition[1]/condition[1]""")
    chain.push(scenario2.steps(3).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter with if condition[1]/I increment counter until counter > 3 using no delay if condition[1]/condition[1]/I increment counter until counter > 3 using no delay[1]""")
    chain.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter with if condition[1]/I increment counter until counter > 3 using no delay if condition[1]/condition[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]""")
    chain.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter with if condition[1]/I increment counter until counter > 3 using no delay if condition[1]/condition[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]/I increment counter[1]""")
    chain.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(0).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter with if condition[1]/I increment counter until counter > 3 using no delay if condition[1]/condition[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]/I increment counter[1]/I increment counter[1]""")
    chain.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(0).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter with if condition[1]/I increment counter until counter > 3 using no delay if condition[1]/condition[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]/I increment counter[1]/I increment counter[1]/counter is defined by javascript "0 + 1"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter with if condition[1]/I increment counter until counter > 3 using no delay if condition[1]/condition[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]/I increment counter[1]/I increment counter[1]""")
    chain.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(0).stepDef.get.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter with if condition[1]/I increment counter until counter > 3 using no delay if condition[1]/condition[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]/I increment counter[1]/I increment counter[1]/counter > 3 is defined by javascript "1 > 3"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter with if condition[1]/I increment counter until counter > 3 using no delay if condition[1]/condition[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]/I increment counter[1]/I increment counter[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter with if condition[1]/I increment counter until counter > 3 using no delay if condition[1]/condition[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]/I increment counter[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter with if condition[1]/I increment counter until counter > 3 using no delay if condition[1]/condition[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]""")
    chain.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter with if condition[1]/I increment counter until counter > 3 using no delay if condition[1]/condition[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]/I increment counter[2]""")
    chain.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(1).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter with if condition[1]/I increment counter until counter > 3 using no delay if condition[1]/condition[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]/I increment counter[2]/I increment counter[1]""")
    chain.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(1).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter with if condition[1]/I increment counter until counter > 3 using no delay if condition[1]/condition[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]/I increment counter[2]/I increment counter[1]/counter is defined by javascript "1 + 1"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter with if condition[1]/I increment counter until counter > 3 using no delay if condition[1]/condition[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]/I increment counter[2]/I increment counter[1]""")
    chain.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(1).stepDef.get.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter with if condition[1]/I increment counter until counter > 3 using no delay if condition[1]/condition[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]/I increment counter[2]/I increment counter[1]/counter > 3 is defined by javascript "2 > 3"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter with if condition[1]/I increment counter until counter > 3 using no delay if condition[1]/condition[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]/I increment counter[2]/I increment counter[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter with if condition[1]/I increment counter until counter > 3 using no delay if condition[1]/condition[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]/I increment counter[2]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter with if condition[1]/I increment counter until counter > 3 using no delay if condition[1]/condition[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]""")
    chain.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(2))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter with if condition[1]/I increment counter until counter > 3 using no delay if condition[1]/condition[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]/I increment counter[3]""")
    chain.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(2).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter with if condition[1]/I increment counter until counter > 3 using no delay if condition[1]/condition[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]/I increment counter[3]/I increment counter[1]""")
    chain.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(2).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter with if condition[1]/I increment counter until counter > 3 using no delay if condition[1]/condition[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]/I increment counter[3]/I increment counter[1]/counter is defined by javascript "2 + 1"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter with if condition[1]/I increment counter until counter > 3 using no delay if condition[1]/condition[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]/I increment counter[3]/I increment counter[1]""")
    chain.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(2).stepDef.get.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter with if condition[1]/I increment counter until counter > 3 using no delay if condition[1]/condition[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]/I increment counter[3]/I increment counter[1]/counter > 3 is defined by javascript "3 > 3"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter with if condition[1]/I increment counter until counter > 3 using no delay if condition[1]/condition[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]/I increment counter[3]/I increment counter[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter with if condition[1]/I increment counter until counter > 3 using no delay if condition[1]/condition[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]/I increment counter[3]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter with if condition[1]/I increment counter until counter > 3 using no delay if condition[1]/condition[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]""")
    chain.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(3))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter with if condition[1]/I increment counter until counter > 3 using no delay if condition[1]/condition[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]/I increment counter[4]""")
    chain.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(3).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter with if condition[1]/I increment counter until counter > 3 using no delay if condition[1]/condition[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]/I increment counter[4]/I increment counter[1]""")
    chain.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(3).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter with if condition[1]/I increment counter until counter > 3 using no delay if condition[1]/condition[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]/I increment counter[4]/I increment counter[1]/counter is defined by javascript "3 + 1"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter with if condition[1]/I increment counter until counter > 3 using no delay if condition[1]/condition[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]/I increment counter[4]/I increment counter[1]""")
    chain.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(3).stepDef.get.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter with if condition[1]/I increment counter until counter > 3 using no delay if condition[1]/condition[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]/I increment counter[4]/I increment counter[1]/counter > 3 is defined by javascript "4 > 3"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter with if condition[1]/I increment counter until counter > 3 using no delay if condition[1]/condition[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]/I increment counter[4]/I increment counter[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter with if condition[1]/I increment counter until counter > 3 using no delay if condition[1]/condition[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]/I increment counter[4]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter with if condition[1]/I increment counter until counter > 3 using no delay if condition[1]/condition[1]/I increment counter until counter > 3 using no delay[1]/counter > 3[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter with if condition[1]/I increment counter until counter > 3 using no delay if condition[1]/condition[1]/I increment counter until counter > 3 using no delay[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter with if condition[1]/I increment counter until counter > 3 using no delay if condition[1]/condition[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter with if condition[1]/I increment counter until counter > 3 using no delay if condition[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter with if condition[1]""")
    chain.push(scenario2.steps(4))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter with if condition[1]/counter should be "4"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]/Increment counter with if condition[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example[1]""")
    chain.pop()
    chain.nodePath should be ("/target/NodeChainTest/until.feature")
    chain.pop()
    chain.nodePath should be ("/")
    chain.pop()
    chain.nodePath should be ("")  
  }

  "Node paths" should "be generared for all nodes in evaluated feature with while loop" in {

    val featureString = """
      Feature: Repeat while example

      @StepDef
      Scenario: I increment counter1
          Given counter1 is defined by javascript "${counter1} + 1"
            And counter1 < 4 is defined by javascript "${counter1} < 4"

      @StepDef
      Scenario: I increment counter2
          Given counter2 is defined by javascript "${counter2} + 1"
            And counter2 < 4 is defined by javascript "${counter2} < 4"

      Scenario: Increment counter1
          Given counter1 is "-1"
          When I increment counter1
            And I increment counter1 while counter1 < 4 using no delay
          Then counter1 should be "4"

      Scenario: Increment counter2 with if condition
          Given counter2 is "-1"
            And condition is defined by javascript "true"
          When I increment counter2
            And I increment counter2 while counter2 < 4 using no delay if condition
          Then counter2 should be "4"
       """

    val rootDir: File = new File("target" +  File.separator + this.getClass.getSimpleName)
    val file = new File(rootDir.getPath, "while.feature")
    file.getParentFile.mkdirs()
    file.createNewFile()
    file.writeText(featureString)

    val unit = FeatureUnit(Root, file, Nil, None, new TagFilter(Nil), None)
    val options = GwenOptions(features = List(file))
    val ctx = EvalEngine.DefaultInstance.init(options, EnvState())
    val result = EvalEngine.DefaultInstance.evaluateUnit(unit, ctx)

    val spec = result.get.spec
    val chain = new NodeChain()
    
    chain.nodePath should be ("")
    chain.push(Root)
    chain.nodePath should be ("/")
    chain.push(unit)
    chain.nodePath should be ("/target/NodeChainTest/while.feature")
    chain.push(spec)
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]""")
    
    val stepDef1 = spec.scenarios(0)
    chain.push(stepDef1)
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/I increment counter1[1]""")
    chain.push(stepDef1.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/I increment counter1[1]/counter1 is defined by javascript "${counter1} + 1"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/I increment counter1[1]""")
    chain.push(stepDef1.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/I increment counter1[1]/counter1 < 4 is defined by javascript "${counter1} < 4"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/I increment counter1[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]""")

    val stepDef2 = spec.scenarios(1)
    chain.push(stepDef2)
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/I increment counter2[1]""")
    chain.push(stepDef2.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/I increment counter2[1]/counter2 is defined by javascript "${counter2} + 1"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/I increment counter2[1]""")
    chain.push(stepDef2.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/I increment counter2[1]/counter2 < 4 is defined by javascript "${counter2} < 4"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/I increment counter2[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]""")

    val scenario1 = spec.scenarios(2)
    chain.push(scenario1)
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter1[1]""")
    chain.push(scenario1.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter1[1]/counter1 is "-1"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter1[1]""")
    chain.push(scenario1.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter1[1]/I increment counter1[1]""")
    chain.push(scenario1.steps(1).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter1[1]/I increment counter1[1]/I increment counter1[1]""")
    chain.push(scenario1.steps(1).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter1[1]/I increment counter1[1]/I increment counter1[1]/counter1 is defined by javascript "-1 + 1"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter1[1]/I increment counter1[1]/I increment counter1[1]""")
    chain.push(scenario1.steps(1).stepDef.get.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter1[1]/I increment counter1[1]/I increment counter1[1]/counter1 < 4 is defined by javascript "0 < 4"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter1[1]/I increment counter1[1]/I increment counter1[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter1[1]/I increment counter1[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter1[1]""")
    chain.push(scenario1.steps(2))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter1[1]/I increment counter1 while counter1 < 4 using no delay[1]""")
    chain.push(scenario1.steps(2).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter1[1]/I increment counter1 while counter1 < 4 using no delay[1]/counter1 < 4[1]""")
    chain.push(scenario1.steps(2).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter1[1]/I increment counter1 while counter1 < 4 using no delay[1]/counter1 < 4[1]/I increment counter1[1]""")
    chain.push(scenario1.steps(2).stepDef.get.steps(0).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter1[1]/I increment counter1 while counter1 < 4 using no delay[1]/counter1 < 4[1]/I increment counter1[1]/I increment counter1[1]""")
    chain.push(scenario1.steps(2).stepDef.get.steps(0).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter1[1]/I increment counter1 while counter1 < 4 using no delay[1]/counter1 < 4[1]/I increment counter1[1]/I increment counter1[1]/counter1 is defined by javascript "0 + 1"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter1[1]/I increment counter1 while counter1 < 4 using no delay[1]/counter1 < 4[1]/I increment counter1[1]/I increment counter1[1]""")
    chain.push(scenario1.steps(2).stepDef.get.steps(0).stepDef.get.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter1[1]/I increment counter1 while counter1 < 4 using no delay[1]/counter1 < 4[1]/I increment counter1[1]/I increment counter1[1]/counter1 < 4 is defined by javascript "1 < 4"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter1[1]/I increment counter1 while counter1 < 4 using no delay[1]/counter1 < 4[1]/I increment counter1[1]/I increment counter1[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter1[1]/I increment counter1 while counter1 < 4 using no delay[1]/counter1 < 4[1]/I increment counter1[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter1[1]/I increment counter1 while counter1 < 4 using no delay[1]/counter1 < 4[1]""")
    chain.push(scenario1.steps(2).stepDef.get.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter1[1]/I increment counter1 while counter1 < 4 using no delay[1]/counter1 < 4[1]/I increment counter1[2]""")
    chain.push(scenario1.steps(2).stepDef.get.steps(1).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter1[1]/I increment counter1 while counter1 < 4 using no delay[1]/counter1 < 4[1]/I increment counter1[2]/I increment counter1[1]""")
    chain.push(scenario1.steps(2).stepDef.get.steps(1).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter1[1]/I increment counter1 while counter1 < 4 using no delay[1]/counter1 < 4[1]/I increment counter1[2]/I increment counter1[1]/counter1 is defined by javascript "1 + 1"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter1[1]/I increment counter1 while counter1 < 4 using no delay[1]/counter1 < 4[1]/I increment counter1[2]/I increment counter1[1]""")
    chain.push(scenario1.steps(2).stepDef.get.steps(1).stepDef.get.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter1[1]/I increment counter1 while counter1 < 4 using no delay[1]/counter1 < 4[1]/I increment counter1[2]/I increment counter1[1]/counter1 < 4 is defined by javascript "2 < 4"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter1[1]/I increment counter1 while counter1 < 4 using no delay[1]/counter1 < 4[1]/I increment counter1[2]/I increment counter1[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter1[1]/I increment counter1 while counter1 < 4 using no delay[1]/counter1 < 4[1]/I increment counter1[2]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter1[1]/I increment counter1 while counter1 < 4 using no delay[1]/counter1 < 4[1]""")
    chain.push(scenario1.steps(2).stepDef.get.steps(2))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter1[1]/I increment counter1 while counter1 < 4 using no delay[1]/counter1 < 4[1]/I increment counter1[3]""")
    chain.push(scenario1.steps(2).stepDef.get.steps(2).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter1[1]/I increment counter1 while counter1 < 4 using no delay[1]/counter1 < 4[1]/I increment counter1[3]/I increment counter1[1]""")
    chain.push(scenario1.steps(2).stepDef.get.steps(2).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter1[1]/I increment counter1 while counter1 < 4 using no delay[1]/counter1 < 4[1]/I increment counter1[3]/I increment counter1[1]/counter1 is defined by javascript "2 + 1"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter1[1]/I increment counter1 while counter1 < 4 using no delay[1]/counter1 < 4[1]/I increment counter1[3]/I increment counter1[1]""")
    chain.push(scenario1.steps(2).stepDef.get.steps(2).stepDef.get.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter1[1]/I increment counter1 while counter1 < 4 using no delay[1]/counter1 < 4[1]/I increment counter1[3]/I increment counter1[1]/counter1 < 4 is defined by javascript "3 < 4"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter1[1]/I increment counter1 while counter1 < 4 using no delay[1]/counter1 < 4[1]/I increment counter1[3]/I increment counter1[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter1[1]/I increment counter1 while counter1 < 4 using no delay[1]/counter1 < 4[1]/I increment counter1[3]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter1[1]/I increment counter1 while counter1 < 4 using no delay[1]/counter1 < 4[1]""")
    chain.push(scenario1.steps(2).stepDef.get.steps(3))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter1[1]/I increment counter1 while counter1 < 4 using no delay[1]/counter1 < 4[1]/I increment counter1[4]""")
    chain.push(scenario1.steps(2).stepDef.get.steps(3).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter1[1]/I increment counter1 while counter1 < 4 using no delay[1]/counter1 < 4[1]/I increment counter1[4]/I increment counter1[1]""")
    chain.push(scenario1.steps(2).stepDef.get.steps(3).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter1[1]/I increment counter1 while counter1 < 4 using no delay[1]/counter1 < 4[1]/I increment counter1[4]/I increment counter1[1]/counter1 is defined by javascript "3 + 1"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter1[1]/I increment counter1 while counter1 < 4 using no delay[1]/counter1 < 4[1]/I increment counter1[4]/I increment counter1[1]""")
    chain.push(scenario1.steps(2).stepDef.get.steps(3).stepDef.get.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter1[1]/I increment counter1 while counter1 < 4 using no delay[1]/counter1 < 4[1]/I increment counter1[4]/I increment counter1[1]/counter1 < 4 is defined by javascript "4 < 4"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter1[1]/I increment counter1 while counter1 < 4 using no delay[1]/counter1 < 4[1]/I increment counter1[4]/I increment counter1[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter1[1]/I increment counter1 while counter1 < 4 using no delay[1]/counter1 < 4[1]/I increment counter1[4]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter1[1]/I increment counter1 while counter1 < 4 using no delay[1]/counter1 < 4[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter1[1]/I increment counter1 while counter1 < 4 using no delay[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter1[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]""")

    val scenario2 = spec.scenarios(3)
    chain.push(scenario2)
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter2 with if condition[1]""")
    chain.push(scenario2.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter2 with if condition[1]/counter2 is "-1"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter2 with if condition[1]""")
    chain.push(scenario2.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter2 with if condition[1]/condition is defined by javascript "true"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter2 with if condition[1]""")
    chain.push(scenario2.steps(2))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter2 with if condition[1]/I increment counter2[1]""")
    chain.push(scenario2.steps(2).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter2 with if condition[1]/I increment counter2[1]/I increment counter2[1]""")
    chain.push(scenario2.steps(2).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter2 with if condition[1]/I increment counter2[1]/I increment counter2[1]/counter2 is defined by javascript "-1 + 1"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter2 with if condition[1]/I increment counter2[1]/I increment counter2[1]""")
    chain.push(scenario2.steps(2).stepDef.get.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter2 with if condition[1]/I increment counter2[1]/I increment counter2[1]/counter2 < 4 is defined by javascript "0 < 4"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter2 with if condition[1]/I increment counter2[1]/I increment counter2[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter2 with if condition[1]/I increment counter2[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter2 with if condition[1]""")
    chain.push(scenario2.steps(3))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter2 with if condition[1]/I increment counter2 while counter2 < 4 using no delay if condition[1]""")
    chain.push(scenario2.steps(3).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter2 with if condition[1]/I increment counter2 while counter2 < 4 using no delay if condition[1]/condition[1]""")
    chain.push(scenario2.steps(3).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter2 with if condition[1]/I increment counter2 while counter2 < 4 using no delay if condition[1]/condition[1]/I increment counter2 while counter2 < 4 using no delay[1]""")
    chain.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter2 with if condition[1]/I increment counter2 while counter2 < 4 using no delay if condition[1]/condition[1]/I increment counter2 while counter2 < 4 using no delay[1]/counter2 < 4[1]""")
    chain.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter2 with if condition[1]/I increment counter2 while counter2 < 4 using no delay if condition[1]/condition[1]/I increment counter2 while counter2 < 4 using no delay[1]/counter2 < 4[1]/I increment counter2[1]""")
    chain.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(0).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter2 with if condition[1]/I increment counter2 while counter2 < 4 using no delay if condition[1]/condition[1]/I increment counter2 while counter2 < 4 using no delay[1]/counter2 < 4[1]/I increment counter2[1]/I increment counter2[1]""")
    chain.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(0).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter2 with if condition[1]/I increment counter2 while counter2 < 4 using no delay if condition[1]/condition[1]/I increment counter2 while counter2 < 4 using no delay[1]/counter2 < 4[1]/I increment counter2[1]/I increment counter2[1]/counter2 is defined by javascript "0 + 1"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter2 with if condition[1]/I increment counter2 while counter2 < 4 using no delay if condition[1]/condition[1]/I increment counter2 while counter2 < 4 using no delay[1]/counter2 < 4[1]/I increment counter2[1]/I increment counter2[1]""")
    chain.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(0).stepDef.get.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter2 with if condition[1]/I increment counter2 while counter2 < 4 using no delay if condition[1]/condition[1]/I increment counter2 while counter2 < 4 using no delay[1]/counter2 < 4[1]/I increment counter2[1]/I increment counter2[1]/counter2 < 4 is defined by javascript "1 < 4"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter2 with if condition[1]/I increment counter2 while counter2 < 4 using no delay if condition[1]/condition[1]/I increment counter2 while counter2 < 4 using no delay[1]/counter2 < 4[1]/I increment counter2[1]/I increment counter2[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter2 with if condition[1]/I increment counter2 while counter2 < 4 using no delay if condition[1]/condition[1]/I increment counter2 while counter2 < 4 using no delay[1]/counter2 < 4[1]/I increment counter2[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter2 with if condition[1]/I increment counter2 while counter2 < 4 using no delay if condition[1]/condition[1]/I increment counter2 while counter2 < 4 using no delay[1]/counter2 < 4[1]""")
    chain.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter2 with if condition[1]/I increment counter2 while counter2 < 4 using no delay if condition[1]/condition[1]/I increment counter2 while counter2 < 4 using no delay[1]/counter2 < 4[1]/I increment counter2[2]""")
    chain.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(1).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter2 with if condition[1]/I increment counter2 while counter2 < 4 using no delay if condition[1]/condition[1]/I increment counter2 while counter2 < 4 using no delay[1]/counter2 < 4[1]/I increment counter2[2]/I increment counter2[1]""")
    chain.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(1).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter2 with if condition[1]/I increment counter2 while counter2 < 4 using no delay if condition[1]/condition[1]/I increment counter2 while counter2 < 4 using no delay[1]/counter2 < 4[1]/I increment counter2[2]/I increment counter2[1]/counter2 is defined by javascript "1 + 1"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter2 with if condition[1]/I increment counter2 while counter2 < 4 using no delay if condition[1]/condition[1]/I increment counter2 while counter2 < 4 using no delay[1]/counter2 < 4[1]/I increment counter2[2]/I increment counter2[1]""")
    chain.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(1).stepDef.get.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter2 with if condition[1]/I increment counter2 while counter2 < 4 using no delay if condition[1]/condition[1]/I increment counter2 while counter2 < 4 using no delay[1]/counter2 < 4[1]/I increment counter2[2]/I increment counter2[1]/counter2 < 4 is defined by javascript "2 < 4"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter2 with if condition[1]/I increment counter2 while counter2 < 4 using no delay if condition[1]/condition[1]/I increment counter2 while counter2 < 4 using no delay[1]/counter2 < 4[1]/I increment counter2[2]/I increment counter2[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter2 with if condition[1]/I increment counter2 while counter2 < 4 using no delay if condition[1]/condition[1]/I increment counter2 while counter2 < 4 using no delay[1]/counter2 < 4[1]/I increment counter2[2]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter2 with if condition[1]/I increment counter2 while counter2 < 4 using no delay if condition[1]/condition[1]/I increment counter2 while counter2 < 4 using no delay[1]/counter2 < 4[1]""")
    chain.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(2))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter2 with if condition[1]/I increment counter2 while counter2 < 4 using no delay if condition[1]/condition[1]/I increment counter2 while counter2 < 4 using no delay[1]/counter2 < 4[1]/I increment counter2[3]""")
    chain.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(2).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter2 with if condition[1]/I increment counter2 while counter2 < 4 using no delay if condition[1]/condition[1]/I increment counter2 while counter2 < 4 using no delay[1]/counter2 < 4[1]/I increment counter2[3]/I increment counter2[1]""")
    chain.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(2).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter2 with if condition[1]/I increment counter2 while counter2 < 4 using no delay if condition[1]/condition[1]/I increment counter2 while counter2 < 4 using no delay[1]/counter2 < 4[1]/I increment counter2[3]/I increment counter2[1]/counter2 is defined by javascript "2 + 1"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter2 with if condition[1]/I increment counter2 while counter2 < 4 using no delay if condition[1]/condition[1]/I increment counter2 while counter2 < 4 using no delay[1]/counter2 < 4[1]/I increment counter2[3]/I increment counter2[1]""")
    chain.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(2).stepDef.get.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter2 with if condition[1]/I increment counter2 while counter2 < 4 using no delay if condition[1]/condition[1]/I increment counter2 while counter2 < 4 using no delay[1]/counter2 < 4[1]/I increment counter2[3]/I increment counter2[1]/counter2 < 4 is defined by javascript "3 < 4"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter2 with if condition[1]/I increment counter2 while counter2 < 4 using no delay if condition[1]/condition[1]/I increment counter2 while counter2 < 4 using no delay[1]/counter2 < 4[1]/I increment counter2[3]/I increment counter2[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter2 with if condition[1]/I increment counter2 while counter2 < 4 using no delay if condition[1]/condition[1]/I increment counter2 while counter2 < 4 using no delay[1]/counter2 < 4[1]/I increment counter2[3]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter2 with if condition[1]/I increment counter2 while counter2 < 4 using no delay if condition[1]/condition[1]/I increment counter2 while counter2 < 4 using no delay[1]/counter2 < 4[1]""")
    chain.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(3))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter2 with if condition[1]/I increment counter2 while counter2 < 4 using no delay if condition[1]/condition[1]/I increment counter2 while counter2 < 4 using no delay[1]/counter2 < 4[1]/I increment counter2[4]""")
    chain.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(3).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter2 with if condition[1]/I increment counter2 while counter2 < 4 using no delay if condition[1]/condition[1]/I increment counter2 while counter2 < 4 using no delay[1]/counter2 < 4[1]/I increment counter2[4]/I increment counter2[1]""")
    chain.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(3).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter2 with if condition[1]/I increment counter2 while counter2 < 4 using no delay if condition[1]/condition[1]/I increment counter2 while counter2 < 4 using no delay[1]/counter2 < 4[1]/I increment counter2[4]/I increment counter2[1]/counter2 is defined by javascript "3 + 1"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter2 with if condition[1]/I increment counter2 while counter2 < 4 using no delay if condition[1]/condition[1]/I increment counter2 while counter2 < 4 using no delay[1]/counter2 < 4[1]/I increment counter2[4]/I increment counter2[1]""")
    chain.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(3).stepDef.get.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter2 with if condition[1]/I increment counter2 while counter2 < 4 using no delay if condition[1]/condition[1]/I increment counter2 while counter2 < 4 using no delay[1]/counter2 < 4[1]/I increment counter2[4]/I increment counter2[1]/counter2 < 4 is defined by javascript "4 < 4"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter2 with if condition[1]/I increment counter2 while counter2 < 4 using no delay if condition[1]/condition[1]/I increment counter2 while counter2 < 4 using no delay[1]/counter2 < 4[1]/I increment counter2[4]/I increment counter2[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter2 with if condition[1]/I increment counter2 while counter2 < 4 using no delay if condition[1]/condition[1]/I increment counter2 while counter2 < 4 using no delay[1]/counter2 < 4[1]/I increment counter2[4]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter2 with if condition[1]/I increment counter2 while counter2 < 4 using no delay if condition[1]/condition[1]/I increment counter2 while counter2 < 4 using no delay[1]/counter2 < 4[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter2 with if condition[1]/I increment counter2 while counter2 < 4 using no delay if condition[1]/condition[1]/I increment counter2 while counter2 < 4 using no delay[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter2 with if condition[1]/I increment counter2 while counter2 < 4 using no delay if condition[1]/condition[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter2 with if condition[1]/I increment counter2 while counter2 < 4 using no delay if condition[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter2 with if condition[1]""")
    chain.push(scenario2.steps(4))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter2 with if condition[1]/counter2 should be "4"[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]/Increment counter2 with if condition[1]""")
    chain.pop()
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example[1]""")
    chain.pop()
    chain.nodePath should be ("/target/NodeChainTest/while.feature")
    chain.pop()
    chain.nodePath should be ("/")
    chain.pop()
    chain.nodePath should be ("")
  }

}
