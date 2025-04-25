/*
 * Copyright 2024 Branko Juric, Brady Wood
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
import gwen.core.data.DataRecord
import gwen.core.data.DataSource
import gwen.core.eval.EvalEngine
import gwen.core.node.gherkin.GherkinParser
import gwen.core.node.gherkin.SpecNormaliser
import gwen.core.node.gherkin.TagFilter
import gwen.core.state.EnvState

import org.scalatest.matchers.should.Matchers
import org.scalatestplus.mockito.MockitoSugar

import java.io.File

class NodeChainTest extends BaseTest with Matchers with MockitoSugar with SpecNormaliser with GherkinParser with TestModel {

  private val mockTagFilter = mock[TagFilter]

  "Node path" should "be generated for Root on new chain" in {
    
    val unit = FeatureUnit(Root, new File("path/to/file.feature"), Nil, None, mockTagFilter, None)
    val builder = new NodeChainBuilder()
    val chain = builder.push(unit)
    chain.nodePath should be ("/path/to/file.feature")
  }

  "Node path" should "be generated on chain with Unit" in {
    
    val unit = FeatureUnit(Root, new File("path/to/file.feature"), Nil, None, mockTagFilter, None)
    val builder = new NodeChainBuilder()
    val chain = builder.push(unit)
    chain.nodePath should be ("/path/to/file.feature")
  }

  "Node path" should "be generated on chain with Unit with CSV data record" in {
    val dataRecord = new DataRecord(DataSource(new File("data.csv")), Occurrence(2, 2), Nil)
    val unit = FeatureUnit(Root, new File("path/to/file.feature"), Nil, Some(dataRecord), mockTagFilter, None)
    val builder = new NodeChainBuilder()
    val chain = builder.push(unit)
    chain.nodePath should be ("/path/to/file.feature")
  }

  "Node path" should "be generated on chain with Unit with JSON data record" in {
    val dataRecord = new DataRecord(DataSource(new File("data.json")), Occurrence(2, 2), Nil)
    val unit = FeatureUnit(Root, new File("path/to/file.feature"), Nil, Some(dataRecord), mockTagFilter, None)
    val builder = new NodeChainBuilder()
    val chain = builder.push(unit)
    chain.nodePath should be ("/path/to/file.feature")
  }

  "Node paths" should "be generared for all nodes in normalised feature with scenario outlines" in {

    val featureString = """
    Feature: Outline

    Background: background
       Given background step 1
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

    val options = GwenOptions(dryRun = false, parallel = false)
    val spec = normaliseSpec(parseSpec(file).get, None, options)
    val unit = FeatureUnit(Root, file, Nil, None, mockTagFilter, None)
    val builder = new NodeChainBuilder()
    var chain = NodeChain()

    chain.nodePath should be ("/")
    chain = builder.push(unit)
    chain.nodePath should be ("/target/NodeChainTest/outline.feature")
    chain = builder.push(spec)
    chain.nodePath should be ("/target/NodeChainTest/outline.feature/Outline")

    val outline = spec.scenarios(0)

    chain = builder.push(outline)
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>""")
    chain = builder.push(outline.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/string 1 is "<string 1>"""")
    chain = builder.pop()._2
    chain = builder.push(outline.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/string 2 is "<string 2>"""")
    chain = builder.pop()._2
    chain = builder.push(outline.steps(2))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/I join the two strings""")
    chain = builder.pop()._2
    chain = builder.push(outline.steps(3))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/the result should be "<result>"""")
    chain = builder.pop()._2

    val examples = outline.examples
    val example1 = examples(0)

    chain = builder.push(example1)
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Compound words""")

    val scenario1 = example1.scenarios(0)
    chain = builder.push(scenario1)
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Compound words/Joining basket and ball should yield basketball -- Compound words""")
    chain = builder.push(scenario1.background.get)
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Compound words/Joining basket and ball should yield basketball -- Compound words/background + Data table record""")
    chain = builder.push(scenario1.background.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Compound words/Joining basket and ball should yield basketball -- Compound words/background + Data table record/string 1 is "basket"""")
    chain = builder.pop()._2
    chain = builder.push(scenario1.background.get.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Compound words/Joining basket and ball should yield basketball -- Compound words/background + Data table record/string 2 is "ball"""")
    chain = builder.pop()._2
    chain = builder.push(scenario1.background.get.steps(2))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Compound words/Joining basket and ball should yield basketball -- Compound words/background + Data table record/result is "basketball"""")
    chain = builder.pop()._2
    chain = builder.push(scenario1.background.get.steps(3))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Compound words/Joining basket and ball should yield basketball -- Compound words/background + Data table record/background step 1""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Compound words/Joining basket and ball should yield basketball -- Compound words/background + Data table record""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Compound words/Joining basket and ball should yield basketball -- Compound words""")
    chain = builder.push(scenario1.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Compound words/Joining basket and ball should yield basketball -- Compound words/string 1 is "basket"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Compound words/Joining basket and ball should yield basketball -- Compound words""")
    chain = builder.push(scenario1.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Compound words/Joining basket and ball should yield basketball -- Compound words/string 2 is "ball"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Compound words/Joining basket and ball should yield basketball -- Compound words""")
    chain = builder.push(scenario1.steps(2))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Compound words/Joining basket and ball should yield basketball -- Compound words/I join the two strings""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Compound words/Joining basket and ball should yield basketball -- Compound words""")
    chain = builder.push(scenario1.steps(3))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Compound words/Joining basket and ball should yield basketball -- Compound words/the result should be "basketball"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Compound words/Joining basket and ball should yield basketball -- Compound words""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Compound words""")

    val scenario2 = example1.scenarios(1)
    chain = builder.push(scenario2)
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Compound words/Joining any and thing should yield anything -- Compound words[2]""")
    chain = builder.push(scenario2.background.get)
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Compound words/Joining any and thing should yield anything -- Compound words[2]/background + Data table record[2]""")
    chain = builder.push(scenario2.background.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Compound words/Joining any and thing should yield anything -- Compound words[2]/background + Data table record[2]/string 1 is "any"""")
    chain = builder.pop()._2
    chain = builder.push(scenario2.background.get.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Compound words/Joining any and thing should yield anything -- Compound words[2]/background + Data table record[2]/string 2 is "thing"""")
    chain = builder.pop()._2
    chain = builder.push(scenario2.background.get.steps(2))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Compound words/Joining any and thing should yield anything -- Compound words[2]/background + Data table record[2]/result is "anything"""")
    chain = builder.pop()._2
    chain = builder.push(scenario2.background.get.steps(3))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Compound words/Joining any and thing should yield anything -- Compound words[2]/background + Data table record[2]/background step 1""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Compound words/Joining any and thing should yield anything -- Compound words[2]/background + Data table record[2]""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Compound words/Joining any and thing should yield anything -- Compound words[2]""")
    chain = builder.push(scenario2.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Compound words/Joining any and thing should yield anything -- Compound words[2]/string 1 is "any"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Compound words/Joining any and thing should yield anything -- Compound words[2]""")
    chain = builder.push(scenario2.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Compound words/Joining any and thing should yield anything -- Compound words[2]/string 2 is "thing"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Compound words/Joining any and thing should yield anything -- Compound words[2]""")
    chain = builder.push(scenario2.steps(2))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Compound words/Joining any and thing should yield anything -- Compound words[2]/I join the two strings""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Compound words/Joining any and thing should yield anything -- Compound words[2]""")
    chain = builder.push(scenario2.steps(3))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Compound words/Joining any and thing should yield anything -- Compound words[2]/the result should be "anything"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Compound words/Joining any and thing should yield anything -- Compound words[2]""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Compound words""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>""")

    val example2 = examples(1)
    chain = builder.push(example2)
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Nonsensical compound words""")
    
    val scenario3 = example2.scenarios(0)
    chain = builder.push(scenario3)
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Nonsensical compound words/Joining howdy and doo should yield howdydoo -- Nonsensical compound words""")
    chain = builder.push(scenario3.background.get)
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Nonsensical compound words/Joining howdy and doo should yield howdydoo -- Nonsensical compound words/background + Data table record""")
    chain = builder.push(scenario3.background.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Nonsensical compound words/Joining howdy and doo should yield howdydoo -- Nonsensical compound words/background + Data table record/string 1 is "howdy"""")
    chain = builder.pop()._2
    chain = builder.push(scenario3.background.get.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Nonsensical compound words/Joining howdy and doo should yield howdydoo -- Nonsensical compound words/background + Data table record/string 2 is "doo"""")
    chain = builder.pop()._2
    chain = builder.push(scenario3.background.get.steps(2))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Nonsensical compound words/Joining howdy and doo should yield howdydoo -- Nonsensical compound words/background + Data table record/result is "howdydoo"""")
    chain = builder.pop()._2
    chain = builder.push(scenario3.background.get.steps(3))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Nonsensical compound words/Joining howdy and doo should yield howdydoo -- Nonsensical compound words/background + Data table record/background step 1""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Nonsensical compound words/Joining howdy and doo should yield howdydoo -- Nonsensical compound words/background + Data table record""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Nonsensical compound words/Joining howdy and doo should yield howdydoo -- Nonsensical compound words""")
    chain = builder.push(scenario3.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Nonsensical compound words/Joining howdy and doo should yield howdydoo -- Nonsensical compound words/string 1 is "howdy"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Nonsensical compound words/Joining howdy and doo should yield howdydoo -- Nonsensical compound words""")
    chain = builder.push(scenario3.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Nonsensical compound words/Joining howdy and doo should yield howdydoo -- Nonsensical compound words/string 2 is "doo"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Nonsensical compound words/Joining howdy and doo should yield howdydoo -- Nonsensical compound words""")
    chain = builder.push(scenario3.steps(2))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Nonsensical compound words/Joining howdy and doo should yield howdydoo -- Nonsensical compound words/I join the two strings""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Nonsensical compound words/Joining howdy and doo should yield howdydoo -- Nonsensical compound words""")
    chain = builder.push(scenario3.steps(3))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Nonsensical compound words/Joining howdy and doo should yield howdydoo -- Nonsensical compound words/the result should be "howdydoo"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Nonsensical compound words/Joining howdy and doo should yield howdydoo -- Nonsensical compound words""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Nonsensical compound words""")

    val scenario4 = example2.scenarios(1)
    chain = builder.push(scenario4)
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Nonsensical compound words/Joining yep and ok should yield yepok -- Nonsensical compound words[2]""")
    chain = builder.push(scenario4.background.get)
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Nonsensical compound words/Joining yep and ok should yield yepok -- Nonsensical compound words[2]/background + Data table record[2]""")
    chain = builder.push(scenario4.background.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Nonsensical compound words/Joining yep and ok should yield yepok -- Nonsensical compound words[2]/background + Data table record[2]/string 1 is "yep"""")
    chain = builder.pop()._2
    chain = builder.push(scenario4.background.get.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Nonsensical compound words/Joining yep and ok should yield yepok -- Nonsensical compound words[2]/background + Data table record[2]/string 2 is "ok"""")
    chain = builder.pop()._2
    chain = builder.push(scenario4.background.get.steps(2))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Nonsensical compound words/Joining yep and ok should yield yepok -- Nonsensical compound words[2]/background + Data table record[2]/result is "yepok"""")
    chain = builder.pop()._2
    chain = builder.push(scenario4.background.get.steps(3))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Nonsensical compound words/Joining yep and ok should yield yepok -- Nonsensical compound words[2]/background + Data table record[2]/background step 1""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Nonsensical compound words/Joining yep and ok should yield yepok -- Nonsensical compound words[2]/background + Data table record[2]""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Nonsensical compound words/Joining yep and ok should yield yepok -- Nonsensical compound words[2]""")
    chain = builder.push(scenario4.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Nonsensical compound words/Joining yep and ok should yield yepok -- Nonsensical compound words[2]/string 1 is "yep"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Nonsensical compound words/Joining yep and ok should yield yepok -- Nonsensical compound words[2]""")
    chain = builder.push(scenario4.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Nonsensical compound words/Joining yep and ok should yield yepok -- Nonsensical compound words[2]/string 2 is "ok"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Nonsensical compound words/Joining yep and ok should yield yepok -- Nonsensical compound words[2]""")
    chain = builder.push(scenario4.steps(2))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Nonsensical compound words/Joining yep and ok should yield yepok -- Nonsensical compound words[2]/I join the two strings""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Nonsensical compound words/Joining yep and ok should yield yepok -- Nonsensical compound words[2]""")
    chain = builder.push(scenario4.steps(3))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Nonsensical compound words/Joining yep and ok should yield yepok -- Nonsensical compound words[2]/the result should be "yepok"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Nonsensical compound words/Joining yep and ok should yield yepok -- Nonsensical compound words[2]""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/Nonsensical compound words""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>""")

    val example3 = examples(2)
    chain = builder.push(example3)
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/""")
    
    val scenario5 = example3.scenarios(0)
    chain = builder.push(scenario5)
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>//Joining ding and dong should yield dingdong""")
    chain = builder.push(scenario5.background.get)
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>//Joining ding and dong should yield dingdong/background + Data table record""")
    chain = builder.push(scenario5.background.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>//Joining ding and dong should yield dingdong/background + Data table record/string 1 is "ding"""")
    chain = builder.pop()._2
    chain = builder.push(scenario5.background.get.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>//Joining ding and dong should yield dingdong/background + Data table record/string 2 is "dong"""")
    chain = builder.pop()._2
    chain = builder.push(scenario5.background.get.steps(2))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>//Joining ding and dong should yield dingdong/background + Data table record/result is "dingdong"""")
    chain = builder.pop()._2
    chain = builder.push(scenario5.background.get.steps(3))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>//Joining ding and dong should yield dingdong/background + Data table record/background step 1""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>//Joining ding and dong should yield dingdong/background + Data table record""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>//Joining ding and dong should yield dingdong""")
    chain = builder.push(scenario5.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>//Joining ding and dong should yield dingdong/string 1 is "ding"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>//Joining ding and dong should yield dingdong""")
    chain = builder.push(scenario5.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>//Joining ding and dong should yield dingdong/string 2 is "dong"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>//Joining ding and dong should yield dingdong""")
    chain = builder.push(scenario5.steps(2))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>//Joining ding and dong should yield dingdong/I join the two strings""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>//Joining ding and dong should yield dingdong""")
    chain = builder.push(scenario5.steps(3))
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>//Joining ding and dong should yield dingdong/the result should be "dingdong"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>//Joining ding and dong should yield dingdong""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>/""")

    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/outline.feature/Outline/Joining <string 1> and <string 2> should yield <result>""")
    chain = builder.pop()._2
    chain.nodePath should be ("/target/NodeChainTest/outline.feature/Outline")
    chain = builder.pop()._2
    chain.nodePath should be ("/target/NodeChainTest/outline.feature")
    chain = builder.pop()._2
    chain.nodePath should be ("/")

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
    val ctx = EvalEngine().init(options, EnvState())
    val result = EvalEngine().evaluateUnit(unit, ctx).headOption

    val spec = result.get.spec
    val builder = new NodeChainBuilder()
    var chain = NodeChain()
    
    chain.nodePath should be ("/")
    chain = builder.push(unit)
    chain.nodePath should be ("/target/NodeChainTest/for-each.feature")
    chain = builder.push(spec)
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition""")

    val stepDef1 = spec.scenarios(0)
    chain = builder.push(stepDef1)
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition/I click option "<option>" in "<type>"""")
    chain = builder.push(stepDef1.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition/I click option "<option>" in "<type>"/the option is "$<option>"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition/I click option "<option>" in "<type>"""")
    chain = builder.push(stepDef1.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition/I click option "<option>" in "<type>"/the type is "$<type>"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition/I click option "<option>" in "<type>"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition""")

    val stepDef2 = spec.scenarios(1)
    chain = builder.push(stepDef2)
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition/I click checbox in "<type>"""")
    chain = builder.push(stepDef2.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition/I click checbox in "<type>"/options is "~option1,option2,option3"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition/I click checbox in "<type>"""")
    chain = builder.push(stepDef2.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition/I click checbox in "<type>"/condition is defined by javascript "true"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition/I click checbox in "<type>"""")
    chain = builder.push(stepDef2.steps(2))
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition/I click checbox in "<type>"/I click option "${opt}" in "$<type>" for each opt in options delimited by "," if condition""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition/I click checbox in "<type>"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition""")

    val scenario = spec.scenarios(2)
    chain = builder.push(scenario)
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition/I process options""")
    chain = builder.push(scenario.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition/I process options/the type is "my type"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition/I process options""")
    chain = builder.push(scenario.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition/I process options/I click checbox in "group"""")
    chain = builder.push(scenario.steps(1).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition/I process options/I click checbox in "group"/I click checbox in "<type>"""")
    chain = builder.push(scenario.steps(1).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition/I process options/I click checbox in "group"/I click checbox in "<type>"/options is "~option1,option2,option3"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition/I process options/I click checbox in "group"/I click checbox in "<type>"""")
    chain = builder.push(scenario.steps(1).stepDef.get.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition/I process options/I click checbox in "group"/I click checbox in "<type>"/condition is defined by javascript "true"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition/I process options/I click checbox in "group"/I click checbox in "<type>"""")
    chain = builder.push(scenario.steps(1).stepDef.get.steps(2))
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition/I process options/I click checbox in "group"/I click checbox in "<type>"/I click option "${opt}" in "group" for each opt in options delimited by "," if condition""")
    chain = builder.push(scenario.steps(1).stepDef.get.steps(2).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition/I process options/I click checbox in "group"/I click checbox in "<type>"/I click option "${opt}" in "group" for each opt in options delimited by "," if condition/condition""")
    chain = builder.push(scenario.steps(1).stepDef.get.steps(2).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition/I process options/I click checbox in "group"/I click checbox in "<type>"/I click option "${opt}" in "group" for each opt in options delimited by "," if condition/condition/I click option "${opt}" in "group" for each opt in options delimited by ","""")
    chain = builder.push(scenario.steps(1).stepDef.get.steps(2).stepDef.get.steps(0).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition/I process options/I click checbox in "group"/I click checbox in "<type>"/I click option "${opt}" in "group" for each opt in options delimited by "," if condition/condition/I click option "${opt}" in "group" for each opt in options delimited by ","/opt""")
    chain = builder.push(scenario.steps(1).stepDef.get.steps(2).stepDef.get.steps(0).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition/I process options/I click checbox in "group"/I click checbox in "<type>"/I click option "${opt}" in "group" for each opt in options delimited by "," if condition/condition/I click option "${opt}" in "group" for each opt in options delimited by ","/opt/I click option "~option1" in "group"[1]""")
    chain = builder.push(scenario.steps(1).stepDef.get.steps(2).stepDef.get.steps(0).stepDef.get.steps(0).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition/I process options/I click checbox in "group"/I click checbox in "<type>"/I click option "${opt}" in "group" for each opt in options delimited by "," if condition/condition/I click option "${opt}" in "group" for each opt in options delimited by ","/opt/I click option "~option1" in "group"[1]/I click option "<option>" in "<type>"""")
    chain = builder.push(scenario.steps(1).stepDef.get.steps(2).stepDef.get.steps(0).stepDef.get.steps(0).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition/I process options/I click checbox in "group"/I click checbox in "<type>"/I click option "${opt}" in "group" for each opt in options delimited by "," if condition/condition/I click option "${opt}" in "group" for each opt in options delimited by ","/opt/I click option "~option1" in "group"[1]/I click option "<option>" in "<type>"/the option is "~option1"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition/I process options/I click checbox in "group"/I click checbox in "<type>"/I click option "${opt}" in "group" for each opt in options delimited by "," if condition/condition/I click option "${opt}" in "group" for each opt in options delimited by ","/opt/I click option "~option1" in "group"[1]/I click option "<option>" in "<type>"""")
    chain = builder.push(scenario.steps(1).stepDef.get.steps(2).stepDef.get.steps(0).stepDef.get.steps(0).stepDef.get.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition/I process options/I click checbox in "group"/I click checbox in "<type>"/I click option "${opt}" in "group" for each opt in options delimited by "," if condition/condition/I click option "${opt}" in "group" for each opt in options delimited by ","/opt/I click option "~option1" in "group"[1]/I click option "<option>" in "<type>"/the type is "group"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition/I process options/I click checbox in "group"/I click checbox in "<type>"/I click option "${opt}" in "group" for each opt in options delimited by "," if condition/condition/I click option "${opt}" in "group" for each opt in options delimited by ","/opt/I click option "~option1" in "group"[1]/I click option "<option>" in "<type>"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition/I process options/I click checbox in "group"/I click checbox in "<type>"/I click option "${opt}" in "group" for each opt in options delimited by "," if condition/condition/I click option "${opt}" in "group" for each opt in options delimited by ","/opt/I click option "~option1" in "group"[1]""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition/I process options/I click checbox in "group"/I click checbox in "<type>"/I click option "${opt}" in "group" for each opt in options delimited by "," if condition/condition/I click option "${opt}" in "group" for each opt in options delimited by ","/opt""")
    chain = builder.push(scenario.steps(1).stepDef.get.steps(2).stepDef.get.steps(0).stepDef.get.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition/I process options/I click checbox in "group"/I click checbox in "<type>"/I click option "${opt}" in "group" for each opt in options delimited by "," if condition/condition/I click option "${opt}" in "group" for each opt in options delimited by ","/opt/I click option "option2" in "group"[2]""")
    chain = builder.push(scenario.steps(1).stepDef.get.steps(2).stepDef.get.steps(0).stepDef.get.steps(1).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition/I process options/I click checbox in "group"/I click checbox in "<type>"/I click option "${opt}" in "group" for each opt in options delimited by "," if condition/condition/I click option "${opt}" in "group" for each opt in options delimited by ","/opt/I click option "option2" in "group"[2]/I click option "<option>" in "<type>"""")
    chain = builder.push(scenario.steps(1).stepDef.get.steps(2).stepDef.get.steps(0).stepDef.get.steps(1).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition/I process options/I click checbox in "group"/I click checbox in "<type>"/I click option "${opt}" in "group" for each opt in options delimited by "," if condition/condition/I click option "${opt}" in "group" for each opt in options delimited by ","/opt/I click option "option2" in "group"[2]/I click option "<option>" in "<type>"/the option is "option2"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition/I process options/I click checbox in "group"/I click checbox in "<type>"/I click option "${opt}" in "group" for each opt in options delimited by "," if condition/condition/I click option "${opt}" in "group" for each opt in options delimited by ","/opt/I click option "option2" in "group"[2]/I click option "<option>" in "<type>"""")
    chain = builder.push(scenario.steps(1).stepDef.get.steps(2).stepDef.get.steps(0).stepDef.get.steps(1).stepDef.get.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition/I process options/I click checbox in "group"/I click checbox in "<type>"/I click option "${opt}" in "group" for each opt in options delimited by "," if condition/condition/I click option "${opt}" in "group" for each opt in options delimited by ","/opt/I click option "option2" in "group"[2]/I click option "<option>" in "<type>"/the type is "group"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition/I process options/I click checbox in "group"/I click checbox in "<type>"/I click option "${opt}" in "group" for each opt in options delimited by "," if condition/condition/I click option "${opt}" in "group" for each opt in options delimited by ","/opt/I click option "option2" in "group"[2]/I click option "<option>" in "<type>"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition/I process options/I click checbox in "group"/I click checbox in "<type>"/I click option "${opt}" in "group" for each opt in options delimited by "," if condition/condition/I click option "${opt}" in "group" for each opt in options delimited by ","/opt/I click option "option2" in "group"[2]""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition/I process options/I click checbox in "group"/I click checbox in "<type>"/I click option "${opt}" in "group" for each opt in options delimited by "," if condition/condition/I click option "${opt}" in "group" for each opt in options delimited by ","/opt""")
    chain = builder.push(scenario.steps(1).stepDef.get.steps(2).stepDef.get.steps(0).stepDef.get.steps(2))
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition/I process options/I click checbox in "group"/I click checbox in "<type>"/I click option "${opt}" in "group" for each opt in options delimited by "," if condition/condition/I click option "${opt}" in "group" for each opt in options delimited by ","/opt/I click option "option3" in "group"[3]""")
    chain = builder.push(scenario.steps(1).stepDef.get.steps(2).stepDef.get.steps(0).stepDef.get.steps(2).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition/I process options/I click checbox in "group"/I click checbox in "<type>"/I click option "${opt}" in "group" for each opt in options delimited by "," if condition/condition/I click option "${opt}" in "group" for each opt in options delimited by ","/opt/I click option "option3" in "group"[3]/I click option "<option>" in "<type>"""")
    chain = builder.push(scenario.steps(1).stepDef.get.steps(2).stepDef.get.steps(0).stepDef.get.steps(2).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition/I process options/I click checbox in "group"/I click checbox in "<type>"/I click option "${opt}" in "group" for each opt in options delimited by "," if condition/condition/I click option "${opt}" in "group" for each opt in options delimited by ","/opt/I click option "option3" in "group"[3]/I click option "<option>" in "<type>"/the option is "option3"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition/I process options/I click checbox in "group"/I click checbox in "<type>"/I click option "${opt}" in "group" for each opt in options delimited by "," if condition/condition/I click option "${opt}" in "group" for each opt in options delimited by ","/opt/I click option "option3" in "group"[3]/I click option "<option>" in "<type>"""")
    chain = builder.push(scenario.steps(1).stepDef.get.steps(2).stepDef.get.steps(0).stepDef.get.steps(2).stepDef.get.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition/I process options/I click checbox in "group"/I click checbox in "<type>"/I click option "${opt}" in "group" for each opt in options delimited by "," if condition/condition/I click option "${opt}" in "group" for each opt in options delimited by ","/opt/I click option "option3" in "group"[3]/I click option "<option>" in "<type>"/the type is "group"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition/I process options/I click checbox in "group"/I click checbox in "<type>"/I click option "${opt}" in "group" for each opt in options delimited by "," if condition/condition/I click option "${opt}" in "group" for each opt in options delimited by ","/opt/I click option "option3" in "group"[3]/I click option "<option>" in "<type>"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition/I process options/I click checbox in "group"/I click checbox in "<type>"/I click option "${opt}" in "group" for each opt in options delimited by "," if condition/condition/I click option "${opt}" in "group" for each opt in options delimited by ","/opt/I click option "option3" in "group"[3]""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition/I process options/I click checbox in "group"/I click checbox in "<type>"/I click option "${opt}" in "group" for each opt in options delimited by "," if condition/condition/I click option "${opt}" in "group" for each opt in options delimited by ","/opt""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition/I process options/I click checbox in "group"/I click checbox in "<type>"/I click option "${opt}" in "group" for each opt in options delimited by "," if condition/condition/I click option "${opt}" in "group" for each opt in options delimited by ","""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition/I process options/I click checbox in "group"/I click checbox in "<type>"/I click option "${opt}" in "group" for each opt in options delimited by "," if condition/condition""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition/I process options/I click checbox in "group"/I click checbox in "<type>"/I click option "${opt}" in "group" for each opt in options delimited by "," if condition""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition/I process options/I click checbox in "group"/I click checbox in "<type>"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition/I process options/I click checbox in "group"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition/I process options""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/for-each.feature/For each delimited value with if condition""")
    chain = builder.pop()._2
    chain.nodePath should be ("/target/NodeChainTest/for-each.feature")
    chain = builder.pop()._2
    chain.nodePath should be ("/")
  }

  "Node paths" should "be generared for all nodes in evaluated feature with until loop" in {

    val featureString = """
      Feature: Repeat until example

      @StepDef
      @Action
      Scenario: I increment counter
          Given counter is defined by javascript "${counter} + 1"
            And counter > 3 is defined by javascript "${counter} > 3"

      Scenario: Increment counter
          Given counter is "0"
          When @Delay('0s') I increment counter until counter > 3
          Then counter should be "4"

      Scenario: Increment counter with if condition
          Given counter is "-1"
            And condition is defined by javascript "true"
          When I increment counter
            And @Delay('0s') I increment counter until counter > 3 if condition
          Then counter should be "4"
      """

    val rootDir: File = new File("target" +  File.separator + this.getClass.getSimpleName)
    val file = new File(rootDir.getPath, "until.feature")
    file.getParentFile.mkdirs()
    file.createNewFile()
    file.writeText(featureString)

    val unit = FeatureUnit(Root, file, Nil, None, new TagFilter(Nil), None)
    val options = GwenOptions(features = List(file))
    val ctx = EvalEngine().init(options, EnvState())
    val result = EvalEngine().evaluateUnit(unit, ctx).headOption
    val spec = result.get.spec

    val builder = new NodeChainBuilder()
    var chain = NodeChain()
    
    chain.nodePath should be ("/")
    chain = builder.push(unit)
    chain.nodePath should be ("/target/NodeChainTest/until.feature")
    chain = builder.push(spec)
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example""")
    
    val stepDef1 = spec.scenarios(0)
    chain = builder.push(stepDef1)
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/I increment counter""")
    chain = builder.push(stepDef1.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/I increment counter/counter is defined by javascript "${counter} + 1"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/I increment counter""")
    chain = builder.push(stepDef1.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/I increment counter/counter > 3 is defined by javascript "${counter} > 3"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/I increment counter""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example""")

    val scenario1 = spec.scenarios(1)
    chain = builder.push(scenario1)
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter""")
    chain = builder.push(scenario1.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter/counter is "0"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter""")
    chain = builder.push(scenario1.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter/I increment counter until counter > 3""")
    chain = builder.push(scenario1.steps(1).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter/I increment counter until counter > 3/counter > 3""")
    chain = builder.push(scenario1.steps(1).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter/I increment counter until counter > 3/counter > 3/I increment counter[1]""")
    chain = builder.push(scenario1.steps(1).stepDef.get.steps(0).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter/I increment counter until counter > 3/counter > 3/I increment counter[1]/I increment counter""")
    chain = builder.push(scenario1.steps(1).stepDef.get.steps(0).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter/I increment counter until counter > 3/counter > 3/I increment counter[1]/I increment counter/counter is defined by javascript "0 + 1"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter/I increment counter until counter > 3/counter > 3/I increment counter[1]/I increment counter""")
    chain = builder.push(scenario1.steps(1).stepDef.get.steps(0).stepDef.get.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter/I increment counter until counter > 3/counter > 3/I increment counter[1]/I increment counter/counter > 3 is defined by javascript "1 > 3"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter/I increment counter until counter > 3/counter > 3/I increment counter[1]/I increment counter""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter/I increment counter until counter > 3/counter > 3/I increment counter[1]""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter/I increment counter until counter > 3/counter > 3""")
    chain = builder.push(scenario1.steps(1).stepDef.get.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter/I increment counter until counter > 3/counter > 3/I increment counter[2]""")
    chain = builder.push(scenario1.steps(1).stepDef.get.steps(1).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter/I increment counter until counter > 3/counter > 3/I increment counter[2]/I increment counter""")
    chain = builder.push(scenario1.steps(1).stepDef.get.steps(1).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter/I increment counter until counter > 3/counter > 3/I increment counter[2]/I increment counter/counter is defined by javascript "1 + 1"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter/I increment counter until counter > 3/counter > 3/I increment counter[2]/I increment counter""")
    chain = builder.push(scenario1.steps(1).stepDef.get.steps(1).stepDef.get.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter/I increment counter until counter > 3/counter > 3/I increment counter[2]/I increment counter/counter > 3 is defined by javascript "2 > 3"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter/I increment counter until counter > 3/counter > 3/I increment counter[2]/I increment counter""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter/I increment counter until counter > 3/counter > 3/I increment counter[2]""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter/I increment counter until counter > 3/counter > 3""")
    chain = builder.push(scenario1.steps(1).stepDef.get.steps(2))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter/I increment counter until counter > 3/counter > 3/I increment counter[3]""")
    chain = builder.push(scenario1.steps(1).stepDef.get.steps(2).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter/I increment counter until counter > 3/counter > 3/I increment counter[3]/I increment counter""")
    chain = builder.push(scenario1.steps(1).stepDef.get.steps(2).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter/I increment counter until counter > 3/counter > 3/I increment counter[3]/I increment counter/counter is defined by javascript "2 + 1"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter/I increment counter until counter > 3/counter > 3/I increment counter[3]/I increment counter""")
    chain = builder.push(scenario1.steps(1).stepDef.get.steps(2).stepDef.get.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter/I increment counter until counter > 3/counter > 3/I increment counter[3]/I increment counter/counter > 3 is defined by javascript "3 > 3"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter/I increment counter until counter > 3/counter > 3/I increment counter[3]/I increment counter""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter/I increment counter until counter > 3/counter > 3/I increment counter[3]""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter/I increment counter until counter > 3/counter > 3""")
    chain = builder.push(scenario1.steps(1).stepDef.get.steps(3))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter/I increment counter until counter > 3/counter > 3/I increment counter[4]""")
    chain = builder.push(scenario1.steps(1).stepDef.get.steps(3).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter/I increment counter until counter > 3/counter > 3/I increment counter[4]/I increment counter""")
    chain = builder.push(scenario1.steps(1).stepDef.get.steps(3).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter/I increment counter until counter > 3/counter > 3/I increment counter[4]/I increment counter/counter is defined by javascript "3 + 1"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter/I increment counter until counter > 3/counter > 3/I increment counter[4]/I increment counter""")
    chain = builder.push(scenario1.steps(1).stepDef.get.steps(3).stepDef.get.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter/I increment counter until counter > 3/counter > 3/I increment counter[4]/I increment counter/counter > 3 is defined by javascript "4 > 3"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter/I increment counter until counter > 3/counter > 3/I increment counter[4]/I increment counter""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter/I increment counter until counter > 3/counter > 3/I increment counter[4]""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter/I increment counter until counter > 3/counter > 3""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter/I increment counter until counter > 3""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example""")

    val scenario2 = spec.scenarios(2)
    chain = builder.push(scenario2)
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter with if condition""")
    chain = builder.push(scenario2.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter with if condition/counter is "-1"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter with if condition""")
    chain = builder.push(scenario2.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter with if condition/condition is defined by javascript "true"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter with if condition""")
    chain = builder.push(scenario2.steps(2))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter with if condition/I increment counter""")
    chain = builder.push(scenario2.steps(2).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter with if condition/I increment counter/I increment counter""")
    chain = builder.push(scenario2.steps(2).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter with if condition/I increment counter/I increment counter/counter is defined by javascript "-1 + 1"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter with if condition/I increment counter/I increment counter""")
    chain = builder.push(scenario2.steps(2).stepDef.get.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter with if condition/I increment counter/I increment counter/counter > 3 is defined by javascript "0 > 3"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter with if condition/I increment counter/I increment counter""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter with if condition/I increment counter""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter with if condition""")
    chain = builder.push(scenario2.steps(3))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter with if condition/I increment counter until counter > 3 if condition""")
    chain = builder.push(scenario2.steps(3).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter with if condition/I increment counter until counter > 3 if condition/condition""")
    chain = builder.push(scenario2.steps(3).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter with if condition/I increment counter until counter > 3 if condition/condition/I increment counter until counter > 3""")
    chain = builder.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter with if condition/I increment counter until counter > 3 if condition/condition/I increment counter until counter > 3/counter > 3""")
    chain = builder.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter with if condition/I increment counter until counter > 3 if condition/condition/I increment counter until counter > 3/counter > 3/I increment counter[1]""")
    chain = builder.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(0).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter with if condition/I increment counter until counter > 3 if condition/condition/I increment counter until counter > 3/counter > 3/I increment counter[1]/I increment counter""")
    chain = builder.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(0).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter with if condition/I increment counter until counter > 3 if condition/condition/I increment counter until counter > 3/counter > 3/I increment counter[1]/I increment counter/counter is defined by javascript "0 + 1"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter with if condition/I increment counter until counter > 3 if condition/condition/I increment counter until counter > 3/counter > 3/I increment counter[1]/I increment counter""")
    chain = builder.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(0).stepDef.get.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter with if condition/I increment counter until counter > 3 if condition/condition/I increment counter until counter > 3/counter > 3/I increment counter[1]/I increment counter/counter > 3 is defined by javascript "1 > 3"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter with if condition/I increment counter until counter > 3 if condition/condition/I increment counter until counter > 3/counter > 3/I increment counter[1]/I increment counter""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter with if condition/I increment counter until counter > 3 if condition/condition/I increment counter until counter > 3/counter > 3/I increment counter[1]""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter with if condition/I increment counter until counter > 3 if condition/condition/I increment counter until counter > 3/counter > 3""")
    chain = builder.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter with if condition/I increment counter until counter > 3 if condition/condition/I increment counter until counter > 3/counter > 3/I increment counter[2]""")
    chain = builder.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(1).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter with if condition/I increment counter until counter > 3 if condition/condition/I increment counter until counter > 3/counter > 3/I increment counter[2]/I increment counter""")
    chain = builder.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(1).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter with if condition/I increment counter until counter > 3 if condition/condition/I increment counter until counter > 3/counter > 3/I increment counter[2]/I increment counter/counter is defined by javascript "1 + 1"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter with if condition/I increment counter until counter > 3 if condition/condition/I increment counter until counter > 3/counter > 3/I increment counter[2]/I increment counter""")
    chain = builder.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(1).stepDef.get.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter with if condition/I increment counter until counter > 3 if condition/condition/I increment counter until counter > 3/counter > 3/I increment counter[2]/I increment counter/counter > 3 is defined by javascript "2 > 3"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter with if condition/I increment counter until counter > 3 if condition/condition/I increment counter until counter > 3/counter > 3/I increment counter[2]/I increment counter""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter with if condition/I increment counter until counter > 3 if condition/condition/I increment counter until counter > 3/counter > 3/I increment counter[2]""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter with if condition/I increment counter until counter > 3 if condition/condition/I increment counter until counter > 3/counter > 3""")
    chain = builder.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(2))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter with if condition/I increment counter until counter > 3 if condition/condition/I increment counter until counter > 3/counter > 3/I increment counter[3]""")
    chain = builder.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(2).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter with if condition/I increment counter until counter > 3 if condition/condition/I increment counter until counter > 3/counter > 3/I increment counter[3]/I increment counter""")
    chain = builder.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(2).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter with if condition/I increment counter until counter > 3 if condition/condition/I increment counter until counter > 3/counter > 3/I increment counter[3]/I increment counter/counter is defined by javascript "2 + 1"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter with if condition/I increment counter until counter > 3 if condition/condition/I increment counter until counter > 3/counter > 3/I increment counter[3]/I increment counter""")
    chain = builder.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(2).stepDef.get.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter with if condition/I increment counter until counter > 3 if condition/condition/I increment counter until counter > 3/counter > 3/I increment counter[3]/I increment counter/counter > 3 is defined by javascript "3 > 3"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter with if condition/I increment counter until counter > 3 if condition/condition/I increment counter until counter > 3/counter > 3/I increment counter[3]/I increment counter""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter with if condition/I increment counter until counter > 3 if condition/condition/I increment counter until counter > 3/counter > 3/I increment counter[3]""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter with if condition/I increment counter until counter > 3 if condition/condition/I increment counter until counter > 3/counter > 3""")
    chain = builder.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(3))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter with if condition/I increment counter until counter > 3 if condition/condition/I increment counter until counter > 3/counter > 3/I increment counter[4]""")
    chain = builder.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(3).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter with if condition/I increment counter until counter > 3 if condition/condition/I increment counter until counter > 3/counter > 3/I increment counter[4]/I increment counter""")
    chain = builder.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(3).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter with if condition/I increment counter until counter > 3 if condition/condition/I increment counter until counter > 3/counter > 3/I increment counter[4]/I increment counter/counter is defined by javascript "3 + 1"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter with if condition/I increment counter until counter > 3 if condition/condition/I increment counter until counter > 3/counter > 3/I increment counter[4]/I increment counter""")
    chain = builder.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(3).stepDef.get.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter with if condition/I increment counter until counter > 3 if condition/condition/I increment counter until counter > 3/counter > 3/I increment counter[4]/I increment counter/counter > 3 is defined by javascript "4 > 3"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter with if condition/I increment counter until counter > 3 if condition/condition/I increment counter until counter > 3/counter > 3/I increment counter[4]/I increment counter""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter with if condition/I increment counter until counter > 3 if condition/condition/I increment counter until counter > 3/counter > 3/I increment counter[4]""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter with if condition/I increment counter until counter > 3 if condition/condition/I increment counter until counter > 3/counter > 3""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter with if condition/I increment counter until counter > 3 if condition/condition/I increment counter until counter > 3""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter with if condition/I increment counter until counter > 3 if condition/condition""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter with if condition/I increment counter until counter > 3 if condition""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter with if condition""")
    chain = builder.push(scenario2.steps(4))
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter with if condition/counter should be "4"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example/Increment counter with if condition""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/until.feature/Repeat until example""")
    chain = builder.pop()._2
    chain.nodePath should be ("/target/NodeChainTest/until.feature")
    chain = builder.pop()._2
    chain.nodePath should be ("/")
  }

  "Node paths" should "be generared for all nodes in evaluated feature with while loop" in {

    val featureString = """
      Feature: Repeat while example

      @StepDef
      @Action
      Scenario: I increment counter1
          Given counter1 is defined by javascript "${counter1} + 1"
            And counter1 < 4 is defined by javascript "${counter1} < 4"

      @StepDef
      @Action
      Scenario: I increment counter2
          Given counter2 is defined by javascript "${counter2} + 1"
            And counter2 < 4 is defined by javascript "${counter2} < 4"

      Scenario: Increment counter1
         Given counter1 is "-1"
          When I increment counter1
           And @Delay('0s') I increment counter1 while counter1 < 4
          Then counter1 should be "4"

      Scenario: Increment counter2 with if condition
          Given counter2 is "-1"
            And condition is defined by javascript "true"
           When I increment counter2
            And @Delay("0s") I increment counter2 while counter2 < 4 if condition
          Then counter2 should be "4"
       """

    val rootDir: File = new File("target" +  File.separator + this.getClass.getSimpleName)
    val file = new File(rootDir.getPath, "while.feature")
    file.getParentFile.mkdirs()
    file.createNewFile()
    file.writeText(featureString)

    val unit = FeatureUnit(Root, file, Nil, None, new TagFilter(Nil), None)
    val options = GwenOptions(features = List(file))
    val ctx = EvalEngine().init(options, EnvState())
    val result = EvalEngine().evaluateUnit(unit, ctx).headOption
    val spec = result.get.spec

    val builder = new NodeChainBuilder()
    var chain = NodeChain()
    
    chain.nodePath should be ("/")
    chain = builder.push(unit)
    chain.nodePath should be ("/target/NodeChainTest/while.feature")
    chain = builder.push(spec)
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example""")
    
    val stepDef1 = spec.scenarios(0)
    chain = builder.push(stepDef1)
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/I increment counter1""")
    chain = builder.push(stepDef1.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/I increment counter1/counter1 is defined by javascript "${counter1} + 1"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/I increment counter1""")
    chain = builder.push(stepDef1.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/I increment counter1/counter1 < 4 is defined by javascript "${counter1} < 4"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/I increment counter1""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example""")

    val stepDef2 = spec.scenarios(1)
    chain = builder.push(stepDef2)
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/I increment counter2""")
    chain = builder.push(stepDef2.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/I increment counter2/counter2 is defined by javascript "${counter2} + 1"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/I increment counter2""")
    chain = builder.push(stepDef2.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/I increment counter2/counter2 < 4 is defined by javascript "${counter2} < 4"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/I increment counter2""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example""")

    val scenario1 = spec.scenarios(2)
    chain = builder.push(scenario1)
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter1""")
    chain = builder.push(scenario1.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter1/counter1 is "-1"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter1""")
    chain = builder.push(scenario1.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter1/I increment counter1""")
    chain = builder.push(scenario1.steps(1).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter1/I increment counter1/I increment counter1""")
    chain = builder.push(scenario1.steps(1).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter1/I increment counter1/I increment counter1/counter1 is defined by javascript "-1 + 1"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter1/I increment counter1/I increment counter1""")
    chain = builder.push(scenario1.steps(1).stepDef.get.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter1/I increment counter1/I increment counter1/counter1 < 4 is defined by javascript "0 < 4"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter1/I increment counter1/I increment counter1""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter1/I increment counter1""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter1""")
    chain = builder.push(scenario1.steps(2))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter1/I increment counter1 while counter1 < 4""")
    chain = builder.push(scenario1.steps(2).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter1/I increment counter1 while counter1 < 4/counter1 < 4""")
    chain = builder.push(scenario1.steps(2).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter1/I increment counter1 while counter1 < 4/counter1 < 4/I increment counter1[1]""")
    chain = builder.push(scenario1.steps(2).stepDef.get.steps(0).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter1/I increment counter1 while counter1 < 4/counter1 < 4/I increment counter1[1]/I increment counter1""")
    chain = builder.push(scenario1.steps(2).stepDef.get.steps(0).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter1/I increment counter1 while counter1 < 4/counter1 < 4/I increment counter1[1]/I increment counter1/counter1 is defined by javascript "0 + 1"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter1/I increment counter1 while counter1 < 4/counter1 < 4/I increment counter1[1]/I increment counter1""")
    chain = builder.push(scenario1.steps(2).stepDef.get.steps(0).stepDef.get.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter1/I increment counter1 while counter1 < 4/counter1 < 4/I increment counter1[1]/I increment counter1/counter1 < 4 is defined by javascript "1 < 4"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter1/I increment counter1 while counter1 < 4/counter1 < 4/I increment counter1[1]/I increment counter1""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter1/I increment counter1 while counter1 < 4/counter1 < 4/I increment counter1[1]""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter1/I increment counter1 while counter1 < 4/counter1 < 4""")
    chain = builder.push(scenario1.steps(2).stepDef.get.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter1/I increment counter1 while counter1 < 4/counter1 < 4/I increment counter1[2]""")
    chain = builder.push(scenario1.steps(2).stepDef.get.steps(1).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter1/I increment counter1 while counter1 < 4/counter1 < 4/I increment counter1[2]/I increment counter1""")
    chain = builder.push(scenario1.steps(2).stepDef.get.steps(1).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter1/I increment counter1 while counter1 < 4/counter1 < 4/I increment counter1[2]/I increment counter1/counter1 is defined by javascript "1 + 1"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter1/I increment counter1 while counter1 < 4/counter1 < 4/I increment counter1[2]/I increment counter1""")
    chain = builder.push(scenario1.steps(2).stepDef.get.steps(1).stepDef.get.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter1/I increment counter1 while counter1 < 4/counter1 < 4/I increment counter1[2]/I increment counter1/counter1 < 4 is defined by javascript "2 < 4"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter1/I increment counter1 while counter1 < 4/counter1 < 4/I increment counter1[2]/I increment counter1""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter1/I increment counter1 while counter1 < 4/counter1 < 4/I increment counter1[2]""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter1/I increment counter1 while counter1 < 4/counter1 < 4""")
    chain = builder.push(scenario1.steps(2).stepDef.get.steps(2))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter1/I increment counter1 while counter1 < 4/counter1 < 4/I increment counter1[3]""")
    chain = builder.push(scenario1.steps(2).stepDef.get.steps(2).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter1/I increment counter1 while counter1 < 4/counter1 < 4/I increment counter1[3]/I increment counter1""")
    chain = builder.push(scenario1.steps(2).stepDef.get.steps(2).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter1/I increment counter1 while counter1 < 4/counter1 < 4/I increment counter1[3]/I increment counter1/counter1 is defined by javascript "2 + 1"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter1/I increment counter1 while counter1 < 4/counter1 < 4/I increment counter1[3]/I increment counter1""")
    chain = builder.push(scenario1.steps(2).stepDef.get.steps(2).stepDef.get.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter1/I increment counter1 while counter1 < 4/counter1 < 4/I increment counter1[3]/I increment counter1/counter1 < 4 is defined by javascript "3 < 4"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter1/I increment counter1 while counter1 < 4/counter1 < 4/I increment counter1[3]/I increment counter1""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter1/I increment counter1 while counter1 < 4/counter1 < 4/I increment counter1[3]""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter1/I increment counter1 while counter1 < 4/counter1 < 4""")
    chain = builder.push(scenario1.steps(2).stepDef.get.steps(3))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter1/I increment counter1 while counter1 < 4/counter1 < 4/I increment counter1[4]""")
    chain = builder.push(scenario1.steps(2).stepDef.get.steps(3).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter1/I increment counter1 while counter1 < 4/counter1 < 4/I increment counter1[4]/I increment counter1""")
    chain = builder.push(scenario1.steps(2).stepDef.get.steps(3).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter1/I increment counter1 while counter1 < 4/counter1 < 4/I increment counter1[4]/I increment counter1/counter1 is defined by javascript "3 + 1"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter1/I increment counter1 while counter1 < 4/counter1 < 4/I increment counter1[4]/I increment counter1""")
    chain = builder.push(scenario1.steps(2).stepDef.get.steps(3).stepDef.get.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter1/I increment counter1 while counter1 < 4/counter1 < 4/I increment counter1[4]/I increment counter1/counter1 < 4 is defined by javascript "4 < 4"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter1/I increment counter1 while counter1 < 4/counter1 < 4/I increment counter1[4]/I increment counter1""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter1/I increment counter1 while counter1 < 4/counter1 < 4/I increment counter1[4]""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter1/I increment counter1 while counter1 < 4/counter1 < 4""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter1/I increment counter1 while counter1 < 4""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter1""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example""")

    val scenario2 = spec.scenarios(3)
    chain = builder.push(scenario2)
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter2 with if condition""")
    chain = builder.push(scenario2.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter2 with if condition/counter2 is "-1"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter2 with if condition""")
    chain = builder.push(scenario2.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter2 with if condition/condition is defined by javascript "true"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter2 with if condition""")
    chain = builder.push(scenario2.steps(2))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter2 with if condition/I increment counter2""")
    chain = builder.push(scenario2.steps(2).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter2 with if condition/I increment counter2/I increment counter2""")
    chain = builder.push(scenario2.steps(2).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter2 with if condition/I increment counter2/I increment counter2/counter2 is defined by javascript "-1 + 1"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter2 with if condition/I increment counter2/I increment counter2""")
    chain = builder.push(scenario2.steps(2).stepDef.get.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter2 with if condition/I increment counter2/I increment counter2/counter2 < 4 is defined by javascript "0 < 4"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter2 with if condition/I increment counter2/I increment counter2""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter2 with if condition/I increment counter2""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter2 with if condition""")
    chain = builder.push(scenario2.steps(3))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter2 with if condition/I increment counter2 while counter2 < 4 if condition""")
    chain = builder.push(scenario2.steps(3).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter2 with if condition/I increment counter2 while counter2 < 4 if condition/condition""")
    chain = builder.push(scenario2.steps(3).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter2 with if condition/I increment counter2 while counter2 < 4 if condition/condition/I increment counter2 while counter2 < 4""")
    chain = builder.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter2 with if condition/I increment counter2 while counter2 < 4 if condition/condition/I increment counter2 while counter2 < 4/counter2 < 4""")
    chain = builder.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter2 with if condition/I increment counter2 while counter2 < 4 if condition/condition/I increment counter2 while counter2 < 4/counter2 < 4/I increment counter2[1]""")
    chain = builder.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(0).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter2 with if condition/I increment counter2 while counter2 < 4 if condition/condition/I increment counter2 while counter2 < 4/counter2 < 4/I increment counter2[1]/I increment counter2""")
    chain = builder.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(0).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter2 with if condition/I increment counter2 while counter2 < 4 if condition/condition/I increment counter2 while counter2 < 4/counter2 < 4/I increment counter2[1]/I increment counter2/counter2 is defined by javascript "0 + 1"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter2 with if condition/I increment counter2 while counter2 < 4 if condition/condition/I increment counter2 while counter2 < 4/counter2 < 4/I increment counter2[1]/I increment counter2""")
    chain = builder.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(0).stepDef.get.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter2 with if condition/I increment counter2 while counter2 < 4 if condition/condition/I increment counter2 while counter2 < 4/counter2 < 4/I increment counter2[1]/I increment counter2/counter2 < 4 is defined by javascript "1 < 4"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter2 with if condition/I increment counter2 while counter2 < 4 if condition/condition/I increment counter2 while counter2 < 4/counter2 < 4/I increment counter2[1]/I increment counter2""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter2 with if condition/I increment counter2 while counter2 < 4 if condition/condition/I increment counter2 while counter2 < 4/counter2 < 4/I increment counter2[1]""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter2 with if condition/I increment counter2 while counter2 < 4 if condition/condition/I increment counter2 while counter2 < 4/counter2 < 4""")
    chain = builder.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter2 with if condition/I increment counter2 while counter2 < 4 if condition/condition/I increment counter2 while counter2 < 4/counter2 < 4/I increment counter2[2]""")
    chain = builder.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(1).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter2 with if condition/I increment counter2 while counter2 < 4 if condition/condition/I increment counter2 while counter2 < 4/counter2 < 4/I increment counter2[2]/I increment counter2""")
    chain = builder.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(1).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter2 with if condition/I increment counter2 while counter2 < 4 if condition/condition/I increment counter2 while counter2 < 4/counter2 < 4/I increment counter2[2]/I increment counter2/counter2 is defined by javascript "1 + 1"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter2 with if condition/I increment counter2 while counter2 < 4 if condition/condition/I increment counter2 while counter2 < 4/counter2 < 4/I increment counter2[2]/I increment counter2""")
    chain = builder.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(1).stepDef.get.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter2 with if condition/I increment counter2 while counter2 < 4 if condition/condition/I increment counter2 while counter2 < 4/counter2 < 4/I increment counter2[2]/I increment counter2/counter2 < 4 is defined by javascript "2 < 4"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter2 with if condition/I increment counter2 while counter2 < 4 if condition/condition/I increment counter2 while counter2 < 4/counter2 < 4/I increment counter2[2]/I increment counter2""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter2 with if condition/I increment counter2 while counter2 < 4 if condition/condition/I increment counter2 while counter2 < 4/counter2 < 4/I increment counter2[2]""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter2 with if condition/I increment counter2 while counter2 < 4 if condition/condition/I increment counter2 while counter2 < 4/counter2 < 4""")
    chain = builder.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(2))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter2 with if condition/I increment counter2 while counter2 < 4 if condition/condition/I increment counter2 while counter2 < 4/counter2 < 4/I increment counter2[3]""")
    chain = builder.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(2).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter2 with if condition/I increment counter2 while counter2 < 4 if condition/condition/I increment counter2 while counter2 < 4/counter2 < 4/I increment counter2[3]/I increment counter2""")
    chain = builder.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(2).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter2 with if condition/I increment counter2 while counter2 < 4 if condition/condition/I increment counter2 while counter2 < 4/counter2 < 4/I increment counter2[3]/I increment counter2/counter2 is defined by javascript "2 + 1"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter2 with if condition/I increment counter2 while counter2 < 4 if condition/condition/I increment counter2 while counter2 < 4/counter2 < 4/I increment counter2[3]/I increment counter2""")
    chain = builder.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(2).stepDef.get.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter2 with if condition/I increment counter2 while counter2 < 4 if condition/condition/I increment counter2 while counter2 < 4/counter2 < 4/I increment counter2[3]/I increment counter2/counter2 < 4 is defined by javascript "3 < 4"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter2 with if condition/I increment counter2 while counter2 < 4 if condition/condition/I increment counter2 while counter2 < 4/counter2 < 4/I increment counter2[3]/I increment counter2""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter2 with if condition/I increment counter2 while counter2 < 4 if condition/condition/I increment counter2 while counter2 < 4/counter2 < 4/I increment counter2[3]""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter2 with if condition/I increment counter2 while counter2 < 4 if condition/condition/I increment counter2 while counter2 < 4/counter2 < 4""")
    chain = builder.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(3))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter2 with if condition/I increment counter2 while counter2 < 4 if condition/condition/I increment counter2 while counter2 < 4/counter2 < 4/I increment counter2[4]""")
    chain = builder.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(3).stepDef.get)
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter2 with if condition/I increment counter2 while counter2 < 4 if condition/condition/I increment counter2 while counter2 < 4/counter2 < 4/I increment counter2[4]/I increment counter2""")
    chain = builder.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(3).stepDef.get.steps(0))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter2 with if condition/I increment counter2 while counter2 < 4 if condition/condition/I increment counter2 while counter2 < 4/counter2 < 4/I increment counter2[4]/I increment counter2/counter2 is defined by javascript "3 + 1"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter2 with if condition/I increment counter2 while counter2 < 4 if condition/condition/I increment counter2 while counter2 < 4/counter2 < 4/I increment counter2[4]/I increment counter2""")
    chain = builder.push(scenario2.steps(3).stepDef.get.steps(0).stepDef.get.steps(3).stepDef.get.steps(1))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter2 with if condition/I increment counter2 while counter2 < 4 if condition/condition/I increment counter2 while counter2 < 4/counter2 < 4/I increment counter2[4]/I increment counter2/counter2 < 4 is defined by javascript "4 < 4"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter2 with if condition/I increment counter2 while counter2 < 4 if condition/condition/I increment counter2 while counter2 < 4/counter2 < 4/I increment counter2[4]/I increment counter2""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter2 with if condition/I increment counter2 while counter2 < 4 if condition/condition/I increment counter2 while counter2 < 4/counter2 < 4/I increment counter2[4]""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter2 with if condition/I increment counter2 while counter2 < 4 if condition/condition/I increment counter2 while counter2 < 4/counter2 < 4""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter2 with if condition/I increment counter2 while counter2 < 4 if condition/condition/I increment counter2 while counter2 < 4""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter2 with if condition/I increment counter2 while counter2 < 4 if condition/condition""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter2 with if condition/I increment counter2 while counter2 < 4 if condition""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter2 with if condition""")
    chain = builder.push(scenario2.steps(4))
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter2 with if condition/counter2 should be "4"""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example/Increment counter2 with if condition""")
    chain = builder.pop()._2
    chain.nodePath should be ("""/target/NodeChainTest/while.feature/Repeat while example""")
    chain = builder.pop()._2
    chain.nodePath should be ("/target/NodeChainTest/while.feature")
    chain = builder.pop()._2
    chain.nodePath should be ("/")
  }

}
