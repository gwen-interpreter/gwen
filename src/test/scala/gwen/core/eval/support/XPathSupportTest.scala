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

package gwen.core.eval.support

import gwen.core._
import gwen.core.Errors.XPathException

import org.scalatest.matchers.should.Matchers

import scala.util.chaining._

class XPathSupportTest extends BaseTest with Matchers with XPathSupport {

  val XmlSource =
    """<root><parent><name>P1</name><surname>O'Reilly</surname><children><child><name>C1</name></child><child><name>C2</name><middleName/><surname>CS2</surname></child></children></parent></root>"""

  "root node" should "return root node" in {
    compact(evaluateXPath("root",XmlSource, XMLNodeType.node)) should be (XmlSource)
  }

  "root/parent/children node" should "return children node" in {
    compact(evaluateXPath("root/parent/children",XmlSource, XMLNodeType.nodeset)) should be (
     """<children><child><name>C1</name></child><child><name>C2</name><middleName/><surname>CS2</surname></child></children>""")
  }

  "root/parent/children/child nodeset" should "return all child nodes" in {
    compact(evaluateXPath("root/parent/children/child",XmlSource, XMLNodeType.nodeset)) should be (
       """<child><name>C1</name></child><child><name>C2</name><middleName/><surname>CS2</surname></child>""")
  }

  "root/parent/children/child[2]/name text" should "return first child name" in {
    compact(evaluateXPath("root/parent/children/child/name",XmlSource, XMLNodeType.text)) should be ("C1")
  }

  "root/parent/children/child[1]/name text" should "return second child name" in {
    compact(evaluateXPath("root/parent/children/child[2]/name",XmlSource, XMLNodeType.text)) should be ("C2")
  }

  "match on surname with single quote" should "return surname node" in {
    compact(evaluateXPath("""root/parent/surname[text()="O'Reilly"]""",XmlSource, XMLNodeType.text)) should be ("O'Reilly")
  }

  "match on empty text node" should "return empty text" in {
    compact(evaluateXPath("""root/parent/child[1]/middleName""",XmlSource, XMLNodeType.text)) should be ("")
  }

  "match on non existent text node" should "return empty text" in {
    compact(evaluateXPath("""root/parent/child[2]/middleName""",XmlSource, XMLNodeType.text)) should be ("")
  }

  "match on non existent node" should "error" in {
    intercept[XPathException] {
      compact(evaluateXPath("""root/parent/middleName""",XmlSource, XMLNodeType.node))
    } tap { error =>
      error.getMessage should be ("No such node: root/parent/middleName")
    }
  }

  "match on non existent nodeset" should "error" in {
    intercept[XPathException] {
      compact(evaluateXPath("root/parent/ancestors",XmlSource, XMLNodeType.nodeset))
    } tap { error =>
      error.getMessage should be ("No such nodeset: root/parent/ancestors")
    }
  }

  private def compact(source: String): String = source.replace("\r", "").split('\n').map(_.trim()).mkString

}
