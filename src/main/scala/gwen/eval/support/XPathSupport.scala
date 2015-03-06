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

package gwen.eval.support

import java.io.StringReader
import java.io.StringWriter
import java.util.Iterator

import org.w3c.dom.Node
import org.w3c.dom.NodeList
import org.xml.sax.InputSource

import gwen.Predefs.Kestrel
import gwen.Predefs.RegexContext
import javax.xml.namespace.NamespaceContext
import javax.xml.transform.OutputKeys
import javax.xml.transform.TransformerFactory
import javax.xml.transform.dom.DOMSource
import javax.xml.transform.stream.StreamResult
import javax.xml.xpath.XPath
import javax.xml.xpath.XPathConstants
import javax.xml.xpath.XPathFactory

/**
 * Can be mixed into evaluation engines to provide XPath support.
 */
trait XPathSupport {

  object XMLNodeType extends Enumeration {
    val text, node, nodeset = Value
  }
  
  /**
   * Evaluates an xpath expression against an xml source string and returns 
   * the result.
   * 
   * @param xpath the expath expression
   * @param source the xml source string
   * @param targetType the target node type
   * @return the result of evaluationg the xpath expression
   */
  def evaluateXPath(xpath: String, source: String, targetType: XMLNodeType.Value): String = 
    withXPath(xpath) { (xPath, expr) =>
      val qname = targetType match {
        case XMLNodeType.text => XPathConstants.STRING
        case XMLNodeType.node => XPathConstants.NODE
        case XMLNodeType.nodeset => XPathConstants.NODESET
        case _ => sys.error(s"Unsupported target XPath output type: $targetType (valid values are text|node|nodeset)")
      }
      val result = xPath.compile(expr).evaluate(new InputSource(new StringReader(source)), qname)
      targetType match {
        case XMLNodeType.text => result.toString
        case XMLNodeType.node => nodeToString(result.asInstanceOf[Node])
        case XMLNodeType.nodeset => nodeListToString(result.asInstanceOf[NodeList])
      }
    }

  private def withXPath[T](expression: String)(f: (XPath, String) => T): T = expression match {
    case r"""(.+?)$expr where (.+?)$$$namespaces""" =>
      val xPath = XPathFactory.newInstance().newXPath() tap { xPath =>
        xPath.setNamespaceContext(new NamespaceContext() { 
          def getNamespaceURI(prefix: String): String = {
            val mappings = namespaces.split(",").map(_.split("=")).map(pair => (pair(0).trim, pair(1).trim)).toMap
            return mappings.getOrElse(prefix, sys.error(s"Unknown namespace prefix: $prefix"));
          }
          def getPrefix(uri: String): String = null
          def getPrefixes(uri: String): Iterator[String] = null
        })
      }
      f(xPath, expr)
    case _ =>
      f(XPathFactory.newInstance().newXPath(), expression)  
  }
  
  private def nodeToString(node: Node): String = {
    val sw = new StringWriter()
    val t = TransformerFactory.newInstance().newTransformer()
    t.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes")
    t.setOutputProperty(OutputKeys.INDENT, "yes")
    t.transform(new DOMSource(node), new StreamResult(sw))
    sw.toString
  }
  
  private def nodeListToString(nodeList: NodeList): String = {
    val result = for(i <- 0 to nodeList.getLength) yield nodeToString(nodeList.item(i))
    result.mkString(sys.props("line.separator"))
  }
  
}