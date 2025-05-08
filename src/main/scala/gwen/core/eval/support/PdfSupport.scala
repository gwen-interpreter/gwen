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

import gwen.core.LocationType
import gwen.core.node.gherkin.Step

import scala.jdk.CollectionConverters._

import java.io.File
import java.io.FileInputStream
import java.net.URL

import org.apache.pdfbox.Loader
import org.apache.pdfbox.io.RandomAccessReadBuffer
import org.apache.pdfbox.text.PDFTextStripper
import java.io.ByteArrayInputStream

/** Can be mixed into evaluation engines to provide PDF support. */
trait PdfSupport {

  def capturePDFText(sourceType: LocationType, source: String): String = {
    val pdf = new RandomAccessReadBuffer(
      sourceType match {
        case LocationType.file =>
          new FileInputStream(new File(source))
        case LocationType.url =>
          new URL(source).openStream()
        case LocationType.base64Blob => 
          new ByteArrayInputStream(source.getBytes)
      }
    )
    val doc = Loader.loadPDF(pdf)
    val stripper = new PDFTextStripper()
    stripper.getText(doc).trim()
  }
    
}