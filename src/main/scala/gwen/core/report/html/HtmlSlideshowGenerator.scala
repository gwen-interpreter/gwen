/*
 * Copyright 2014-2021 Branko Juric, Brady Wood
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

package gwen.core.report.html

import gwen.core._
import gwen.core.node.event.NodeEventDispatcher
import gwen.core.report.ReportGenerator
import gwen.core.report.html.format.SlideshowFormatter

import scala.util.chaining._

import java.io.File

/**
  * Generates a slideshow HTML file
  *
  * @author Branko Juric
  */
class HtmlSlideshowGenerator(options: GwenOptions, info: GwenInfo) extends ReportGenerator(HtmlSlideshowConfig, options, info) with SlideshowFormatter {

  override def init(lifecycle: NodeEventDispatcher): Unit = {

    super.init(lifecycle)
    
    reportDir foreach { rdir =>

      // copy in JS files (if they don't already exist)
      new File(rdir, "resources/js") tap { dir =>
        FileIO.copyClasspathTextResourceToFile("/gwen/core/report/html/js/jquery.reel-min.js", dir)
      }

      // copy in font files (if they don't already exist)
      new File(rdir, "resources/fonts") tap { dir =>
        FileIO.copyClasspathBinaryResourceToFile("/gwen/core/report/html/fonts/glyphicons-halflings-regular.eot", dir)
        FileIO.copyClasspathBinaryResourceToFile("/gwen/core/report/html/fonts/glyphicons-halflings-regular.svg", dir)
        FileIO.copyClasspathBinaryResourceToFile("/gwen/core/report/html/fonts/glyphicons-halflings-regular.ttf", dir)
        FileIO.copyClasspathBinaryResourceToFile("/gwen/core/report/html/fonts/glyphicons-halflings-regular.woff", dir)
        FileIO.copyClasspathBinaryResourceToFile("/gwen/core/report/html/fonts/glyphicons-halflings-regular.woff2", dir)
      }

    }
  }

}
