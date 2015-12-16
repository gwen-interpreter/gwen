/*
 * Copyright 2014-2015 Branko Juric, Brady Wood
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

package gwen.report

import java.io.File
import gwen.Predefs.Kestrel
import gwen.eval.GwenOptions

/**
  * Generates a slideshow HTML file
  * 
  * @author Branko Juric
  */
class HtmlSlideshowGenerator(val options: GwenOptions) extends ReportGenerator(ReportFormat.slideshow, options) with HtmlSlideshowFormatter {

  // copy in JS files (if they don't already exist)
  new File(reportDir, "resources/js") tap { dir =>
    copyClasspathTextResourceToFile("/gwen/report/html/js/jquery.reel-min.js", dir)
  }
  
  // copy in font files (if they don't already exist)
  new File(reportDir, "resources/fonts") tap { dir =>
    copyClasspathBinaryResourceToFile("/gwen/report/html/fonts/glyphicons-halflings-regular.eot", dir)
    copyClasspathBinaryResourceToFile("/gwen/report/html/fonts/glyphicons-halflings-regular.svg", dir)
    copyClasspathBinaryResourceToFile("/gwen/report/html/fonts/glyphicons-halflings-regular.ttf", dir)
    copyClasspathBinaryResourceToFile("/gwen/report/html/fonts/glyphicons-halflings-regular.woff", dir)
    copyClasspathBinaryResourceToFile("/gwen/report/html/fonts/glyphicons-halflings-regular.woff2", dir)
  }
  
}