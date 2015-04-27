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

package gwen.report.html

import java.io.File
import scala.reflect.io.Path
import gwen.Predefs.FileIO
import gwen.Predefs.Kestrel
import gwen.report.ReportGenerator
import gwen.GwenInfo
import gwen.eval.GwenOptions

/**
  * Generates a HTML evaluation report. The report includes a feature
  * summary and all evaluation features details.
  * 
  * @author Branko Juric
  */
class HtmlReportGenerator(val options: GwenOptions) 
  extends ReportGenerator(options, "feature-summary", "html") 
  with HtmlReportFormatter {

  // copy in CSS files (if they don't already exist)
  new File(Path(new File(reportDir, "resources/css")).createDirectory().path) tap { dir =>
    copyClasspathTextResourceToFile("/gwen/report/html/css/gwen.css", dir)
    copyClasspathTextResourceToFile("/gwen/report/html/css/bootstrap.min.css", dir)
  }
  
  // copy in JS files (if they don't already exist)
  new File(Path(new File(reportDir, "resources/js")).createDirectory().path) tap { dir =>
    copyClasspathTextResourceToFile("/gwen/report/html/js/jquery-1.11.0.min.js", dir)
    copyClasspathTextResourceToFile("/gwen/report/html/js/bootstrap.min.js", dir)
  }
  
  // copy in image files (if they don't already exist)
  new File(Path(new File(reportDir, "resources/img")).createDirectory().path) tap { dir =>
    copyClasspathBinaryResourceToFile("/gwen/report/html/img/gwen-logo.png", dir)
  }
  
  // copy in index file
  copyClasspathBinaryResourceToFile("/gwen/report/html/index.html", reportDir)
  
}