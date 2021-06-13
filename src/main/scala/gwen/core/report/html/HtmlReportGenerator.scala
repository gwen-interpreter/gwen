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
import gwen.core.report.ReportGenerator
import gwen.core.report.html.format.HtmlReportFormatter

import scala.util.chaining._

import java.io.File

/**
  * Generates a HTML evaluation report. The report includes a feature
  * summary and all evaluation features details.
  *
  * @author Branko Juric
  */
class HtmlReportGenerator(options: GwenOptions) extends ReportGenerator(HtmlReportConfig, options) with HtmlReportFormatter {

  reportDir foreach { rdir =>

    // copy in CSS files (if they don't already exist)
    new File(rdir, "resources/css") tap { dir =>
      copyClasspathTextResourceToFile("/gwen/core/report/html/css/gwen.css", dir)
      copyClasspathTextResourceToFile("/gwen/core/report/html/css/bootstrap.min.css", dir)
    }

    // copy in JS files (if they don't already exist)
    new File(rdir, "resources/js") tap { dir =>
      copyClasspathTextResourceToFile("/gwen/core/report/html/js/jquery.min.js", dir)
      copyClasspathTextResourceToFile("/gwen/core/report/html/js/bootstrap.min.js", dir)
    }

    // copy in image files (if they don't already exist)
    new File(rdir, "resources/img") tap { dir =>
      copyClasspathBinaryResourceToFile("/gwen/core/report/html/img/gwen-logo.png", dir)
    }

    // copy in index file
    options.reportDir foreach { dir =>
      copyClasspathBinaryResourceToFile("/gwen/core/report/html/index.html", dir)
    }

  }

}
