/*
 * Copyright 2014-2020 Branko Juric, Brady Wood
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

package gwen.eval

import gwen._
import gwen.dsl.StateLevel
import gwen.dsl.Tag
import gwen.report.ReportFormat

import scopt.OptionParser

import java.io.File

/**
  * Captures gwen command line options.
  *
  * @param batch true to run in batch mode, false for interactive REPL (default is false)
  * @param parallel true to run features or scenarios in parallel depending on state level (default is false)
  * @param parallelFeatures true to run features in parallel regardless of state level (default is false)
  * @param reportDir optional directory to generate evaluation report into
  * @param properties list of properties files to load as settings
  * @param tags list of tags to include and exclude (tag, True=include|False=exclude) 
  * @param dryRun true to not evaluate steps on engine (and validate for correctness only)
  * @param dataFile optional CSV file for data driven testing (must include column headers in 1st line)
  * @param metas optional list of meta file and/or directories
  * @param features optional list of feature file and/or directories
  *    
  * @author Branko Juric
  */
case class GwenOptions(
    batch: Boolean = false,
    parallel: Boolean = false,
    parallelFeatures: Boolean = false,
    reportDir: Option[File] = None,
    reportFormats: List[ReportFormat.Value] = Nil, 
    properties: List[File] = Nil,
    tags: List[(Tag, Boolean)] = Nil,
    dryRun: Boolean = false,
    dataFile: Option[File] = None,
    metas: List[File] = Nil, 
    features: List[File] = Nil,
    args: Option[Array[String]] = None) {
  
  val isParallelScenarios = StateLevel.isScenario && parallel && !parallelFeatures
  
  /**
    * Gets the command string used to invoke gwen.
    * 
    *  @param info the gwen implementation info
    */
  def commandString(info: GwenInfo): String = args match {
    case (Some(params)) => s"${info.implName}.${if(sys.props("os.name").startsWith("Windows")) "bat" else "sh"} ${params.mkString(" ")}"
    case _ => ""
  }
  
}
    
object GwenOptions {
  
  /**
    * Creates a new options object from the given command line arguments.
    *
    * @param args the command line arguments
    * @throws gwen.Errors.InvocationException if the given arguments fail to parse
    */
  def apply(args: Array[String]): GwenOptions = {
    
    val parser = new OptionParser[GwenOptions]("gwen") {
    
      version("version") text "Print Gwen version"
    
      help("help") text "Print CLI usage"
      
      opt[Unit]("parallel") action {
        (_, c) => { 
          c.copy(parallel = true, batch = true)
        }
      } text "Execute features/scenarios in parallel"

      opt[Unit]("parallel-features") action {
        (_, c) => { 
          c.copy(parallelFeatures = true, batch = true)
        }
      } text "Execute features only in parallel"

      opt[Unit]('b', "batch") action {
        (_, c) => c.copy(batch = true) 
      } text "Exit after execution completes (omit to open REPL)"

      opt[Unit]('n', "dry-run") action {
        (_, c) => c.copy(dryRun = true) 
      } text "Detect and report errors and possible runtime failures"
    
      opt[String]('p', "properties") action {
        (ps, c) => 
          c.copy(properties = ps.split(",").toList.map(new File(_)))
      } validate { ps => 
        ((ps.split(",") flatMap { p => 
          if (new File(p).exists()) None 
          else Some(s"Specified properties file not found: $p")
        }) collectFirst {
          case error => failure(error)
        }).getOrElse(success)
      } valueName "<files>" text "Properties/settings files to load (comma separated)"
    
      opt[File]('r', "report") action {
        (f, c) => c.copy(reportDir = Some(f)) 
      } valueName "<dir>" text "Directory to output evaluation report(s) to"
      
      opt[String]('f', "formats") action {
        (fs, c) => 
          c.copy(reportFormats = fs.split(",").toList.map(f => ReportFormat.withName(f)))
      } valueName "<formats>" text s"Report formats to produce (comma separated)\n                           - ${ReportFormat.values.filter(_ != ReportFormat.slideshow).mkString(",")} (default is ${ReportFormat.html})"
      
      opt[String]('t', "tags") action {
        (ts, c) => 
          c.copy(tags = ts.split(",").toList.map(t => (Tag(t), t.startsWith("@"))))
      } validate { ts =>
        ((ts.split(",") flatMap { t => 
          if (t.matches("""^(~?@\w+,?)+$""")) None 
          else Some(s"Invalid tag $t: tags must start with @ or ~@")
        }) collectFirst {
          case error => failure(error)
        }).getOrElse(success)
      } valueName "<@tag/~@tag>" text "Tags to @include/~@exclude for execution (comma separated)"
      
      opt[File]('i', "input-data") action {
        (d, c) => c.copy(dataFile = Some(d))
      } validate { d => 
        if (!d.exists) failure(s"Specified data file not found: $d")
        else success
      } valueName "<file>" text "Feed input data file (CSV with column headers)"
      
      opt[String]('m', "meta") action {
        (ms, c) => 
          c.copy(metas = ms.split(",").toList.map(new File(_)))
      } validate { ms => 
        ((ms.split(",") flatMap { m => 
          if (new File(m).exists()) None 
          else Some(s"Specified meta entry not found: $m")
        }) collectFirst {
          case error => failure(error)
        }).getOrElse(success)
      } valueName "<files/dirs>" text "Explicit meta files/directories to load (comma separated)"
    
      arg[File]("<files/dirs>").unbounded().optional().action { 
        (f, c) => 
          c.copy(features = c.features :+ f)
      } validate {
        f => if (f.exists) success else failure(s"Specified feature(s) not found: $f")
      } text "Feature files/directories to execute (space separated)"
    
    }

    (parser.parse(args, GwenOptions()).map { options =>
      new GwenOptions(
        options.batch,
        options.parallel,
        options.parallelFeatures,
        options.reportDir,
        if (options.reportFormats.nonEmpty) options.reportFormats else { if (options.reportDir.nonEmpty) List(ReportFormat.html) else Nil },
        options.properties,
        options.tags,
        options.dryRun,
        options.dataFile,
        FileIO.appendFile(options.metas, Settings.UserMeta),
        options.features,
        Some(args)) 
      } tap { options =>
        options foreach { opt =>
          if (opt.batch && opt.features.isEmpty) {
            Errors.invocationError("No feature files or directories specified")
          }
          val reportables = opt.reportFormats.filter(_ != ReportFormat.rp)
          if (reportables.nonEmpty && opt.reportDir.isEmpty) {
            Errors.invocationError(s"Required -r/--report option not specified for -f/--format option${if (reportables.size > 1) "s" else ""}: ${reportables.mkString(",")}")
          }
        }
      }).getOrElse(Errors.invocationError("Failed to read in gwen arguments"))
  }
  
}