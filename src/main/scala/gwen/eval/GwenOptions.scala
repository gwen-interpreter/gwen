/*
 * Copyright 2014 Branko Juric, Brady Wood
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

import java.io.File
import gwen.dsl.Tag
import scopt.OptionParser
import gwen.Predefs.Kestrel
import scala.util.Try

/**
 * Captures gwen command line options.
 *
 * @param batch
 * 			true to run in batch mode, false for interactive REPL
 *    		(default is false)
 * @param parallel
 * 			true to run each given file/dir entry in parallel, false for serial
 *    		(default is false)      
 * @param reportDir
 * 			optional directory to generate evaluation report into
 * @param propertiesFile
 *          optional properties file to load into system properties
 * @param tags
 * 			list of tags to include and exclude (tag, True=include|False=exclude) 
 * @param metaFiles
 *          optional list of meta file overrides
 * @param paths
 * 			optional list of feature file and/or directory paths
 *    
 * @author Branko Juric
 */
case class GwenOptions(
    batch: Boolean = false,
    parallel: Boolean = false,
    reportDir: Option[File] = None,
    propertiesFile: Option[File] = None,
    tags: List[(Tag, Boolean)] = Nil,
    metaFiles: List[File] = Nil, 
    paths: List[File] = Nil) 
    
object GwenOptions {
  
  /**
   * Gets the command line options parser.
   * 
   * @param interpreterClassName
   * 				the fully qualified interpreter class name
   */
  private [eval] def parse(interpreterClassName: String, args: Array[String]): Try[GwenOptions] = {
    
    val parser = new OptionParser[GwenOptions]("scala " + interpreterClassName) {
    
      version("version") text("Prints the implementation version")
    
      help("help") text("Prints this usage text")

      opt[Unit]('b', "batch") action {
        (_, c) => c.copy(batch = true) 
      } text("Batch/server mode")
      
      opt[Unit]('|', "parallel") action {
        (_, c) => { 
          c.copy(parallel = true, batch = true)
        }
      } text("Parallel batch execution mode)")
    
      opt[File]('p', "properties") action {
        (f, c) => 
          c.copy(propertiesFile = Some(f))
      } validate {
        f => if (f.exists) success else failure(s"Specified properties file not found: $f")
      } valueName("<properties file>") text("<properties file> = User properties file")
    
      opt[File]('r', "report") action {
        (f, c) => c.copy(reportDir = Some(f)) 
      } valueName("<report directory>") text("<report directory> Evaluation report output directory")

      opt[String]('t', "tags") action {
        (ts, c) => 
          c.copy(tags = ts.split(",").toList.map(t => (Tag.string2Tag(t), t.toString.startsWith("@"))))
      } validate { ts =>
        ((ts.split(",") flatMap { t => 
          if (t.matches("""^(~?@\w+,?)+$""")) None 
          else Some(s"Invalid tag $t: tags must start with @ or ~@")
        }) collectFirst {
          case error => failure(error)
        }).getOrElse(success)
      } valueName("<include/exclude tags>") text("<include/exclude tags> = Comma separated list of tags to @include or ~@exclude")
    
      opt[String]('m', "meta") action {
        (ms, c) => 
          c.copy(metaFiles = ms.split(",").toList.map(new File(_)))
      } validate { ms => 
        ((ms.split(",") flatMap { m => 
          if (new File(m).exists()) None 
          else Some(s"Specified meta file not found: $m")
        }) collectFirst {
          case error => failure(error)
        }).getOrElse(success)
      } valueName("<meta file(s)>") text("<meta file(s)> = Comma separated list of meta files")
    
      arg[File]("<feature files and/or directories>") unbounded() optional() action { 
        (f, c) => 
          c.copy(paths = c.paths :+ f)
      } validate {
        f => if (f.exists) success else failure(s"Specified path not found: $f")
      } text("Space separated list of feature files and/or directories")
    
    }
  
    Try (
      (parser.parse(args, GwenOptions()) tap { (options: Option[GwenOptions]) =>
        options map { opt =>
          if (opt.batch && opt.paths.isEmpty) {
            sys.error("No feature files and/or directories specified")
          }
          opt.propertiesFile match {
            case (Some(propsFile)) =>
              sys.props += (("config.file", propsFile.getAbsolutePath()))
            case None => { }
          }
        }
      }).getOrElse(sys.error(""))
    )
  }
  
}