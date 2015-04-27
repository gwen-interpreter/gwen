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

package gwen.eval

import java.io.File
import scala.util.Try
import gwen.GwenInfo
import gwen.Predefs.Kestrel
import gwen.dsl.Tag
import gwen.Settings
import gwen.UserOverrides
import scopt.OptionParser

/**
  * Captures gwen command line options.
  *
  * @param batch true to run in batch mode, false for interactive REPL
  *              (default is false)
  * @param parallel true to run each given file/dir entry in parallel, false for serial
  *                 (default is false)      
  * @param reportDir optional directory to generate evaluation report into
  * @param properties list of properties files to load into system properties
  * @param tags list of tags to include and exclude (tag, True=include|False=exclude) 
  * @param metaFiles optional list of meta file overrides
  * @param paths optional list of feature file and/or directory paths
  *    
  * @author Branko Juric
  */
case class GwenOptions(
    batch: Boolean = false,
    parallel: Boolean = false,
    reportDir: Option[File] = None,
    properties: List[File] = Nil,
    tags: List[(Tag, Boolean)] = Nil,
    metaFiles: List[File] = Nil, 
    paths: List[File] = Nil,
    args: Option[Array[String]] = None) {
  
  /**
    * Gets the command string used to invoke gwen.
    * 
    *  @param info the gwen implementation info
    */
  def commandString(info: GwenInfo) = args match {
    case (Some(args)) => s"${info.implName}.${if(sys.props("os.name").startsWith("Windows")) "bat" else "sh"} ${args.mkString(" ")}"
    case _ => ""
  }
  
}
    
object GwenOptions {
  
  /**
    * Creates a new options object from the given command line arguments.
    * 
    * @param interpreterClass the interpreter implementation class
    * @param args the command line arguments
    */
  def apply(interpreterClass: Class[_], args: Array[String]): GwenOptions = { 
    
    val parser = new OptionParser[GwenOptions]("scala " + interpreterClass.getName) {
    
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
      } valueName("<properties files>") text("Comma separated list of properties file paths")
    
      opt[File]('r', "report") action {
        (f, c) => c.copy(reportDir = Some(f)) 
      } valueName("<report directory>") text("Evaluation report output directory")

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
      } valueName("<tags>") text("Comma separated list of @include or ~@exclude tags")
    
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
      } valueName("<meta files>") text("Comma separated list of meta file paths")
    
      arg[File]("<feature paths>") unbounded() optional() action { 
        (f, c) => 
          c.copy(paths = c.paths :+ f)
      } validate {
        f => if (f.exists) success else failure(s"Specified path not found: $f")
      } text("Space separated list of feature file and/or directory paths")
    
    }
  
    (parser.parse(args, GwenOptions()).map { options => 
      new GwenOptions(
        options.batch,
        options.parallel,
        options.reportDir,
        UserOverrides.addUserProperties(options.properties),
        options.tags,
        UserOverrides.addUserMeta(options.metaFiles),
        options.paths,
        Some(args)) 
      } tap { options =>
        options foreach { opt =>
          if (opt.batch && opt.paths.isEmpty) {
            sys.error("No feature files and/or directories specified")
          }
          Settings.loadAll(opt.properties)
        }
      }).getOrElse(sys.error("Failed to parse gwen arguments"))
  }
  
}