/*
 * Copyright 2014-2024 Branko Juric, Brady Wood
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

package gwen.core.eval

import gwen.core._
import gwen.core.eval.FileComparisonOperator
import gwen.core.eval.binding._
import gwen.core.eval.engine.UnitEngine
import gwen.core.eval.action._
import gwen.core.eval.action.composite._
import gwen.core.eval.action.unit._
import gwen.core.eval.support.XMLNodeType
import gwen.core.node.event.NodeEventDispatcher
import gwen.core.node.gherkin._
import gwen.core.state.EnvState
import gwen.core.status._

import scala.concurrent.duration._

object EvalEngine {
  def apply(): EvalEngine[EvalContext] = new EvalEngine[EvalContext]() {
    override def init(options: GwenOptions, envState: EnvState): EvalContext = {
      new EvalContext(options, envState)
    }
  }
}

/**
  * Base evaluation engine with default DSL implementation.
  *
  * @author Branko Juric
  */
abstract class EvalEngine[T <: EvalContext] extends NodeEventDispatcher with UnitEngine[T] with StepTranslator[T] with GwenInfo with ImplicitValueKeys {

  /**
    * Initialises the engine and returns a new evaluation context.
    * 
    * @param options command line options
    * @param envState the initial environment state
    */
  def init(options: GwenOptions, envState: EnvState): T

  /**
    * Translates a composite DSL step into an engine operation.
    *
    * @param step the step to translate
    * @return an optional function that performs the composite step operation and returns it in evaluated form
    */
  override def translateCompositeStep(step: Step): Option[CompositeStepAction[T]] = {
    step.expression match {
      case r"""(.+?)$doStep for each data record""" =>
        Some(new ForEachTableRecord(doStep, this))
      case r"""(.+?)$doStep for each (.+?)$entry in (.+?)$source delimited by "(.+?)"$delimiter""" =>
        Some(new ForEachDelimited(doStep, entry, source, delimiter, this))
      case r"""(.+?)$doStep for each (.+?)$entry in (.+?)$source array""" =>
        Some(new ForEachJsonArrayElement(doStep, entry, source, this))
      case r"""(.+?)$doStep for each (.+?)$entry in (?:json|JSON) array (.+?)$source""" =>
        Some(new ForEachJsonArrayElement(doStep, entry, source, this))
      case r"""(.+)$doStep if (.+?)$attribute is( not)?$negation defined""" =>
        Some(new IfDefinedCondition(doStep, attribute, Option(negation).nonEmpty, this))
      case r"""(.+)$doStep if "(.+?)$filepath" file( not)?$negation exists""" =>
        Some(new IfFileCondition(doStep, Some(filepath), None, FileComparisonOperator.exists, Option(negation).nonEmpty, this))
      case r"""(.+)$doStep if "(.+?)$filepath" file does not exist""" =>
        Some(new IfFileCondition(doStep, Some(filepath), None, FileComparisonOperator.exists, true, this))
      case r"""(.+)$doStep if "(.+?)$filepath" file is( not)?$negation empty""" =>
        Some(new IfFileCondition(doStep, Some(filepath), None, FileComparisonOperator.empty, Option(negation).nonEmpty, this))
      case r"""(.+)$doStep if (.+? file)$filepathRef( not)?$negation exists""" =>
        Some(new IfFileCondition(doStep, None, Some(filepathRef), FileComparisonOperator.exists, Option(negation).nonEmpty, this))
      case r"""(.+)$doStep if (.+? file)$filepathRef does not exist""" =>
        Some(new IfFileCondition(doStep, None, Some(filepathRef), FileComparisonOperator.exists, true, this))
      case r"""(.+)$doStep if (.+? file)$filepathRef is( not)?$negation empty""" =>
        Some(new IfFileCondition(doStep, None, Some(filepathRef), FileComparisonOperator.empty, Option(negation).nonEmpty, this))
      case r"""(.+)$doStep if (.+?)$attribute is( not)?$negation (?:blank|empty)""" =>
        Some(new IfCompareCondition(doStep, attribute, ComparisonOperator.be, Option(negation).nonEmpty, "", step.isTrim, step.isIgnoreCase, this))
      case r"""(.+)$doStep if (.+?)$attribute is( not)?$negation "(.*?)"$expression""" =>
        Some(new IfCompareCondition(doStep, attribute, ComparisonOperator.be, Option(negation).nonEmpty, expression, step.isTrim, step.isIgnoreCase, this))
      case r"""(.+)$doStep if (.+?)$attribute (contains|starts with|ends with|matches regex|matches xpath|matches json path|matches template|matches template file)$operator "(.*?)"$expression""" =>
        Some(new IfCompareCondition(doStep, attribute, ComparisonOperator.fromModal(operator), false, expression, step.isTrim, step.isIgnoreCase, this))
      case r"""(.+)$doStep if (.+?)$attribute does not (contain|start with|end with|match regex|match xpath|match json path|match template|match template file)$operator "(.*?)"$expression""" =>
        Some(new IfCompareCondition(doStep, attribute, ComparisonOperator.valueOf(operator), true, expression, step.isTrim, step.isIgnoreCase, this))
      case r"""(.+)$doStep if(?:(?!\bif\b))( not)?$negation (.+)$condition""" if !condition.contains('"') =>
        Some(new IfCondition(doStep, condition, Option(negation).nonEmpty, defaultConditionTimeoutSecs, this))
      case r"""(.+?)$doStep (until|while)$operation (.+?)$attribute is( not)?$negation defined""" if (doStep != "I wait" && !step.expression.matches(""".*".*(until|while).*".*""")) =>
        Some(new RepeatIfDefined(doStep, operation, attribute, Option(negation).nonEmpty, step.delayOpt.getOrElse(defaultRepeatDelay), step.timeoutOpt.getOrElse(defaultRepeatTimeout(defaultRepeatDelay)), defaultConditionTimeoutSecs, this))
      case r"""(.+?)$doStep (until|while)$operation "(.+?)$filepath" file( not)?$negation exists""" if (doStep != "I wait") =>
        Some(new RepeatIfFile(doStep, operation, s""""$filepath" file ${if(Option(negation).nonEmpty) "not " else ""}exists""", Some(filepath), None, FileComparisonOperator.exists, Option(negation).nonEmpty, step.delayOpt.getOrElse(defaultRepeatDelay), step.timeoutOpt.getOrElse(defaultRepeatTimeout(defaultRepeatDelay)), this))
      case r"""(.+?)$doStep (until|while)$operation "(.+?)$filepath" file does not exist""" if (doStep != "I wait") =>
        Some(new RepeatIfFile(doStep, operation, s""""$filepath" file does not exist""", Some(filepath), None, FileComparisonOperator.exists, true, step.delayOpt.getOrElse(defaultRepeatDelay), step.timeoutOpt.getOrElse(defaultRepeatTimeout(defaultRepeatDelay)), this))
      case r"""(.+?)$doStep (until|while)$operation "(.+?)$filepath" file is( not)?$negation empty""" if (doStep != "I wait") =>
        Some(new RepeatIfFile(doStep, operation, s""""$filepath" file is ${if(Option(negation).nonEmpty) "not " else ""}empty""", Some(filepath), None, FileComparisonOperator.empty, Option(negation).nonEmpty, step.delayOpt.getOrElse(defaultRepeatDelay), step.timeoutOpt.getOrElse(defaultRepeatTimeout(defaultRepeatDelay)), this))
      case r"""(.+?)$doStep (until|while)$operation (.+? file)$filepathRef( not)?$negation exists""" if (doStep != "I wait") =>
        Some(new RepeatIfFile(doStep, operation, s"$filepathRef file ${if(Option(negation).nonEmpty) "not " else ""}exists", None, Some(filepathRef), FileComparisonOperator.exists, Option(negation).nonEmpty, step.delayOpt.getOrElse(defaultRepeatDelay), step.timeoutOpt.getOrElse(defaultRepeatTimeout(defaultRepeatDelay)), this))
      case r"""(.+?)$doStep (until|while)$operation (.+? file)$filepathRef does not exist""" if (doStep != "I wait") =>
        Some(new RepeatIfFile(doStep, operation, s"$filepathRef file does not exist", None, Some(filepathRef), FileComparisonOperator.exists, true, step.delayOpt.getOrElse(defaultRepeatDelay), step.timeoutOpt.getOrElse(defaultRepeatTimeout(defaultRepeatDelay)), this))
      case r"""(.+?)$doStep (until|while)$operation (.+? file)$filepathRef is( not)?$negation empty""" if (doStep != "I wait") =>
        Some(new RepeatIfFile(doStep, operation, s"$filepathRef ${if(Option(negation).nonEmpty) "not " else ""}empty", None, Some(filepathRef), FileComparisonOperator.empty, Option(negation).nonEmpty, step.delayOpt.getOrElse(defaultRepeatDelay), step.timeoutOpt.getOrElse(defaultRepeatTimeout(defaultRepeatDelay)), this))
      case r"""(.+?)$doStep (until|while)$operation (.+?)$attribute is( not)?$negation (?:blank|empty)""" if (doStep != "I wait")   =>
        Some(new RepeatIfCompareCondition(doStep, operation, attribute, ComparisonOperator.be, Option(negation).nonEmpty, "", step.isTrim, step.isIgnoreCase, step.delayOpt.getOrElse(defaultRepeatDelay), step.timeoutOpt.getOrElse(defaultRepeatTimeout(defaultRepeatDelay)), defaultConditionTimeoutSecs, this))
      case r"""(.+?)$doStep (until|while)$operation (.+?)$attribute is( not)?$negation "(.*?)"$expression""" if (doStep != "I wait")  =>
        Some(new RepeatIfCompareCondition(doStep, operation, attribute, ComparisonOperator.be, Option(negation).nonEmpty, expression, step.isTrim, step.isIgnoreCase, step.delayOpt.getOrElse(defaultRepeatDelay), step.timeoutOpt.getOrElse(defaultRepeatTimeout(defaultRepeatDelay)), defaultConditionTimeoutSecs, this))
      case r"""(.+?)$doStep (until|while)$operation (.+?)$attribute (contains|starts with|ends with|matches regex|matches xpath|matches json path|matches template|matches template file)$operator "(.*?)"$expression""" if (doStep != "I wait")  =>
        Some(new RepeatIfCompareCondition(doStep, operation, attribute, ComparisonOperator.fromModal(operator), false, expression, step.isTrim, step.isIgnoreCase, step.delayOpt.getOrElse(defaultRepeatDelay), step.timeoutOpt.getOrElse(defaultRepeatTimeout(defaultRepeatDelay)), defaultConditionTimeoutSecs, this))
      case r"""(.+?)$doStep (until|while)$operation (.+?)$attribute does not (contain|start with|end with|match regex|match xpath|match json path|match template|match template file)$operator "(.*?)"$expression""" if (doStep != "I wait")  =>
        Some(new RepeatIfCompareCondition(doStep, operation, attribute, ComparisonOperator.valueOf(operator), true, expression, step.isTrim, step.isIgnoreCase, step.delayOpt.getOrElse(defaultRepeatDelay), step.timeoutOpt.getOrElse(defaultRepeatTimeout(defaultRepeatDelay)), defaultConditionTimeoutSecs, this))
      case r"""(.+?)$doStep (until|while)$operation (.+?)$condition""" if (doStep != "I wait" && !condition.contains('"') && !step.expression.matches(""".*".*(until|while).*".*""")) =>
        Some(new RepeatJS(doStep, operation, condition, step.delayOpt.getOrElse(defaultRepeatDelay), step.timeoutOpt.getOrElse(defaultRepeatTimeout(defaultRepeatDelay)), defaultConditionTimeoutSecs, this))
      case _ =>
        None
    }
  }

  /**
    * Translates a DSL step into an engine operation.
    *
    * @param step the step to translate
    * @return a step operation that throws an exception on failure
    */
  override def translateStep(step: Step): UnitStepAction[T] = {
    step.expression match {
      case r"""my (.+?)$name (?:property|setting) is "(.*?)"$value""" =>
        new SetProperty(name, value)
      case r"""I reset my (.+?)$name (?:property|setting)""" =>
        new ClearProperty(name)
      case r"""I wait (\d+)$duration second(?:s?)""" =>
        new Sleep(duration.toInt)
      case r"""I wait until "(.+?)$filepath" file( not)?$negation exists""" =>
        new WaitForFileCondition(Some(filepath), None, FileComparisonOperator.exists, Option(negation).nonEmpty, step.timeoutOpt.map(_.toSeconds).getOrElse(60))
      case r"""I wait until "(.+?)$filepath" file does not exist""" =>
        new WaitForFileCondition(Some(filepath), None, FileComparisonOperator.exists, true, step.timeoutOpt.map(_.toSeconds).getOrElse(60))
      case r"""I wait until "(.+?)$filepath" file is( not)?$negation empty""" =>
        new WaitForFileCondition(Some(filepath), None, FileComparisonOperator.empty, Option(negation).nonEmpty, step.timeoutOpt.map(_.toSeconds).getOrElse(60))
      case r"""I wait until (.+? file)$filepathRef( not)?$negation exists""" =>
        new WaitForFileCondition(None, Some(filepathRef), FileComparisonOperator.exists, Option(negation).nonEmpty, step.timeoutOpt.map(_.toSeconds).getOrElse(60))
      case r"""I wait until (.+? file)$filepathRef does not exist""" =>
        new WaitForFileCondition(None, Some(filepathRef), FileComparisonOperator.exists, true, step.timeoutOpt.map(_.toSeconds).getOrElse(60))
      case r"""I wait until (.+? file)$filepathRef is( not)?$negation empty""" =>
        new WaitForFileCondition(None, Some(filepathRef), FileComparisonOperator.empty, Option(negation).nonEmpty, step.timeoutOpt.map(_.toSeconds).getOrElse(60))
      case r"""I execute system process "(.+?)"$systemproc delimited by "(.+?)"$delimiter""" =>
        new ExecuteSysProc(systemproc, Some(delimiter))
      case r"""I execute system process "(.+?)"$systemproc""" =>
        new ExecuteSysProc(step.orDocString(systemproc), None)
      case r"""I execute (?:a )?unix system process "(.+?)"$systemproc""" =>
        new ExecuteSysProcUnix(step.orDocString(systemproc), None)
      case r"""I execute (?:javascript|js) "(.+?)$javascript"""" =>
        new ExecuteJS(step.orDocString(javascript))
      case r"""I capture (.+?)$attribute by (?:javascript|js) "(.+?)"$expression""" =>
        new CaptureByJS(attribute, step.orDocString(expression))
      case r"""I capture the (text|node|nodeset)$targetType in (.+?)$source by xpath "(.+?)"$expression as (.+?)$name""" =>
        new CaptureByXPath(name, expression, source, XMLNodeType.valueOf(targetType))
      case r"""I capture the text in (.+?)$source by regex "(.+?)"$expression as (.+?)$name""" =>
        new CaptureByRegex(name, expression, source)
      case r"""I capture the content in (.+?)$source by json path "(.+?)"$expression as (.+?)$name""" =>
        new CaptureByJsonPath(name, expression, source)
      case r"""I capture the similarity score of (.+?)$attribute1 compared to "(.+?)"$value2 as (.+?)$name""" =>
        new CaptureSimilarity(name, attribute1, None, Some(value2), step.isTrim, step.isIgnoreCase)
      case r"""I capture the similarity score of (.+?)$attribute1 compared to (.+?)$attribute2 as (.+?)$name""" =>
        new CaptureSimilarity(name, attribute1, Some(attribute2), None, step.isTrim, step.isIgnoreCase)
      case r"""I capture the similarity score of (.+?)$attribute1 compared to "(.+?)"$value2""" =>
        new CaptureSimilarity("similarity score", attribute1, None, Some(value2), step.isTrim, step.isIgnoreCase)
      case r"""I capture the similarity score of (.+?)$attribute1 compared to (.+?)$attribute2""" =>
        new CaptureSimilarity("similarity score", attribute1, Some(attribute2), None, step.isTrim, step.isIgnoreCase)
      case r"""I capture the PDF text from (url|file)$locationType "(.+?)"$location as (.+?)$name""" =>
        new CapturePDF(name, LocationType.valueOf(locationType), location, defaultConditionTimeoutSecs)
      case r"""I capture the PDF text from (url|file)$locationType "(.+?)"$location""" =>
        new CapturePDF("the PDF text", LocationType.valueOf(locationType), location, defaultConditionTimeoutSecs)
      case r"""I capture the base64 encoded PDF text from (.+?)$attribute""" =>
        new CapturePDF("the PDF text", LocationType.base64Blob, attribute, defaultConditionTimeoutSecs)
      case r"""I capture (.+?)$source as (.+?)$attribute""" =>
        new Capture(attribute, source)
      case r"""I capture (.+?)$attribute""" =>
        new Capture(attribute, attribute)
      case r"""I base64 decode (.+?)$attribute as (.+?)$name""" =>
        new CaptureBase64Decoded(name, attribute)
      case r"""I base64 decode (.+?)$attribute""" =>
        new CaptureBase64Decoded(attribute, attribute)
      case r"""(.+?)$attribute is defined by system process "(.+?)"$expression delimited by "(.+?)"$delimiter""" =>
        new BindAsType(attribute, BindingType.sysproc, expression, None, Some(delimiter), step.isMasked)
      case r"""(.+?)$attribute is defined by (javascript|js|system process|unix system process|property|setting)$attrType "(.+?)"$expression""" =>
        new BindAsType(attribute, BindingType.parse(attrType), step.orDocString(expression), None, None, step.isMasked)
      case r"""(.+?)$attribute is defined by file "(.+?)"$filepath""" =>
        new BindAsType(attribute, BindingType.file, step.orDocString(filepath), None, None, step.isMasked)
      case r"""(.+?)$attribute is defined by (.+?)$enc file "(.+?)"$filepath""" =>
        new BindAsType(attribute, BindingType.file, step.orDocString(filepath), Some(enc), None, step.isMasked)
      case r"""(.+?)$attribute is defined by (.+?)$function applied to "(.+?)"$args delimited by "(.*)"$delimiter""" =>
        new BindAsType(attribute, BindingType.function, function, Some(args), Some(delimiter), step.isMasked)
      case r"""(.+?)$attribute is defined by (.+?)$function applied to "(.*)"$arg""" =>
        new BindAsType(attribute, BindingType.function, function, Some(step.orDocString(arg)), None, step.isMasked)
      case r"""(.+?)$attribute is defined by the (text|node|nodeset)$targetType in (.+?)$source by xpath "(.+?)"$expression""" =>
        new BindAsXPath(attribute, step.orDocString(expression), targetType, source, step.isMasked)
      case r"""(.+?)$attribute is defined in (.+?)$source by regex "(.+?)"$expression""" =>
        new BindAsRegex(attribute, step.orDocString(expression), source, step.isMasked)
      case r"""(.+?)$attribute is defined in (.+?)$source by json path "(.+?)"$expression""" =>
        new BindAsJsonPath(attribute, step.orDocString(expression), source, step.isMasked)
      case r"""(.+?)$attribute is defined by sql "(.+?)"$selectStmt in the (.+?)$dbName database""" =>
        new BindAsSQL(attribute, dbName, selectStmt, step.isMasked)
      case r"""(.+?)$attribute is defined in the (.+?)$dbName database by sql "(.+?)"$selectStmt""" =>
        new BindAsSQL(attribute, dbName, step.orDocString(selectStmt), step.isMasked)
      case r"""(.+?)$attribute is "(.*?)"$value""" =>
        new BindAttribute(attribute, step.orDocString(value))
      case r"""(.+?)$attribute is (blank|empty|true|false)$literal""" =>
        new BindAttribute(attribute, ValueLiteral.valueOf(literal).value)
      case r"""I update the (.+?)$dbName database by sql "(.+?)"$updateStmt""" =>
        new UpdateBySQL(dbName, step.orDocString(updateStmt))
      case r"""(.+?)$source at (json path|xpath)$matcher "(.+?)"$path should( not)?$negation be (blank|empty|true|false)$literal""" =>
        new CompareByPath(source, BindingType.valueOf(matcher), path, ValueLiteral.valueOf(literal).value, ComparisonOperator.be, Option(negation).nonEmpty, step.message, step.isTrim, step.isIgnoreCase)
      case r"""(.+?)$source at (json path|xpath)$matcher "(.+?)"$path should( not)?$negation (be|contain|start with|end with|match regex|match template|match template file)$operator "(.*?)"$expression""" =>
        new CompareByPath(source, BindingType.valueOf(matcher), path, step.orDocString(expression), ComparisonOperator.valueOf(operator), Option(negation).nonEmpty, step.message, step.isTrim, step.isIgnoreCase)
      case r"""(.+?)$attribute should( not)?$negation (be|be less than|be at most|be more than|be at least)$operator (\d+(?:\.\d*)?)$percentage% similar to "(.+?)"$value2""" =>
        new CheckSimilarity(attribute, None, Some(value2), SimilarityOperator.valueOf(operator), percentage.toDouble, Option(negation).nonEmpty, step.message, step.isTrim, step.isIgnoreCase)
      case r"""(.+?)$attribute1 should( not)?$negation (be|be less than|be at most|be more than|be at least)$operator (\d+(?:\.\d*)?)$percentage% similar to (.+?)$attribute2""" =>
        new CheckSimilarity(attribute1, Some(attribute2), None, SimilarityOperator.valueOf(operator), percentage.toDouble, Option(negation).nonEmpty, step.message, step.isTrim, step.isIgnoreCase)
      case r"""(.+?)$attribute should be absent""" =>
        new IsDefined(attribute, true, step.message)
      case r"""(.+?)$attribute should( not)?$negation be defined""" =>
        new IsDefined(attribute, Option(negation).nonEmpty, step.message)
      case r""""(.+?)"$filepath file should( not)?$negation exist""" =>
        new CompareFile(Some(filepath), None, FileComparisonOperator.exists, Option(negation).nonEmpty, step.message)
      case r"""(.+? file)$filepathRef should( not)?$negation exist""" =>
        new CompareFile(None, Some(filepathRef), FileComparisonOperator.exists, Option(negation).nonEmpty, step.message)
      case r""""(.+?)"$filepath file should( not)?$negation be empty""" =>
        new CompareFile(Some(filepath), None, FileComparisonOperator.empty, Option(negation).nonEmpty, step.message)
      case r"""(.+? file)$filepathRef should( not)?$negation be empty""" =>
        new CompareFile(None, Some(filepathRef), FileComparisonOperator.empty, Option(negation).nonEmpty, step.message)
      case r"""(.+?)$attribute should( not)?$negation be (blank|empty|true|false)$literal""" =>
        new Compare(attribute, ValueLiteral.valueOf(literal).value, ComparisonOperator.be, Option(negation).nonEmpty, step.message, step.isTrim, step.isIgnoreCase)
      case r"""(.+?)$attribute should( not)?$negation (be|contain|start with|end with|match regex|match xpath|match json path|match template|match template file)$operator "(.*?)"$expression""" =>
        new Compare(attribute, step.orDocString(expression), ComparisonOperator.valueOf(operator), Option(negation).nonEmpty, step.message, step.isTrim, step.isIgnoreCase)
      case r"""I attach "(.+?)"$filepath as "(.+?)"$name""" =>
        new AttachFile(Some(name), filepath)
      case r"""I attach "(.+?)"$filepath as (.+?)$name""" =>
        new AttachFile(Some(name), filepath)
      case r"""I attach "(.+?)"$filepath""" =>
        new AttachFile(None, filepath)
      case r"""I (write|append)$mode "(.*?)"$content to "(.+?)"$filepath file""" =>
        new WriteTextToFile(Some(content), None, Some(filepath), None, mode == "write")
      case r"""I (write|append)$mode new line to "(.+?)"$filepath file""" =>
        new WriteNewLineToFile(Some(filepath), None, mode == "write")
      case r"""I (write|append)$mode (.+?)$contentRef to "(.+?)"$filepath file""" =>
        new WriteTextToFile(None, Some(contentRef), Some(filepath), None, mode == "write")
      case r"""I (write|append)$mode "(.*?)"$content to (.+? file)$filepathRef""" =>
        new WriteTextToFile(Some(content), None, None, Some(filepathRef), mode == "write")
      case r"""I (write|append)$mode new line to (.+? file)$filepathRef""" =>
        new WriteNewLineToFile(None, Some(filepathRef), mode == "write")
      case r"""I (write|append)$mode (.+?)$contentRef to (.+? file)$filepathRef""" =>
        new WriteTextToFile(None, Some(contentRef), None, Some(filepathRef), mode == "write")
      case r"""I log record to (.+?)$resultsFileId file""" =>
        new LogResultsRecord(resultsFileId)
      case r"""I download "(.+?)"$url to "(.+?)"$filepath""" =>
        new DownloadToFile(url, Some(filepath), None, defaultConditionTimeoutSecs)
      case r"""I download "(.+?)"$url to (.+?)$filepathRef""" =>
        new DownloadToFile(url, None, Some(filepathRef), defaultConditionTimeoutSecs)
      case r"""I lookup (.+?)$name in the "(.+?)$filepath" file where "(.+)"$predicate""" =>
        new DataLookup(name, name, Some(filepath), None, step.orDocString(predicate))
      case r"""I lookup (.+?)$name in (.+? file)$filepathRef where "(.+)"$predicate""" =>
        new DataLookup(name, name, None, Some(filepathRef), step.orDocString(predicate))
      case r"""I lookup (.+?)$dataName in the "(.+?)$filepath" file as (.+?)$name where "(.+)"$predicate""" =>
        new DataLookup(dataName, name, Some(filepath), None, step.orDocString(predicate))
      case r"""I lookup (.+?)$dataName in (.+? file)$filepathRef as (.+?)$name where "(.+)"$predicate""" =>
        new DataLookup(dataName, name, None, Some(filepathRef), step.orDocString(predicate))
      case "I reset accumulated errors" =>
        new ResetAccumulatedErrors()
      case "there should be no accumulated errors" =>
        new Compare(`gwen.accumulated.errors`, "", ComparisonOperator.be, false, Some("${gwen.accumulated.errors}"), false, false)

      case _ =>
        Errors.undefinedStepError(step)
        
    }
  }

  def defaultConditionTimeoutSecs: Long = 10
  def defaultRepeatDelay: Duration = Duration(1, SECONDS)
  
  def defaultRepeatTimeout(delay: Duration): Duration = delay * 30
  
  def logStatus(options: GwenOptions, node: GherkinNode): Unit = { 
    val msg = s"${node.evalStatus} ${node.nodeType}: $node"
    StatusLogger.log(options, logger, node.evalStatus, msg)
  }  
  
}
