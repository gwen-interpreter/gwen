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

package gwen.core.eval

import gwen.core._
import gwen.core.eval.binding._
import gwen.core.eval.engine.UnitEngine
import gwen.core.eval.lambda._
import gwen.core.eval.lambda.composite._
import gwen.core.eval.lambda.unit._
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
abstract class EvalEngine[T <: EvalContext] extends NodeEventDispatcher with UnitEngine[T] with StepTranslator[T] with GwenInfo {

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
  override def translateCompositeStep(step: Step): Option[CompositeStep[T]] = {
    step.expression match {
      case r"""(.+?)$doStep for each data record""" =>
        Some(new ForEachTableRecord(doStep, this))
      case r"""(.+?)$doStep for each (.+?)$entry in (.+?)$source delimited by "(.+?)"$delimiter""" =>
        Some(new ForEachDelimited(doStep, entry, source, delimiter, this))
      case r"""(.+)$doStep if (.+?)$attribute is( not)?$negation defined""" =>
        Some(new IfDefinedCondition(doStep, attribute, Option(negation).isDefined, this))
      case r"""(.+)$doStep if (.+?)$attribute is( not)?$negation blank""" =>
        Some(new IfCompareCondition(doStep, attribute, ComparisonOperator.be, Option(negation).isDefined, "", this))
      case r"""(.+)$doStep if (.+?)$attribute is( not)?$negation "(.*?)"$expression""" =>
        Some(new IfCompareCondition(doStep, attribute, ComparisonOperator.be, Option(negation).isDefined, expression, this))
      case r"""(.+)$doStep if (.+?)$attribute (contains|starts with|ends with|matches regex|matches xpath|matches json path|matches template|matches template file)$operator "(.*?)"$expression""" =>
        Some(new IfCompareCondition(doStep, attribute, ComparisonOperator.fromModal(operator), false, expression, this))
      case r"""(.+)$doStep if (.+?)$attribute does not (contain|start with|end with|match regex|match xpath|match json path|match template|match template file)$operator "(.*?)"$expression""" =>
        Some(new IfCompareCondition(doStep, attribute, ComparisonOperator.valueOf(operator), true, expression, this))
      case r"""(.+)$doStep if(?:(?!\bif\b))( not)?$negation (.+)$condition""" if !condition.contains('"') =>
        Some(new IfCondition(doStep, condition, Option(negation).isDefined, defaultConditionTimeoutSecs, this))
      case r"""(.+?)$doStep (until|while)$operation (.+?)$attribute is( not)?$negation defined using no delay and (.+?)$timeoutPeriod (minute|second|millisecond)$timeoutUnit (?:timeout|wait)""" =>
        Some(new RepeatIfDefined(doStep, operation, attribute, Option(negation).isDefined, Duration.Zero, Duration(timeoutPeriod.toLong, timeoutUnit), defaultConditionTimeoutSecs, this))
      case r"""(.+?)$doStep (until|while)$operation (.+?)$attribute is( not)?$negation defined using no delay""" =>
        Some(new RepeatIfDefined(doStep, operation, attribute, Option(negation).isDefined, Duration.Zero, defaultRepeatTimeout(defaultRepeatDelay), defaultConditionTimeoutSecs, this))
      case r"""(.+?)$doStep (until|while)$operation (.+?)$attribute is( not)?$negation defined using (.+?)$delayPeriod (second|millisecond)$delayUnit delay and (.+?)$timeoutPeriod (minute|second|millisecond)$timeoutUnit (?:timeout|wait)""" if doStep != "I wait" =>
        Some(new RepeatIfDefined(doStep, operation, attribute, Option(negation).isDefined, Duration(delayPeriod.toLong, delayUnit), Duration(timeoutPeriod.toLong, timeoutUnit), defaultConditionTimeoutSecs, this))
      case r"""(.+?)$doStep (until|while)$operation (.+?)$attribute is( not)?$negation defined using (.+?)$delayPeriod (second|millisecond)$delayUnit delay""" if doStep != "I wait" =>
        val delayDuration = Duration(delayPeriod.toLong, delayUnit)
        Some(new RepeatIfDefined(doStep, operation, attribute, Option(negation).isDefined, delayDuration, defaultRepeatTimeout(delayDuration), defaultConditionTimeoutSecs, this))
      case r"""(.+?)$doStep (until|while)$operation (.+?)$attribute is( not)?$negation defined using (.+?)$timeoutPeriod (minute|second|millisecond)$timeoutUnit (?:timeout|wait)""" if doStep != "I wait" =>
        Some(new RepeatIfDefined(doStep, operation, attribute, Option(negation).isDefined, defaultRepeatDelay, Duration(timeoutPeriod.toLong, timeoutUnit), defaultConditionTimeoutSecs, this))
      case r"""(.+?)$doStep (until|while)$operation (.+?)$attribute is( not)?$negation defined""" if (doStep != "I wait" && !step.expression.matches(""".*".*(until|while).*".*""")) =>
        Some(new RepeatIfDefined(doStep, operation, attribute, Option(negation).isDefined, defaultRepeatDelay, defaultRepeatTimeout(defaultRepeatDelay), defaultConditionTimeoutSecs, this))
      case r"""(.+?)$doStep (until|while)$operation (.+?)$condition using no delay and (.+?)$timeoutPeriod (minute|second|millisecond)$timeoutUnit (?:timeout|wait)""" if !condition.contains('"') =>
        Some(new RepeatJS(doStep, operation, condition, Duration.Zero, Duration(timeoutPeriod.toLong, timeoutUnit), defaultConditionTimeoutSecs, this))
      case r"""(.+?)$doStep (until|while)$operation (.+?)$condition using no delay""" if !condition.contains('"') =>
        Some(new RepeatJS(doStep, operation, condition, Duration.Zero, defaultRepeatTimeout(defaultRepeatDelay), defaultConditionTimeoutSecs, this))
      case r"""(.+?)$doStep (until|while)$operation (.+?)$condition using (.+?)$delayPeriod (second|millisecond)$delayUnit delay and (.+?)$timeoutPeriod (minute|second|millisecond)$timeoutUnit (?:timeout|wait)""" if doStep != "I wait" && !condition.contains('"') =>
        Some(new RepeatJS(doStep, operation, condition, Duration(delayPeriod.toLong, delayUnit), Duration(timeoutPeriod.toLong, timeoutUnit), defaultConditionTimeoutSecs, this))
      case r"""(.+?)$doStep (until|while)$operation (.+?)$condition using (.+?)$delayPeriod (second|millisecond)$delayUnit delay""" if doStep != "I wait" && !condition.contains('"') =>
        val delayDuration = Duration(delayPeriod.toLong, delayUnit)
        Some(new RepeatJS(doStep, operation, condition, delayDuration, defaultRepeatTimeout(delayDuration), defaultConditionTimeoutSecs, this))
      case r"""(.+?)$doStep (until|while)$operation (.+?)$condition using (.+?)$timeoutPeriod (minute|second|millisecond)$timeoutUnit (?:timeout|wait)""" if doStep != "I wait" && !condition.contains('"') =>
        Some(new RepeatJS(doStep, operation, condition, defaultRepeatDelay, Duration(timeoutPeriod.toLong, timeoutUnit), defaultConditionTimeoutSecs, this))
      case r"""(.+?)$doStep (until|while)$operation (.+?)$condition""" if (doStep != "I wait" && !condition.contains('"') && !step.expression.matches(""".*".*(until|while).*".*""")) =>
        Some(new RepeatJS(doStep, operation, condition, defaultRepeatDelay, defaultRepeatTimeout(defaultRepeatDelay), defaultConditionTimeoutSecs, this))
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
  override def translateStep(step: Step): UnitStep[T] = {
    step.expression match {
      case r"""my (.+?)$name (?:property|setting) (?:is|will be) "(.*?)"$value""" =>
        new SetProperty(name, value)
      case r"""I reset my (.+?)$name (?:property|setting)""" =>
        new ClearProperty(name)
      case r"""(.+?)$attribute (?:is|will be) "(.*?)"$value""" =>
        new BindAttribute(attribute, step.orDocString(value))
      case r"""(.+?)$attribute (?:is|will be) (blank|true|false)$literal""" =>
        new BindAttribute(attribute, ValueLiteral.valueOf(literal).value)
      case r"""I wait (\d+)$duration second(?:s?)""" =>
        new Sleep(duration.toInt)
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
      case r"""I capture the similarity score of (.+?)$attribute1 compared to "(.+?)"$value2( ignoring case)?$ignoringCase as (.+?)$name""" =>
        new CaptureSimilarity(name, attribute1, None, Some(value2), Option(ignoringCase).isDefined)
      case r"""I capture the similarity score of (.+?)$attribute1 compared to (.+?)$attribute2( ignoring case)?$ignoringCase as (.+?)$name""" =>
        new CaptureSimilarity(name, attribute1, Some(attribute2), None, Option(ignoringCase).isDefined)
      case r"""I capture the similarity score of (.+?)$attribute1 compared to "(.+?)"$value2( ignoring case)?$ignoringCase""" =>
        new CaptureSimilarity("similarity score", attribute1, None, Some(value2), Option(ignoringCase).isDefined)
      case r"""I capture the similarity score of (.+?)$attribute1 compared to (.+?)$attribute2( ignoring case)?$ignoringCase""" =>
        new CaptureSimilarity("similarity score", attribute1, Some(attribute2), None, Option(ignoringCase).isDefined)
      case r"""I capture the PDF text from (url|file)$locationType "(.+?)"$location as (.+?)$name""" =>
        new CapturePDF(name, LocationType.valueOf(locationType), location, defaultConditionTimeoutSecs)
      case r"""I capture the PDF text from (url|file)$locationType "(.+?)"$location""" =>
        new CapturePDF("the PDF text", LocationType.valueOf(locationType), location, defaultConditionTimeoutSecs)
      case r"""I capture (.+?)$source as (.+?)$attribute""" =>
        new Capture(attribute, source)
      case r"""I capture (.+?)$attribute""" =>
        new Capture(attribute, attribute)
      case r"""I base64 decode (.+?)$attribute as (.+?)$name""" =>
        new CaptureBase64Decoded(name, attribute)
      case r"""I base64 decode (.+?)$attribute""" =>
        new CaptureBase64Decoded(attribute, attribute)
      case r"""(.+?)$attribute (?:is|will be) defined by system process "(.+?)"$expression delimited by "(.+?)"$delimiter""" =>
        new BindAsType(attribute, BindingType.sysproc, expression, None, Some(delimiter))
      case r"""(.+?)$attribute (?:is|will be) defined by (javascript|js|system process|unix system process|property|setting|file)$attrType "(.+?)"$expression""" =>
        new BindAsType(attribute, BindingType.parse(attrType), step.orDocString(expression), None, None)
      case r"""(.+?)$attribute (?:is|will be) defined by (.+?)$function applied to "(.+?)"$args delimited by "(.*)"$delimiter""" =>
        new BindAsType(attribute, BindingType.function, function, Some(args), Some(delimiter))
      case r"""(.+?)$attribute (?:is|will be) defined by (.+?)$function applied to "(.*)"$arg""" =>
        new BindAsType(attribute, BindingType.function, function, Some(step.orDocString(arg)), None)
      case r"""(.+?)$attribute (?:is|will be) defined by the (text|node|nodeset)$targetType in (.+?)$source by xpath "(.+?)"$expression""" =>
        new BindAsXPath(attribute, step.orDocString(expression), targetType, source)
      case r"""(.+?)$attribute (?:is|will be) defined in (.+?)$source by regex "(.+?)"$expression""" =>
        new BindAsRegex(attribute, step.orDocString(expression), source)
      case r"""(.+?)$attribute (?:is|will be) defined in (.+?)$source by json path "(.+?)"$expression""" =>
        new BindAsJsonPath(attribute, step.orDocString(expression), source)
      case r"""(.+?)$attribute (?:is|will be) defined by sql "(.+?)"$selectStmt in the (.+?)$dbName database""" =>
        new BindAsSQL(attribute, dbName, selectStmt)
      case r"""(.+?)$attribute (?:is|will be) defined in the (.+?)$dbName database by sql "(.+?)"$selectStmt""" =>
        new BindAsSQL(attribute, dbName, step.orDocString(selectStmt))
      case r"""I update the (.+?)$dbName database by sql "(.+?)"$updateStmt""" =>
        new UpdateBySQL(dbName, step.orDocString(updateStmt))
      case r"""(.+?)$source at (json path|xpath)$matcher "(.+?)"$path should( not)?$negation be (blank|true|false)$literal""" =>
        new CompareByPath(source, BindingType.valueOf(matcher), path, ValueLiteral.valueOf(literal).value, ComparisonOperator.be, Option(negation).isDefined, step.message)
      case r"""(.+?)$source at (json path|xpath)$matcher "(.+?)"$path should( not)?$negation (be|contain|start with|end with|match regex|match template|match template file)$operator "(.*?)"$expression""" =>
        new CompareByPath(source, BindingType.valueOf(matcher), path, step.orDocString(expression), ComparisonOperator.valueOf(operator), Option(negation).isDefined, step.message)
      case r"""(.+?)$attribute should( not)?$negation (be|be less than|be at most|be more than|be at least)$operator (\d+(?:\.\d*)?)$percentage% similar to "(.+?)"$value2( ignoring case)?$ignoringCase""" =>
        new CheckSimilarity(attribute, None, Some(value2), SimilarityOperator.valueOf(operator), percentage.toDouble, Option(ignoringCase).isDefined, Option(negation).isDefined, step.message)
      case r"""(.+?)$attribute1 should( not)?$negation (be|be less than|be at most|be more than|be at least)$operator (\d+(?:\.\d*)?)$percentage% similar to (.+?)$attribute2( ignoring case)?$ignoringCase""" =>
        new CheckSimilarity(attribute1, Some(attribute2), None, SimilarityOperator.valueOf(operator), percentage.toDouble, Option(ignoringCase).isDefined, Option(negation).isDefined, step.message)
      case r"""(.+?)$attribute should( not)?$negation be (blank|true|false)$literal""" =>
        new Compare(attribute, ValueLiteral.valueOf(literal).value, ComparisonOperator.be, Option(negation).isDefined, step.message)
      case r"""(.+?)$attribute should( not)?$negation (be|contain|start with|end with|match regex|match xpath|match json path|match template|match template file)$operator "(.*?)"$expression""" =>
        new Compare(attribute, step.orDocString(expression), ComparisonOperator.valueOf(operator), Option(negation).isDefined, step.message)
      case r"""(.+?)$attribute should be absent""" =>
        new IsDefined(attribute, true, step.message)
      case r"""(.+?)$attribute should( not)?$negation be defined""" =>
        new IsDefined(attribute, Option(negation).isDefined, step.message)
      case r"""I attach "(.+?)"$filepath as "(.+?)"$name""" =>
        new AttachFile(name, filepath)
      case r"""I attach "(.+?)"$filepath as (.+?)$name""" =>
        new AttachFile(name, filepath)
      case r"""I (write|append)$mode "(.*?)"$content to "(.+?)"$filepath file""" =>
        new WriteTextToFile(Some(content), None, Some(filepath), None, mode == "write")
      case r"""I (write|append)$mode new line to "(.+?)"$filepath file""" =>
        new WriteNewLineToFile(Some(filepath), None, mode == "write")
      case r"""I (write|append)$mode (.+?)$contentRef to "(.+?)"$filepath file""" =>
        new WriteTextToFile(None, Some(contentRef), Some(filepath), None, mode == "write")
      case r"""I (write|append)$mode "(.*?)"$content to (.+?)$filepathRef file""" =>
        new WriteTextToFile(Some(content), None, None, Some(filepathRef), mode == "write")
      case r"""I (write|append)$mode new line to (.+?)$filepathRef file""" =>
        new WriteNewLineToFile(None, Some(filepathRef), mode == "write")
      case r"""I (write|append)$mode (.+?)$contentRef to (.+?)$filepathRef file""" =>
        new WriteTextToFile(None, Some(contentRef), None, Some(filepathRef), mode == "write")
      case r"""I download "(.+?)"$url to "(.+?)"$filepath""" =>
        new DownloadToFile(url, Some(filepath), None, defaultConditionTimeoutSecs)
      case r"""I download "(.+?)"$url to (.+?)$filepathRef""" =>
        new DownloadToFile(url, None, Some(filepathRef), defaultConditionTimeoutSecs)
      case r"""I lookup (.+?)$name in the "(.+?)$filepath" file where "(.+)"$predicate""" =>
        new DataLookup(name, name, Some(filepath), None, step.orDocString(predicate))
      case r"""I lookup (.+?)$name in (.+?)$filepathRef file where "(.+)"$predicate""" =>
        new DataLookup(name, name, None, Some(filepathRef), step.orDocString(predicate))
      case r"""I lookup (.+?)$dataName in the "(.+?)$filepath" file as (.+?)$name where "(.+)"$predicate""" =>
        new DataLookup(dataName, name, Some(filepath), None, step.orDocString(predicate))
      case r"""I lookup (.+?)$dataName in (.+?)$filepathRef file as (.+?)$name where "(.+)"$predicate""" =>
        new DataLookup(dataName, name, None, Some(filepathRef), step.orDocString(predicate))
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
