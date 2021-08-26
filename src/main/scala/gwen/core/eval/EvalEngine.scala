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
  val DefaultInstance = new EvalEngine[EvalContext]() {
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
abstract class EvalEngine[T <: EvalContext] extends NodeEventDispatcher with UnitEngine[T] with StepTranslator[T] {

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
      case r"""(.+?)$doStep for each (.+?)$entry in (.+?)$source delimited by "(.+?)"$$$delimiter""" =>
        Some(new ForEachDelimited(doStep, entry, source, delimiter, this))
      case r"""(.+?)$doStep if (.+?)$$$condition""" =>
        Some(new IfCondition(doStep, condition, this))
      case r"""(.+?)$doStep (until|while)$operation (.+?)$condition using no delay and (.+?)$timeoutPeriod (minute|second|millisecond)$timeoutUnit (?:timeout|wait)""" =>
        Some(new Repeat(doStep, operation, condition, Duration.Zero, Duration(timeoutPeriod.toLong, timeoutUnit), this))
      case r"""(.+?)$doStep (until|while)$operation (.+?)$condition using no delay""" =>
        Some(new Repeat(doStep, operation, condition, Duration.Zero, defaultRepeatTimeout(defaultRepeatDelay), this))
      case r"""(.+?)$doStep (until|while)$operation (.+?)$condition using (.+?)$delayPeriod (second|millisecond)$delayUnit delay and (.+?)$timeoutPeriod (minute|second|millisecond)$timeoutUnit (?:timeout|wait)""" if doStep != "I wait" =>
        Some(new Repeat(doStep, operation, condition, Duration(delayPeriod.toLong, delayUnit), Duration(timeoutPeriod.toLong, timeoutUnit), this))
      case r"""(.+?)$doStep (until|while)$operation (.+?)$condition using (.+?)$delayPeriod (second|millisecond)$delayUnit delay""" if doStep != "I wait" =>
        val delayDuration = Duration(delayPeriod.toLong, delayUnit)
        Some(new Repeat(doStep, operation, condition, delayDuration, defaultRepeatTimeout(delayDuration), this))
      case r"""(.+?)$doStep (until|while)$operation (.+?)$condition using (.+?)$timeoutPeriod (minute|second|millisecond)$timeoutUnit (?:timeout|wait)""" if doStep != "I wait" =>
        Some(new Repeat(doStep, operation, condition, defaultRepeatDelay, Duration(timeoutPeriod.toLong, timeoutUnit), this))
      case r"""(.+?)$doStep (until|while)$operation (.+?)$$$condition""" if (doStep != "I wait" && !step.expression.matches(""".*".*(until|while).*".*""")) =>
        Some(new Repeat(doStep, operation, condition, defaultRepeatDelay, defaultRepeatTimeout(defaultRepeatDelay), this))
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
      case r"""my (.+?)$name (?:property|setting) (?:is|will be) "(.*?)"$$$value""" =>
        new SetProperty(name, value)
      case r"""I reset my (.+?)$name (?:property|setting)""" =>
        new ClearProperty(name)
      case r"""(.+?)$attribute (?:is|will be) "(.*?)"$$$value""" =>
        new BindAttribute(attribute, step.orDocString(value))
      case r"""I wait (\d+)$duration second(?:s?)""" =>
        new Sleep(duration.toInt)
      case r"""I execute system process "(.+?)"$systemproc delimited by "(.+?)"$delimiter""" =>
        new ExecuteSysProc(systemproc, Some(delimiter))
      case r"""I execute system process "(.+?)"$$$systemproc""" =>
        new ExecuteSysProc(step.orDocString(systemproc), None)
      case r"""I execute a unix system process "(.+?)"$systemproc delimited by "(.+?)"$delimiter""" =>
        new ExecuteSysProcUnix(systemproc, Some(delimiter))
      case r"""I execute a unix system process "(.+?)"$$$systemproc""" =>
        new ExecuteSysProcUnix(step.orDocString(systemproc), None)
      case r"""I execute (?:javascript|js) "(.+?)$javascript"""" =>
        new ExecuteJS(step.orDocString(javascript))
      case r"""I capture (.+?)$attribute by (?:javascript|js) "(.+?)"$$$expression""" =>
        new CaptureByJS(attribute, step.orDocString(expression))
      case r"""I capture the (text|node|nodeset)$targetType in (.+?)$source by xpath "(.+?)"$expression as (.+?)$$$name""" =>
        new CaptureByXPath(name, expression, source, XMLNodeType.valueOf(targetType))
      case r"""I capture the text in (.+?)$source by regex "(.+?)"$expression as (.+?)$$$name""" =>
        new CaptureByRegex(name, expression, source)
      case r"""I capture the content in (.+?)$source by json path "(.+?)"$expression as (.+?)$$$name""" =>
        new CaptureByJsonPath(name, expression, source)
      case r"""I capture (.+?)$source as (.+?)$$$attribute""" =>
        new Capture(attribute, source)
      case r"""I capture (.+?)$$$attribute""" =>
        new Capture(attribute, attribute)
      case r"""I base64 decode (.+?)$attribute as (.+?)$$$name""" =>
        new CaptureBase64Decoded(name, attribute)
      case r"""I base64 decode (.+?)$attribute""" =>
        new CaptureBase64Decoded(attribute, attribute)
      case r"""(.+?)$attribute (?:is|will be) defined by system process "(.+?)"$expression delimited by "(.+?)"$delimiter""" =>
        new BindAsType(attribute, BindingType.sysproc, step.orDocString(expression), Some(delimiter))
      case r"""(.+?)$attribute (?:is|will be) defined by (javascript|js|system process|property|setting|file)$attrType "(.+?)"$$$expression""" =>
        new BindAsType(attribute, BindingType.parse(attrType), step.orDocString(expression), None)
      case r"""(.+?)$attribute (?:is|will be) defined by the (text|node|nodeset)$targetType in (.+?)$source by xpath "(.+?)"$$$expression""" =>
        new BindAsXPath(attribute, step.orDocString(expression), targetType, source)
      case r"""(.+?)$attribute (?:is|will be) defined in (.+?)$source by regex "(.+?)"$$$expression""" =>
        new BindAsRegex(attribute, step.orDocString(expression), source)
      case r"""(.+?)$attribute (?:is|will be) defined in (.+?)$source by json path "(.+?)"$$$expression""" =>
        new BindAsJsonPath(attribute, step.orDocString(expression), source)
      case r"""(.+?)$attribute (?:is|will be) defined by sql "(.+?)"$selectStmt in the (.+?)$dbName database""" =>
        new BindAsSQL(attribute, dbName, selectStmt)
      case r"""(.+?)$attribute (?:is|will be) defined in the (.+?)$dbName database by sql "(.+?)"$$$selectStmt""" =>
        new BindAsSQL(attribute, dbName, step.orDocString(selectStmt))
      case r"""I update the (.+?)$dbName database by sql "(.+?)"$$$updateStmt""" =>
        new UpdateBySQL(dbName, step.orDocString(updateStmt))
      case r"""(.+?)$source at (json path|xpath)$matcher "(.+?)"$path should( not)?$negation (be|contain|start with|end with|match regex|match template|match template file)$operator "(.*?)"$$$expression""" =>
        new CompareByPath(source, BindingType.valueOf(matcher), path, step.orDocString(expression), ComparisonOperator.valueOf(operator), Option(negation).isDefined)
      case r"""(.+?)$attribute should( not)?$negation (be|contain|start with|end with|match regex|match xpath|match json path|match template|match template file)$operator "(.*?)"$$$expression""" =>
        new Compare(attribute, step.orDocString(expression), ComparisonOperator.valueOf(operator), Option(negation).isDefined)
      case r"""(.+?)$attribute should be absent""" =>
        new IsAbsent(attribute)  
      case r"""I attach "(.+?)"$filepath as "(.+?)"$$$name""" =>
        new AttachFile(name, filepath)
      case _ =>
        Errors.undefinedStepError(step)
        
    }
  }

  def defaultRepeatDelay: Duration = Duration(1, SECONDS)
  
  private def defaultRepeatTimeout(delay: Duration): Duration = delay * 30

  def logStatus(node: GherkinNode): Unit = { 
    val msg = s"${node.evalStatus} ${node.nodeType}: ${node.name}"
    node.evalStatus match {
      case Loaded => logger.debug(msg)
      case Passed(_) => logger.info(msg)
      case Failed(_, _) => logger.error(msg)
      case Sustained(_, _) => logger.warn(msg)
      case _ => logger.warn(msg)
    }
  }
  
}
