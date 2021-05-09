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
import gwen.core.eval.step._
import gwen.core.eval.step.composite._
import gwen.core.eval.support.XMLNodeType
import gwen.core.model._
import gwen.core.model.event.LifecycleEventDispatcher
import gwen.core.model.gherkin._

import scala.concurrent.duration._

object EvalEngine {
  val DefaultInstance = new EvalEngine[EvalContext]() {
    override def init(options: GwenOptions, envOpt: Option[EvalEnvironment] = None): EvalContext = {
      new EvalContext(options, new EvalEnvironment())
    }
  }
}

/**
  * Base evaluation engine with default DSL implementation.
  *
  * @author Branko Juric
  */
abstract class EvalEngine[T <: EvalContext] extends UnitEngine[T] with StepTranslator[T] {

  val lifecycle = new LifecycleEventDispatcher()

  /**
    * Initialises the engine and returns a new evaluation context.
    * 
    * @param options command line options
    * @param envOpt optional environment context to use
    */
  def init(options: GwenOptions, envOpt: Option[EvalEnvironment]): T

  /**
    * Translates a composite DSL step into an engine operation.
    *
    * @param parent the parent (calling node)
    * @param step the step to translate
    * @param env the environment state
    * @param ctx the evaluation context
    * @return an optional function that performs the composite step operation and returns it in evaluated form
    */
  override def translateComposite(parent: Identifiable, step: Step, env: EvalEnvironment, ctx: T): Option[CompositeStep[T]] = {
    step.expression match {
      case r"""(.+?)$doStep for each data record""" => Some {
        new ForEachTableRecord(doStep, this, ctx)
      }
      case r"""(.+?)$doStep for each (.+?)$entry in (.+?)$source delimited by "(.+?)"$$$delimiter""" =>  Some { 
        new ForEachDelimited(doStep, entry, source, delimiter, this, ctx)
      }
      case r"""(.+?)$doStep if (.+?)$$$condition""" =>  Some {
        new IfCondition(doStep, condition, this, ctx)
      }
      case r"""(.+?)$doStep (until|while)$operation (.+?)$condition using no delay and (.+?)$timeoutPeriod (minute|second|millisecond)$timeoutUnit (?:timeout|wait)""" => Some {
        new Repeat(doStep, operation, condition, Duration.Zero, Duration(timeoutPeriod.toLong, timeoutUnit), this, ctx)
      }
      case r"""(.+?)$doStep (until|while)$operation (.+?)$condition using no delay""" => Some {
        new Repeat(doStep, operation, condition, Duration.Zero, defaultRepeatTimeout(DefaultRepeatDelay), this, ctx)
      }
      case r"""(.+?)$doStep (until|while)$operation (.+?)$condition using (.+?)$delayPeriod (second|millisecond)$delayUnit delay and (.+?)$timeoutPeriod (minute|second|millisecond)$timeoutUnit (?:timeout|wait)""" => Some {
        new Repeat(doStep, operation, condition, Duration(delayPeriod.toLong, delayUnit), Duration(timeoutPeriod.toLong, timeoutUnit), this, ctx)
      }
      case r"""(.+?)$doStep (until|while)$operation (.+?)$condition using (.+?)$delayPeriod (second|millisecond)$delayUnit delay""" => Some {
        val delayDuration = Duration(delayPeriod.toLong, delayUnit)
        new Repeat(doStep, operation, condition, delayDuration, defaultRepeatTimeout(delayDuration), this, ctx)
      }
      case r"""(.+?)$doStep (until|while)$operation (.+?)$condition using (.+?)$timeoutPeriod (minute|second|millisecond)$timeoutUnit (?:timeout|wait)""" => Some {
        new Repeat(doStep, operation, condition, DefaultRepeatDelay, Duration(timeoutPeriod.toLong, timeoutUnit), this, ctx)
      }
      case r"""(.+?)$doStep (until|while)$operation (.+?)$$$condition""" if (doStep != "I wait" && !step.expression.matches(""".*".*(until|while).*".*""")) => Some {
        new Repeat(doStep, operation, condition, DefaultRepeatDelay, defaultRepeatTimeout(DefaultRepeatDelay), this, ctx)
      }
      case _ => 
        env.getStepDef(step.name) match {
          case Some((stepDef, params)) if stepDef.isForEach && stepDef.isDataTable =>
            val dataTable = ForEachTableRecord.parseFlatTable {
              stepDef.tags.find(_.name.startsWith(s"${ReservedTags.DataTable.toString}(")) map { 
                tag => DataTable(tag, step) 
              }
            }
            Some(new ForEachTableRecordAnnotated(stepDef, step, dataTable, this, ctx))
          case _ => None
        }
    }
  }

  /**
    * Translates a DSL step into an engine operation.
    *
    * @param parent the parent (calling node)
    * @param step the step to translate
    * @param env the environment state
    * @param ctx the evaluation context
    * @return a step operation that throws an exception on failure
    */
  override def translate(parent: Identifiable, step: Step, env: EvalEnvironment, ctx: T): UnitStep[T] = {
    step.expression match {
      case r"""my (.+?)$name (?:property|setting) (?:is|will be) "(.*?)"$$$value""" =>
        new SetProperty(name, value, this, ctx)
      case r"""I reset my (.+?)$name (?:property|setting)""" =>
        new ClearProperty(name, this, ctx)
      case r"""(.+?)$attribute (?:is|will be) "(.*?)"$$$value""" =>
        new Bind(attribute, step.orDocString(value), this, ctx)
      case r"""I wait ([0-9]+?)$duration second(?:s?)""" =>
        new Sleep(duration.toLong, this, ctx)
      case r"""I execute system process "(.+?)"$$$systemproc""" =>
        new ExecuteSysProc(step.orDocString(systemproc), this, ctx)
      case r"""I execute a unix system process "(.+?)"$$$systemproc""" =>
        new ExecuteSysProcUnix(step.orDocString(systemproc), this, ctx)
      case r"""I execute (?:javascript|js) "(.+?)$javascript"""" =>
        new ExecuteJS(step.orDocString(javascript), this, ctx)
      case r"""I capture (.+?)$attribute by (?:javascript|js) "(.+?)"$$$expression""" =>
        new CaptureByJS(attribute, step.orDocString(expression), this, ctx)
      case r"""I capture the (text|node|nodeset)$targetType in (.+?)$source by xpath "(.+?)"$expression as (.+?)$$$name""" =>
        new CaptureByXPath(name, expression, source, XMLNodeType.withName(targetType), this, ctx)
      case r"""I capture the text in (.+?)$source by regex "(.+?)"$expression as (.+?)$$$name""" =>
        new CaptureByRegex(name, expression, source, this, ctx)
      case r"""I capture the content in (.+?)$source by json path "(.+?)"$expression as (.+?)$$$name""" =>
        new CaptureByJsonPath(name, expression, source, this, ctx)
      case r"""I capture (.+?)$source as (.+?)$$$attribute""" =>
        new Capture(attribute, source, this, ctx)
      case r"""I capture (.+?)$$$attribute""" =>
        new Capture(attribute, attribute, this, ctx)
      case r"""I base64 decode (.+?)$attribute as (.+?)$$$name""" =>
        new CaptureBase64Decoded(name, attribute, this, ctx)
      case r"""I base64 decode (.+?)$attribute""" =>
        new CaptureBase64Decoded(attribute, attribute, this, ctx)
      case r"""(.+?)$attribute (?:is|will be) defined by (javascript|js|system process|property|setting|file)$attrType "(.+?)"$$$expression""" =>
        new BindAsType(attribute, BindingType.parse(attrType), step.orDocString(expression), this, ctx)
      case r"""(.+?)$attribute (?:is|will be) defined by the (text|node|nodeset)$targetType in (.+?)$source by xpath "(.+?)"$$$expression""" =>
        new BindAsXPath(attribute, step.orDocString(expression), targetType, source, this, ctx)
      case r"""(.+?)$attribute (?:is|will be) defined in (.+?)$source by regex "(.+?)"$$$expression""" =>
        new BindAsRegex(attribute, step.orDocString(expression), source, this, ctx)
      case r"""(.+?)$attribute (?:is|will be) defined in (.+?)$source by json path "(.+?)"$$$expression""" =>
        new BindAsJsonPath(attribute, step.orDocString(expression), source, this, ctx)
      case r"""(.+?)$attribute (?:is|will be) defined by sql "(.+?)"$selectStmt in the (.+?)$dbName database""" =>
        new BindAsSQL(attribute, dbName, selectStmt, this, ctx)
      case r"""(.+?)$attribute (?:is|will be) defined in the (.+?)$dbName database by sql "(.+?)"$$$selectStmt""" =>
        new BindAsSQL(attribute, dbName, step.orDocString(selectStmt), this, ctx)
      case r"""I update the (.+?)$dbName database by sql "(.+?)"$$$updateStmt""" =>
        new UpdateBySQL(dbName, step.orDocString(updateStmt), this, ctx)
      case r"""(.+?)$source at (json path|xpath)$matcher "(.+?)"$path should( not)?$negation (be|contain|start with|end with|match regex|match template|match template file)$operator "(.*?)"$$$expression""" =>
        new CompareByPath(source, BindingType.withName(matcher), path, Option(negation).isDefined, operator, ctx.parseExpression(operator, step.orDocString(expression)), this, ctx)
      case r"""(.+?)$attribute should( not)?$negation (be|contain|start with|end with|match regex|match xpath|match json path|match template|match template file)$operator "(.*?)"$$$expression""" =>
        new Compare(attribute, Option(negation).isDefined, operator, ctx.parseExpression(operator, step.orDocString(expression)), this, ctx)
      case r"""(.+?)$attribute should be absent""" =>
        new IsAbsent(attribute, this, ctx)  
      case r"""I attach "(.+?)"$filepath as "(.+?)"$$$name""" =>
        new AttachFile(name, filepath, this, ctx)        
      case _ =>  Errors.undefinedStepError(step)
    }
  }

  val DefaultRepeatDelay: Duration = Duration(1, SECONDS)
  
  private def defaultRepeatTimeout(delay: Duration): Duration = delay * 30

  def logStatus(node: SpecNode): Unit = { 
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
