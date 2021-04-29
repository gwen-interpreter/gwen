/*
 * Copyright 2015-2020 Branko Juric, Brady Wood
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
import gwen.eval.binding._
import gwen.model._
import gwen.model.gherkin._

import scala.sys.process.stringSeqToProcess
import scala.sys.process.stringToProcess
import scala.util.{Failure, Success, Try}

import java.io.File
import gwen.eval.support.SQLSupport

/** Provides the common default steps for all engines. */
trait DefaultEngine[T <: EvalContext] extends EvalEngine[T] {

   /**
    * Defines the default composite steps supported by all engines.
    *
    * @param step the step to evaluate
    * @param ctx the evaluation context
    */
  override def evaluateComposite(parent: Identifiable, step: Step, ctx: T): Option[Step] = ctx.withEnv { env => 
        
    Option {
        
      step.expression match {

        case r"""(.+?)$doStep for each data record""" => doEvaluate(step) { _ =>
          val dataTable = env.topScope.getObject(DataTable.tableKey) match {
            case Some(table: FlatTable) => table
            case Some(other) => Errors.dataTableError(s"Cannot use for each on object of type: ${other.getClass.getName}")
            case _ => Errors.dataTableError("Calling step has no data table")
          }
          val records = () => {
            dataTable.records.indices.map(idx => dataTable.recordScope(idx))
          }
          foreach(records, DataTable.recordKey, parent, step, doStep, ctx)
        }

        case r"""(.+?)$doStep for each (.+?)$entry in (.+?)$source delimited by "(.+?)"$$$delimiter""" => doEvaluate(step) { _ =>
          val sourceValue = ctx.getBoundReferenceValue(source)
          val values = () => {
            sourceValue.split(delimiter).toSeq
          }
          foreach(values, entry, parent, step, doStep, ctx)
        }

        case r"""(.+?)$doStep if (.+?)$$$condition""" => doEvaluate(step) { _ =>
          if (condition.matches(""".*( until | while | for each | if ).*""") && !condition.matches(""".*".*((until|while|for each|if)).*".*""")) {
            Errors.illegalStepError("Nested 'if' condition found in illegal step position (only trailing position supported)")
          }
          val javascript = new JavaScriptBinding(condition, ctx).resolve()
          env.getStepDef(doStep) foreach { stepDef =>
            checkStepDefRules(step.copy(withName = doStep, withStepDef = Some(stepDef)), env)
          }
          val iStep = step.copy(withEvalStatus = Pending)
          val tags = List(Tag(ReservedTags.Synthetic), Tag(ReservedTags.If), Tag(ReservedTags.StepDef))
          val iStepDef = Scenario(None, tags, ReservedTags.If.toString, condition, Nil, None, List(step.copy(withName = doStep)), Nil)
          ctx.evaluate(evalStepDef(step, iStepDef, iStep, Nil, ctx)) {
            val boolResult = ctx.evaluateJSPredicate(ctx.interpolate(javascript))
            if (boolResult) {
              logger.info(s"Processing conditional step ($condition = true): ${step.keyword} $doStep")
              evalStepDef(step, iStepDef, iStep, Nil, ctx)
            } else {
              logger.info(s"Skipping conditional step ($condition = false): ${step.keyword} $doStep")
              step.copy(withEvalStatus = Passed(0))
            }
          }
        }

        case _ => null
      }
    }
  }
  
  /**
    * Defines the default steps supported by all engines.
    *
    * @param step the step to evaluate
    * @param ctx the evaluation context
    * @throws gwen.Errors.UndefinedStepException if the given step is undefined
    *         or unsupported
    */
  override def evaluate(step: Step, ctx: T): Unit = ctx.withEnv { env => 

    step.expression match {

      case r"""my (.+?)$name (?:property|setting) (?:is|will be) "(.*?)"$$$value""" =>
        checkStepRules(step, BehaviorType.Context, env)
        Settings.setLocal(name, value)

      case r"""I reset my (.+?)$name (?:property|setting)""" =>
        checkStepRules(step, BehaviorType.Context, env)
        Settings.clearLocal(name)

      case r"""(.+?)$attribute (?:is|will be) "(.*?)"$$$value""" => step.orDocString(value) tap { value =>
        checkStepRules(step, BehaviorType.Context, env)
        env.topScope.set(attribute, value)
      }

      case r"""I wait ([0-9]+?)$duration second(?:s?)""" =>
        checkStepRules(step, BehaviorType.Action, env)
        ctx.perform {
          Thread.sleep(duration.toLong * 1000)
        }
      
      case r"""I execute system process "(.+?)"$$$systemproc""" => step.orDocString(systemproc) tap { systemproc =>
        checkStepRules(step, BehaviorType.Action, env)
        ctx.perform {
          systemproc.! match {
            case 0 =>
            case _ => Errors.systemProcessError(s"The call to system process '$systemproc' has failed.")
          }
        }
      }

      case r"""I execute a unix system process "(.+?)"$$$systemproc""" => step.orDocString(systemproc) tap { systemproc =>
        checkStepRules(step, BehaviorType.Action, env)
        ctx.perform {
          Seq("/bin/sh", "-c", systemproc).! match {
            case 0 =>
            case _ => Errors.systemProcessError(s"The call to system process '$systemproc' has failed.")
          }
        }
      }

      case r"""I execute (?:javascript|js) "(.+?)$javascript"""" => step.orDocString(javascript) tap { javascript =>
        checkStepRules(step, BehaviorType.Action, env)
        ctx.evaluateJS(javascript)
      }


      case r"""I capture (.+?)$attribute by (?:javascript|js) "(.+?)"$$$expression""" => step.orDocString(expression) tap { expression =>
        checkStepRules(step, BehaviorType.Action, env)
        val value = Option(ctx.evaluateJS(ctx.formatJSReturn(ctx.interpolate(expression)))).map(_.toString).orNull
        env.topScope.set(attribute, value tap { content =>
          env.addAttachment(attribute, "txt", content)
        })
      }

      case r"""I capture the (text|node|nodeset)$targetType in (.+?)$source by xpath "(.+?)"$expression as (.+?)$$$name""" =>
        checkStepRules(step, BehaviorType.Action, env)
        val src = ctx.getBoundReferenceValue(source)
        val result = ctx.evaluate(s"$$[dryRun:${BindingType.xpath}]") {
          ctx.evaluateXPath(expression, src, ctx.XMLNodeType.withName(targetType)) tap { content =>
            env.addAttachment(name, "txt", content)
          }
        }
        env.topScope.set(name, result)

      case r"""I capture the text in (.+?)$source by regex "(.+?)"$expression as (.+?)$$$name""" =>
        checkStepRules(step, BehaviorType.Action, env)
        val src = ctx.getBoundReferenceValue(source)
        val result = ctx.evaluate(s"$$[dryRun:${BindingType.regex}]") {
          ctx.extractByRegex(expression, src) tap { content =>
            env.addAttachment(name, "txt", content)
          }
        }
        env.topScope.set(name, result)

      case r"""I capture the content in (.+?)$source by json path "(.+?)"$expression as (.+?)$$$name""" =>
        checkStepRules(step, BehaviorType.Action, env)
        val src = ctx.getBoundReferenceValue(source)
        val result = ctx.evaluate(s"$$[dryRun:${BindingType.`json path`}]") {
          ctx.evaluateJsonPath(expression, src) tap { content =>
            env.addAttachment(name, "txt", content)
          }
        }
        env.topScope.set(name, result)

      case r"""I capture (.+?)$source as (.+?)$$$attribute""" =>
        checkStepRules(step, BehaviorType.Action, env)
        val value = ctx.getBoundReferenceValue(source)
        env.topScope.set(attribute, value tap { content =>
          env.addAttachment(attribute, "txt", content)
        })

      case r"""I capture (.+?)$$$attribute""" =>
        checkStepRules(step, BehaviorType.Action, env)
        val value = ctx.getBoundReferenceValue(attribute)
        env.topScope.set(attribute, value tap { content =>
          env.addAttachment(attribute, "txt", content)
        })

      case r"""I base64 decode (.+?)$attribute as (.+?)$$$name""" =>
        checkStepRules(step, BehaviorType.Action, env)
        val source = ctx.getBoundReferenceValue(attribute)
        val result = ctx.evaluate("$[dryRun:decodeBase64]") {
          ctx.decodeBase64(source) tap { content =>
            env.addAttachment(name, "txt", content)
          }
        }
        env.topScope.set(name, result)

      case r"""I base64 decode (.+?)$attribute""" =>
        checkStepRules(step, BehaviorType.Action, env)
        val source = ctx.getBoundReferenceValue(attribute)
        val result = ctx.evaluate("$[dryRun:decodeBase64]") {
          ctx.decodeBase64(source) tap { content =>
            env.addAttachment(attribute, "txt", content)
          }
        }
        env.topScope.set(attribute, result)

      case r"""(.+?)$attribute (?:is|will be) defined by (javascript|js|system process|property|setting|file)$attrType "(.+?)"$$$expression""" => step.orDocString(expression) tap { expression =>
        checkStepRules(step, BehaviorType.Context, env)
        BindingType.parse(attrType) match {
          case BindingType.javascript => JavaScriptBinding.bind(attribute, expression, env)
          case BindingType.sysproc => SysprocBinding.bind(attribute, expression, env)
          case BindingType.file => FileBinding.bind(attribute, expression, env)
          case _ => env.topScope.set(attribute, Settings.get(expression))
        }
      }

      case r"""(.+?)$attribute (?:is|will be) defined by the (text|node|nodeset)$targetType in (.+?)$source by xpath "(.+?)"$$$expression""" => step.orDocString(expression) tap { expression =>
        checkStepRules(step, BehaviorType.Context, env)
        XPathBinding.bind(attribute, expression, targetType, source, env)
      }

      case r"""(.+?)$attribute (?:is|will be) defined in (.+?)$source by regex "(.+?)"$$$expression""" => step.orDocString(expression) tap { expression =>
        checkStepRules(step, BehaviorType.Context, env)
        RegexBinding.bind(attribute, expression, source, env)
      }

      case r"""(.+?)$attribute (?:is|will be) defined in (.+?)$source by json path "(.+?)"$$$expression""" => step.orDocString(expression) tap { expression =>
        checkStepRules(step, BehaviorType.Context, env)
        JsonPathBinding.bind(attribute, expression, source, env)
      }

      case r"""(.+?)$attribute (?:is|will be) defined by sql "(.+?)"$selectStmt in the (.+?)$dbName database""" =>
        checkStepRules(step, BehaviorType.Context, env)
        SQLBinding.bind(attribute, dbName, selectStmt, env)

      case r"""(.+?)$attribute (?:is|will be) defined in the (.+?)$dbName database by sql "(.+?)"$$$selectStmt""" => step.orDocString(selectStmt) tap { selectStmt =>
        checkStepRules(step, BehaviorType.Context, env)
        SQLBinding.bind(attribute, dbName, selectStmt, env)
      }

      case r"""I update the (.+?)$dbName database by sql "(.+?)"$$$updateStmt""" => step.orDocString(updateStmt) tap { updateStmt =>
        checkStepRules(step, BehaviorType.Action, env)
        SQLSupport.checkDBSettings(dbName)
        val rowsAffected = ctx.evaluate(0) {
          ctx.executeSQLUpdate(updateStmt, dbName)
        }
        env.scopes.set(s"$dbName rows affected", rowsAffected.toString)
      }

      case r"""(.+?)$source at (json path|xpath)$matcher "(.+?)"$path should( not)?$negation (be|contain|start with|end with|match regex|match template|match template file)$operator "(.*?)"$$$expression""" => step.orDocString(expression) tap { expression =>
        checkStepRules(step, BehaviorType.Assertion, env)
        val expected = ctx.parseExpression(operator, expression)
        ctx.perform {
          val src = env.scopes.get(source)
          val actual = BindingType.parse(matcher) match {
            case BindingType.`json path` => ctx.evaluateJsonPath(path, src)
            case BindingType.xpath => ctx.evaluateXPath(
              path, src, ctx.XMLNodeType.text)
          }
          val negate = Option(negation).isDefined
          val result = ctx.compare(s"$source at $matcher '$path'", expected, actual, operator, negate)
          val opName = if (operator.endsWith(" file")) operator.substring(0, operator.length - 5) else operator
          result match {
            case Success(assertion) =>
              assert(assertion, s"Expected $source at $matcher '$path' to ${if(negate) "not " else ""}$opName '$expected' but got '$actual'")
            case Failure(error) =>
              assert(assertion = false, error.getMessage)
          }
        }
      }

      case r"""(.+?)$attribute should( not)?$negation (be|contain|start with|end with|match regex|match xpath|match json path|match template|match template file)$operator "(.*?)"$$$expression""" => step.orDocString(expression) tap { expression =>
        checkStepRules(step, BehaviorType.Assertion, env)
        val binding = ctx.getBinding(attribute)
        val actualValue = binding.resolve()
        val expected = ctx.parseExpression(operator, expression)
        ctx.perform {
          val negate = Option(negation).isDefined
          val result = ctx.compare(attribute, expected, actualValue, operator, negate)
          val opName = if (operator.endsWith(" file")) operator.substring(0, operator.length - 5) else operator
          result match {
            case Success(assertion) =>
              assert(assertion, s"Expected $binding to ${if(negate) "not " else ""}$opName '$expected' but got '$actualValue'")
            case Failure(error) =>
              assert(assertion = false, error.getMessage)
          }
        }
      }

      case r"""(.+?)$attribute should be absent""" =>
        checkStepRules(step, BehaviorType.Assertion, env)
        ctx.perform {
          assert(Try(ctx.getBoundReferenceValue(attribute)).isFailure, s"Expected $attribute to be absent")
        }

      case r"""I attach "(.+?)"$filepath as "(.+?)"$$$name""" =>
        checkStepRules(step, BehaviorType.Action, env)
        val file = new File(filepath)
        if (!file.exists) { 
          Errors.fileAttachError(file, "not found")
        }
        ctx.perform {
          env.addAttachment(name, file)
        }
      
      case _ => Errors.undefinedStepError(step)
      
    }
  }
  
}