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

package gwen.eval.support

import gwen._
import gwen.dsl._
import gwen.eval.{EnvContext, EvalEngine}

import scala.sys.process.stringSeqToProcess
import scala.sys.process.stringToProcess
import scala.util.{Failure, Success, Try}

import java.io.File

/** Provides the common default steps that all engines can support. */
trait DefaultEngineSupport[T <: EnvContext] extends EvalEngine[T] {

   /**
    * Defines the default priority steps supported by all engines. For example, a step that calls another step needs
    * to execute with priority to ensure that there is no match conflict between the two (which can occur if the
     * step being called by a step is a StepDef or another step that matches the entire calling step).
    *
    * @param step the step to evaluate
    * @param env the environment context
    */
  override def evaluatePriority(parent: Identifiable, step: Step, env: T): Option[Step] = Option {

    step.expression match {

      case r"""(.+?)$doStep for each data record""" => doEvaluate(step, env) { _ =>
        val dataTable = env.topScope.getObject("table") match {
          case Some(table: FlatTable) => table
          case Some(other) => Errors.dataTableError(s"Cannot use for each on object of type: ${other.getClass.getName}")
          case _ => Errors.dataTableError("Calling step has no data table")
        }
        val records = () => {
          dataTable.records.indices.map(idx => dataTable.recordScope(idx))
        }
        foreach(records, "record", parent, step, doStep, env)
      }

      case r"""(.+?)$doStep for each (.+?)$entry in (.+?)$source delimited by "(.+?)"$$$delimiter""" => doEvaluate(step, env) { _ =>
        val sourceValue = env.getBoundReferenceValue(source)
        val values = () => {
          sourceValue.split(delimiter).toSeq
        }
        foreach(values, entry, parent, step, doStep, env)
      }

      case r"""(.+?)$doStep if (.+?)$$$condition""" => doEvaluate(step, env) { _ =>
        val javascript = env.scopes.get(s"$condition/javascript")
        env.getStepDef(doStep) foreach { stepDef =>
          checkStepDefRules(step.copy(withName = doStep, withStepDef = Some(stepDef)), env)
        }
        val iStep = step.copy(withEvalStatus = Pending)
        val tags = List(Tag(ReservedTags.Synthetic), Tag(ReservedTags.If), Tag(ReservedTags.StepDef))
        val sdPath = SourceRef.nodePath(s"${iStep.sourceRef.flatMap(_.nodePath).getOrElse("/")}/$condition", 1)
        val iStepDef = Scenario(iStep.sourceRef, tags, ReservedTags.If.toString, condition, Nil, None, List(step.copy(withName = doStep)), Nil, Nil, Nil).withNodePath(sdPath)
        env.evaluate(evalStepDef(step, iStepDef, iStep, env)) {
          if (env.evaluateJSPredicate(env.interpolate(javascript)(env.getBoundReferenceValue))) {
            logger.info(s"Processing conditional step ($condition = true): ${step.keyword} $doStep")
            evalStepDef(step, iStepDef, iStep, env)
          } else {
            logger.info(s"Skipping conditional step ($condition = false): ${step.keyword} $doStep")
            step.copy(withEvalStatus = Passed(0))
          }
        }
      }

      case _ => null
    }
  }
  
  /**
    * Defines the default steps supported by all engines.
    *
    * @param step the step to evaluate
    * @param env the environment context
    * @throws gwen.Errors.UndefinedStepException if the given step is undefined
    *         or unsupported
    */
  override def evaluate(step: Step, env: T): Unit = {

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
        env.perform {
          Thread.sleep(duration.toLong * 1000)
        }
      
      case r"""I execute system process "(.+?)"$$$systemproc""" => step.orDocString(systemproc) tap { systemproc =>
        checkStepRules(step, BehaviorType.Action, env)
        env.perform {
          systemproc.! match {
            case 0 =>
            case _ => Errors.systemProcessError(s"The call to system process '$systemproc' has failed.")
          }
        }
      }

      case r"""I execute a unix system process "(.+?)"$$$systemproc""" => step.orDocString(systemproc) tap { systemproc =>
        checkStepRules(step, BehaviorType.Action, env)
        env.perform {
          Seq("/bin/sh", "-c", systemproc).! match {
            case 0 =>
            case _ => Errors.systemProcessError(s"The call to system process '$systemproc' has failed.")
          }
        }
      }

      case r"""I execute (?:javascript|js) "(.+?)$javascript"""" => step.orDocString(javascript) tap { javascript =>
        checkStepRules(step, BehaviorType.Action, env)
        env.evaluateJS(javascript)
      }


      case r"""I capture (.+?)$attribute by (?:javascript|js) "(.+?)"$$$expression""" => step.orDocString(expression) tap { expression =>
        checkStepRules(step, BehaviorType.Action, env)
        val value = Option(env.evaluateJS(env.formatJSReturn(env.interpolate(expression)(env.getBoundReferenceValue)))).map(_.toString).orNull
        env.topScope.set(attribute, value tap { content =>
          env.addAttachment(attribute, "txt", content)
        })
      }

      case r"""I capture the (text|node|nodeset)$targetType in (.+?)$source by xpath "(.+?)"$expression as (.+?)$$$name""" =>
        checkStepRules(step, BehaviorType.Action, env)
        val src = env.getBoundReferenceValue(source)
        val result = env.evaluateXPath(expression, src, env.XMLNodeType.withName(targetType)) tap { content =>
          env.addAttachment(name, "txt", content)
        }
        env.topScope.set(name, result)

      case r"""I capture the text in (.+?)$source by regex "(.+?)"$expression as (.+?)$$$name""" =>
        checkStepRules(step, BehaviorType.Action, env)
        val src = env.getBoundReferenceValue(source)
        val result = env.extractByRegex(expression, src) tap { content =>
          env.addAttachment(name, "txt", content)
        }
        env.topScope.set(name, result)

      case r"""I capture the content in (.+?)$source by json path "(.+?)"$expression as (.+?)$$$name""" =>
        checkStepRules(step, BehaviorType.Action, env)
        val src = env.getBoundReferenceValue(source)
        val result = env.evaluateJsonPath(expression, src) tap { content =>
          env.addAttachment(name, "txt", content)
        }
        env.topScope.set(name, result)

      case r"""I capture (.+?)$source as (.+?)$$$attribute""" =>
        checkStepRules(step, BehaviorType.Action, env)
        val value = env.getBoundReferenceValue(source)
        env.topScope.set(attribute, value tap { content =>
          env.addAttachment(attribute, "txt", content)
        })

      case r"""I capture (.+?)$$$attribute""" =>
        checkStepRules(step, BehaviorType.Action, env)
        val value = env.getBoundReferenceValue(attribute)
        env.topScope.set(attribute, value tap { content =>
          env.addAttachment(attribute, "txt", content)
        })

      case r"""I base64 decode (.+?)$attribute as (.+?)$$$name""" =>
        checkStepRules(step, BehaviorType.Action, env)
        val source = env.getBoundReferenceValue(attribute)
        val result = env.decodeBase64(source) tap { content =>
          env.addAttachment(name, "txt", content)
        }
        env.topScope.set(name, result)

      case r"""I base64 decode (.+?)$attribute""" =>
        checkStepRules(step, BehaviorType.Action, env)
        val source = env.getBoundReferenceValue(attribute)
        val result = env.decodeBase64(source) tap { content =>
          env.addAttachment(attribute, "txt", content)
        }
        env.topScope.set(attribute, result)

      case r"""(.+?)$attribute (?:is|will be) defined by (javascript|js|system process|property|setting|file)$attrType "(.+?)"$$$expression""" => step.orDocString(expression) tap { expression =>
        checkStepRules(step, BehaviorType.Context, env)
        attrType match {
          case "javascript" | "js" => env.scopes.set(s"$attribute/javascript", expression)
          case "system process" => env.scopes.set(s"$attribute/sysproc", expression)
          case "file" => env.scopes.set(s"$attribute/file", expression)
          case _ => env.topScope.set(attribute, Settings.get(expression))
        }
      }

      case r"""(.+?)$attribute (?:is|will be) defined by the (text|node|nodeset)$targetType in (.+?)$source by xpath "(.+?)"$$$expression""" => step.orDocString(expression) tap { expression =>
        checkStepRules(step, BehaviorType.Context, env)
        env.scopes.set(s"$attribute/xpath/source", source)
        env.scopes.set(s"$attribute/xpath/targetType", targetType)
        env.scopes.set(s"$attribute/xpath/expression", expression)
      }

      case r"""(.+?)$attribute (?:is|will be) defined in (.+?)$source by regex "(.+?)"$$$expression""" => step.orDocString(expression) tap { expression =>
        checkStepRules(step, BehaviorType.Context, env)
        env.scopes.set(s"$attribute/regex/source", source)
        env.scopes.set(s"$attribute/regex/expression", expression)
      }

      case r"""(.+?)$attribute (?:is|will be) defined in (.+?)$source by json path "(.+?)"$$$expression""" => step.orDocString(expression) tap { expression =>
        checkStepRules(step, BehaviorType.Context, env)
        env.scopes.set(s"$attribute/json path/source", source)
        env.scopes.set(s"$attribute/json path/expression", expression)
      }

      case r"""(.+?)$attribute (?:is|will be) defined by sql "(.+?)"$selectStmt in the (.+?)$dbName database""" =>
        checkStepRules(step, BehaviorType.Context, env)
        Settings.get(s"gwen.db.${dbName}.driver")
        Settings.get(s"gwen.db.${dbName}.url")
        env.scopes.set(s"$attribute/sql/selectStmt", selectStmt)
        env.scopes.set(s"$attribute/sql/dbName", dbName)

      case r"""(.+?)$attribute (?:is|will be) defined in the (.+?)$dbName database by sql "(.+?)"$$$selectStmt""" => step.orDocString(selectStmt) tap { selectStmt =>
        checkStepRules(step, BehaviorType.Context, env)
        Settings.get(s"gwen.db.${dbName}.driver")
        Settings.get(s"gwen.db.${dbName}.url")
        env.scopes.set(s"$attribute/sql/selectStmt", selectStmt)
        env.scopes.set(s"$attribute/sql/dbName", dbName)
      }

      case r"""I update the (.+?)$dbName database by sql "(.+?)"$$$updateStmt""" => step.orDocString(updateStmt) tap { updateStmt =>
        checkStepRules(step, BehaviorType.Action, env)
        Settings.get(s"gwen.db.${dbName}.driver")
        Settings.get(s"gwen.db.${dbName}.url")
        val rowsAffected = env.executeSQLUpdate(updateStmt, dbName)
        env.scopes.set(s"$dbName rows affected", rowsAffected.toString)
      }

      case r"""(.+?)$source at (json path|xpath)$matcher "(.+?)"$path should( not)?$negation (be|contain|start with|end with|match regex|match template|match template file)$operator "(.*?)"$$$expression""" => step.orDocString(expression) tap { expression =>
        checkStepRules(step, BehaviorType.Assertion, env)
        val expected = env.parseExpression(operator, expression)
        env.perform {
          val src = env.scopes.get(source)
          val actual = matcher match {
            case "json path" => env.evaluateJsonPath(path, src)
            case "xpath" => env.evaluateXPath(path, src, env.XMLNodeType.text)
          }
          val negate = Option(negation).isDefined
          val result = env.compare(s"$source at $matcher '$path'", expected, actual, operator, negate)
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
        val binding = env.getBinding(attribute)
        val actualValue = env.getBoundReferenceValue(binding)
        val expected = env.parseExpression(operator, expression)
        env.perform {
          val negate = Option(negation).isDefined
          val result = env.compare(attribute, expected, actualValue, operator, negate)
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
        env.perform {
          assert(Try(env.getBoundReferenceValue(attribute)).isFailure, s"Expected $attribute to be absent")
        }

      case r"""I attach "(.+?)"$filepath as "(.+?)"$$$name""" =>
        checkStepRules(step, BehaviorType.Action, env)
        val file = new File(filepath)
        if (!file.exists) { 
          Errors.fileAttachError(file, "not found")
        }
        env.perform {
          env.addAttachment(name, file)
        }
      
      case _ => Errors.undefinedStepError(step)
      
    }
  }
  
}