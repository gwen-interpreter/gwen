/*
 * Copyright 2015 Branko Juric, Brady Wood
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

import scala.sys.process.stringSeqToProcess
import scala.sys.process.stringToProcess
import gwen.Predefs.RegexContext
import gwen.dsl.Step
import gwen.eval.{EnvContext, EvalEngine}
import gwen.errors._
import gwen.Settings
import gwen.Predefs.Kestrel

/** Provides the common default steps that all engines can support. */
trait DefaultEngineSupport[T <: EnvContext] extends EvalEngine[T] {

  /**
    * Defines the default steps supported by all engines.
    *
    * @param step the step to evaluate
    * @param env the environment context
    * @throws gwen.errors.UndefinedStepException if the given step is undefined
    *         or unsupported
    */
  override def evaluate(step: Step, env: T): Unit = {
    step.expression match {

      case r"""my (.+?)$name (?:property|setting) (?:is|will be) "(.*?)"$$$value""" =>
        Settings.add(name, value)
        
      case r"""(.+?)$attribute (?:is|will be) "(.*?)"$$$value""" =>
        env.featureScope.set(attribute, value)

      case r"""I wait ([0-9]+?)$duration second(?:s?)""" => env.execute {
        Thread.sleep(duration.toLong * 1000)
      }
      
      case r"""I execute system process "(.+?)"$$$systemproc""" => env.execute {
        systemproc.! match {
          case 0 => 
          case _ => systemProcessError(s"The call to $systemproc has failed.")
        }
      }
      case r"""I execute a unix system process "(.+?)"$$$systemproc""" => env.execute {
        Seq("/bin/sh", "-c", systemproc).! match {
          case 0 => 
          case _ => systemProcessError(s"The call to $systemproc has failed.")
        }
      }

      case r"""I capture the (text|node|nodeset)$targetType in (.+?)$source by xpath "(.+?)"$expression as (.+?)$$$name""" =>
        val src = env.getBoundReferenceValue(source)
        env.featureScope.set(name, env.execute(env.evaluateXPath(expression, src, env.XMLNodeType.withName(targetType)) tap { content =>
          env.addAttachment(name, "txt", content)
        }).getOrElse(s"$$[xpath:$expression]"))

      case r"""I capture the text in (.+?)$source by regex "(.+?)"$expression as (.+?)$$$name""" =>
        val src = env.getBoundReferenceValue(source)
        env.featureScope.set(name, env.execute(env.extractByRegex(expression, src) tap { content =>
          env.addAttachment(name, "txt", content)
        }).getOrElse(s"$$[regex:$expression"))

      case r"""I capture the content in (.+?)$source by json path "(.+?)"$expression as (.+?)$$$name""" =>
        val src = env.getBoundReferenceValue(source)
        env.featureScope.set(name, env.execute(env.evaluateJsonPath(expression, src) tap { content =>
          env.addAttachment(name, "txt", content)
        }).getOrElse(s"$$[json path:$expression"))

      case r"""I capture (.+?)$source as (.+?)$attribute""" =>
        val value = env.getBoundReferenceValue(source)
        env.featureScope.set(attribute, value tap { content =>
          env.addAttachment(attribute, "txt", content)
        })

      case r"""I capture (.+?)$$$attribute""" =>
        val value = env.getBoundReferenceValue(attribute)
        env.featureScope.set(attribute, value tap { content =>
          env.addAttachment(attribute, "txt", content)
        })

      case r"""I base64 decode (.+?)$attribute as (.+?)$$$name""" =>
        val source = env.getBoundReferenceValue(attribute)
        env.featureScope.set(name, env.execute(env.decodeBase64(source) tap { content =>
          env.addAttachment(name, "txt", content)
        }).getOrElse(s"$$[base64 decoded $attribute]"))

      case r"""I base64 decode (.+?)$attribute""" =>
        val source = env.getBoundReferenceValue(attribute)
        env.featureScope.set(attribute, env.execute(env.decodeBase64(source) tap { content =>
          env.addAttachment(attribute, "txt", content)
        }).getOrElse(s"$$[base64 decoded $attribute]"))

      case r"""(.+?)$attribute (?:is|will be) defined by (javascript|system process|property|setting|file)$attrType "(.+?)"$$$expression""" =>
        attrType match {
          case "javascript" => env.activeScope.set(s"$attribute/javascript", expression)
          case "system process" => env.activeScope.set(s"$attribute/sysproc", expression)
          case "file" => env.activeScope.set(s"$attribute/file", expression)
          case _ => env.featureScope.set(attribute, Settings.get(expression))
        }

      case r"""(.+?)$attribute (?:is|will be) defined by the (text|node|nodeset)$targetType in (.+?)$source by xpath "(.+?)"$$$expression""" =>
        env.activeScope.set(s"$attribute/xpath/source", source)
        env.activeScope.set(s"$attribute/xpath/targetType", targetType)
        env.activeScope.set(s"$attribute/xpath/expression", expression)

      case r"""(.+?)$attribute (?:is|will be) defined in (.+?)$source by regex "(.+?)"$$$expression""" =>
        env.activeScope.set(s"$attribute/regex/source", source)
        env.activeScope.set(s"$attribute/regex/expression", expression)

      case r"""(.+?)$attribute (?:is|will be) defined in (.+?)$source by json path "(.+?)"$$$expression""" =>
        env.activeScope.set(s"$attribute/json path/source", source)
        env.activeScope.set(s"$attribute/json path/expression", expression)

      case r"""(.+?)$attribute (?:is|will be) defined by sql "(.+?)"$selectStmt in the (.+?)$dbName database""" =>
        env.activeScope.set(s"$attribute/sql/selectStmt", selectStmt)
        env.activeScope.set(s"$attribute/sql/dbName", dbName)

      case r"""(.+?)$source at (json path|xpath)$matcher "(.+?)"$path should( not)?$negation (be|contain|start with|end with|match regex)$operator "(.*?)"$$$expression""" =>
        val src = env.activeScope.get(source)
        env.execute {
          val actual = matcher match {
            case "json path" => env.evaluateJsonPath(path, src)
            case "xpath" => env.evaluateXPath(path, src, env.XMLNodeType.text)
          }
          val negate = Option(negation).isDefined
          val result = env.compare(expression, actual, operator, negate)
          assert(result, s"Expected $source at $matcher '$path' to ${if(negate) "not " else ""}$operator '$expression' but got '$actual'")
        }

      case r"""(.+?)$attribute should( not)?$negation (be|contain|start with|end with|match regex|match xpath|match json path)$operator "(.*?)"$$$expression""" =>
        val actualValue = env.getBoundReferenceValue(attribute)
        env.execute {
          val negate = Option(negation).isDefined
          val result = env.compare(expression, actualValue, operator, negate)
          assert(result, s"Expected $attribute to ${if(negate) "not " else ""}$operator '$expression' but got '$actualValue'")
        }

      case r"""(.+?)$attribute should be absent""" => env.execute {
        assert(env.activeScope.getOpt(attribute).isEmpty, s"Expected $attribute to be absent")
      }

      case r"""(.+?)$doStep for each data table record""" =>

        //val binding = env.getLocatorBinding(iteration)
        //foreach(() => env.locateAll(env, binding), element, step, doStep, env)
      
      case _ => undefinedStepError(step)
      
    }
  }
  
}