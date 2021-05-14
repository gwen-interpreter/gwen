/*
 * Copyright 2021 Branko Juric, Brady Wood
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

package gwen.report.portal

import RPConfig._
import gwen._
import gwen.dsl._
import gwen.eval.FeatureResult
import gwen.eval.FeatureUnit
import gwen.eval.LifecycleEvent
import gwen.eval.LifecycleEventListener
import gwen.eval.ScopedDataStack
import gwen.report.RPReportConfig

import scala.concurrent.duration.Duration
import scala.io.Source

import com.typesafe.scalalogging.LazyLogging
import com.epam.reportportal.listeners.LogLevel

import java.io.File
import java.{util => ju}

class RPReporter(rpClient: RPClient) 
    extends LifecycleEventListener("Report Portal Reporter", RPConfig.bypassNodeTypes) 
    with LazyLogging {

  override def beforeUnit(event: LifecycleEvent[FeatureUnit]): Unit = { 
    val unit = event.source
    val nodeType = unit.nodeType
    val name = unit.featureFile.getPath
    rpClient.startItem(
      event.time, nodeType, false, name, "", Nil, Map(), None, unit.uuid, None
    )
  } 

  override def afterUnit(event: LifecycleEvent[FeatureUnit]): Unit = {
    val unit = event.source
    val evalStatus = unit.result.map(_.spec.evalStatus).getOrElse(Skipped)
    rpClient.finishItem(event.time, "", event.parentUuid, evalStatus)
  }
    
  override def beforeFeature(event: LifecycleEvent[FeatureSpec]): Unit = {
    val spec = event.source
    val feature = spec.feature
    val nodeType = feature.nodeType
    val name = s"${spec.specType}: ${feature.name}"
    val desc = formatDescription(feature)
    val tags = parseTags(feature.tags)
    val sourceRef = feature.sourceRef
    val breadcrumbs = breadcrumbAtts(sourceRef, event.callTrail, event.scopes)
    val atts = Map("language" -> feature.language) ++ breadcrumbs
    rpClient.startItem(
      event.time, nodeType, false, name, desc, tags, atts, sourceRef, spec.uuid, event.parentUuid
    )
  }

  override def afterFeature(event: LifecycleEvent[FeatureResult]): Unit = {
    val result = event.source
    val evalStatus = result.spec.evalStatus
    rpClient.finishItem(event.time, "", event.parentUuid, evalStatus)
  }

  override def beforeBackground(event: LifecycleEvent[Background]): Unit = {
    val background = event.source
    val nodeType = background.nodeType
    val name = s"${background.keyword}: ${background.name}"
    val desc = formatDescription(background)
    val sourceRef = background.sourceRef
    val inlined = isInlined(background, event.callTrail)
    rpClient.startItem(
      event.time, nodeType, inlined, name, desc, Nil, Map(), sourceRef, background.uuid, event.parentUuid
    )
  }

  override def afterBackground(event: LifecycleEvent[Background]): Unit = {
    val background = event.source
    val evalStatus = background.evalStatus
    val desc = formatDescriptionWithError(background, event.callTrail)
    rpClient.finishItem(event.time, desc, event.parentUuid, evalStatus)
  }

  override def beforeScenario(event: LifecycleEvent[Scenario]): Unit = {
    val scenario = event.source
    val inlined = isInlined(scenario, event.callTrail)
    beforeScenario(event.time, scenario, event.parentUuid, event.callTrail, event.scopes, inlined)
  }

  private def beforeScenario(startTime: ju.Date, scenario: Scenario, parentUuid: String, callTrail: List[Step], scopes: ScopedDataStack, inlined: Boolean): Unit = {
    val nodeType = scenario.nodeType
    val name = s"${scenario.keyword}: ${scenario.name}"
    val desc = formatDescription(scenario)
    val tags = parseTags(scenario.tags)
    val sourceRef = scenario.sourceRef
    val atts = breadcrumbAtts(sourceRef, callTrail, scopes)
    rpClient.startItem(
      startTime, nodeType, inlined, name, desc, tags, atts, sourceRef, scenario.uuid, parentUuid
    )
  }

  override def afterScenario(event: LifecycleEvent[Scenario]): Unit = {
    afterScenario(event.time, event.source, event.parentUuid, event.callTrail)
  }

  private def afterScenario(endTime: ju.Date, scenario: Scenario, parentUuid: String, callTrail: List[Step]): Unit = {
    val evalStatus = scenario.evalStatus
    val desc = formatDescriptionWithError(scenario, callTrail)
    rpClient.finishItem(endTime, desc, parentUuid, evalStatus)
  }

  override def beforeExamples(event: LifecycleEvent[Examples]): Unit = {
    val examples = event.source
    val inlined = isInlined(examples, event.callTrail)
    beforeExamples(event.time, examples, event.parentUuid, event.callTrail, inlined)
  }

  private def beforeExamples(startTime: ju.Date, examples: Examples, parentUuid: String, callTrail: List[Step], inlined: Boolean) = {
    val nodeType = examples.nodeType
    val name = s"${examples.keyword}: ${examples.name}"
    val desc = formatDescription(examples)
    val tags = parseTags(examples.tags)
    val sourceRef = examples.sourceRef
    rpClient.startItem(
      startTime, nodeType, inlined, name, desc, tags, Map(), sourceRef, examples.uuid, parentUuid
    )
    if (examples.table.nonEmpty) {
      rpClient.sendItemLog(LogLevel.INFO, Formatting.formatTable(examples.table))
    }
  }

  override def afterExamples(event: LifecycleEvent[Examples]): Unit = {
    afterExamples(event.time, event.source, event.parentUuid, event.callTrail)
  }

  private def afterExamples(endTime: ju.Date, examples: Examples, parentUuid: String, callTrail: List[Step]): Unit = {
    val evalStatus = examples.evalStatus
    val desc = formatDescriptionWithError(examples, callTrail)
    rpClient.finishItem(endTime, desc, parentUuid, evalStatus)
  }

  override def beforeRule(event: LifecycleEvent[Rule]): Unit = {
    val rule = event.source
    val nodeType = rule.nodeType
    val name = s"${rule.keyword}: ${rule.name}"
    val desc = formatDescription(rule)
    val sourceRef = rule.sourceRef
    val atts = breadcrumbAtts(sourceRef, event.callTrail, event.scopes)
    rpClient.startItem(
      event.time, nodeType, false, name, desc, Nil, atts, sourceRef, rule.uuid, event.parentUuid
    )
  }

  override def afterRule(event: LifecycleEvent[Rule]): Unit = {
    val rule = event.source
    val evalStatus = rule.evalStatus
    rpClient.finishItem(event.time, "", event.parentUuid, evalStatus)
  }

  override def beforeStepDef(event: LifecycleEvent[Scenario]): Unit = {
    val stepDef = event.source
    val inlined = isInlined(stepDef, event.callTrail)
    beforeStepDef(event.time, stepDef, event.parentUuid, inlined)
  }

  private def beforeStepDef(startTime: ju.Date, stepDef: Scenario, parentUuid: String, inlined: Boolean): Unit = {
    val nodeType = stepDef.nodeType
    val name = s"${if (stepDef.isForEach) "ForEach" else stepDef.keyword}: ${stepDef.name}"
    val desc = formatDescription(stepDef)
    val tags = parseTags(stepDef.tags)
    val sourceRef = stepDef.sourceRef
    rpClient.startItem(
      startTime, nodeType, inlined, name, desc, tags, Map(), sourceRef, stepDef.uuid, parentUuid
    )
  }

  override def afterStepDef(event: LifecycleEvent[Scenario]): Unit = {
    afterStepDef(event.time, event.source, event.parentUuid, event.callTrail)
  }

  private def afterStepDef(endTime: ju.Date, stepDef: Scenario, parentUuid: String, callTrail: List[Step]): Unit = {
    val evalStatus = stepDef.evalStatus
    val desc = formatDescriptionWithError(stepDef, callTrail)
    rpClient.finishItem(endTime, desc, parentUuid, evalStatus)
  }

  override def beforeStep(event: LifecycleEvent[Step]): Unit = {
    val step = event.source
    val inlined = isInlined(step, event.callTrail)
    beforeStep(event.time, step, event.parentUuid, event.callTrail, event.scopes, inlined)
  }

  private def beforeStep(startTime: ju.Date, step: Step, parentUuid: String, callTrail: List[Step], scopes: ScopedDataStack, inlined: Boolean) = {
    val nodeType = step.nodeType
    val name = s"${step.keyword} ${step.name}"
    val desc = formatDescription(step)
    val sourceRef = step.sourceRef
    val breadcrumbs = breadcrumbAtts(sourceRef, callTrail, scopes)
    val atts = if (breadcrumbs.nonEmpty) { breadcrumbs ++ Map("step" -> step.name) } else breadcrumbs
    rpClient.startItem(
      startTime, nodeType, inlined, name, desc, Nil, atts, sourceRef, step.uuid, parentUuid
    )
    step.docString foreach { docString =>
      rpClient.sendItemLog(LogLevel.INFO, Formatting.formatDocString(docString))
    }
    if (step.table.nonEmpty) {
      rpClient.sendItemLog(LogLevel.INFO, Formatting.formatTable(step.table))
    }
  }

  override def afterStep(event: LifecycleEvent[Step]): Unit = {
    afterStep(event.time, event.source, event.parentUuid, event.callTrail, event.scopes)
  }

  private def afterStep(endTime: ju.Date, step: Step, parentUuid: String, callTrail: List[Step], scopes: ScopedDataStack): Unit = {
    val evalStatus = step.evalStatus
    if (SendStepDefs.isNone && !SendFailedStepDefs.isNone && EvalStatus.isError(evalStatus.status)) {
      injectErrorTrail(endTime, step, parentUuid, callTrail, scopes)
    }
    logNonErrorAttachments(step)
    if (isLeafNode(step)) {
      if (EvalStatus.isError(step.evalStatus.status)) {
        step.errorTrails foreach { errorTrail =>
          logFailedStepResult(callTrail, errorTrail)
        }
      } else { 
        logNonErrorMessage(step)
      }
    }
    val desc = formatDescriptionWithError(step, callTrail)
    rpClient.finishItem(endTime, desc, parentUuid, evalStatus)
  }

  override def healthCheck(event: LifecycleEvent[Step]): Unit = { 
    if (RPSettings.`gwen.rp.heartbeat`) {
      rpClient.healthCheck(RPSettings.`gwen.rp.heartbeat.timeoutSecs`)
    }
  }

  private def isLeafNode(node: SpecNode): Boolean = {
    (SendStepDefs.isNone && SendFailedStepDefs.isNone) || (node match {
      case step: Step => step.stepDef.isEmpty
      case _ => false
    })
  }

  private def logNonErrorAttachments(step: Step): Unit = {
    val steps = {
      if (SendStepDefs.isNone && (SendFailedStepDefs.isNone || !EvalStatus.isError(step.evalStatus.status))) {
        step.deepSteps
      } else {
        List(step)
      }
    }
    steps filter { 
      s => !EvalStatus.isError(s.evalStatus.status)
    } foreach { s => 
      rpClient.sendAttachmentLogs(s.evalStatus, s.attachments)
    }
  }

  private def logNonErrorMessage(step: Step): Unit = {
    val evalStatus = step.evalStatus
    val message = {
      if (evalStatus == Disabled) Some(Disabled.status.toString)
      else if (evalStatus.cause.nonEmpty) Some(evalStatus.message)
      else None
    }
    message foreach { msg => 
      rpClient.sendItemLog(evalStatus, msg, None) 
    }
  }

  private def logFailedStepResult(callTrail: List[Step], errorTrail: List[Step]): Unit = {
    val failedStep = errorTrail.last
    val evalStatus = failedStep.evalStatus
    val attachments = failedStep.attachments
    val screenshotAttachment = attachments find { case (name, _) => name == "Screenshot" }
    val errorAttachment = attachments find { case (name, _) => name == "Error details" }
    val envAttachment = attachments find { case (name, _) => name == "Environment" }
    val message = if (SendErrorTrace.isInlined || SendEnvTrace.isInlined) {
      val errorTrace = if (SendErrorTrace.isInlined) {
        errorAttachment map { case (name, file) =>
          s"\r\n\r\n$name:\r\n\r\n${Source.fromFile(file).mkString}"
        } getOrElse ""
      } else ""
      val envTrace = if (SendEnvTrace.isInlined) { 
        envAttachment map { case (name, file) =>
          s"\r\n\r\n$name:\r\n\r\n${Source.fromFile(file).mkString}"
        } getOrElse ""
      } else ""
      s"${errorMessage(callTrail, errorTrail)}$errorTrace$envTrace"
    } else {
      errorMessage(callTrail, errorTrail)
    }
    rpClient.sendItemLog(evalStatus, message, screenshotAttachment map { case (_, file) => file } )
    if (SendHierarchy.isAttached) {
      errorHierarchy(callTrail, errorTrail).foreach { hierarchy =>
        val file = File.createTempFile(s"Hierarchy-", ".txt")
        file.deleteOnExit()
        file.writeText(hierarchy)
        val attachment = ("Hierarchy", file)
        rpClient.sendAttachmentLog(evalStatus, attachment)
      }
    }
    if (SendErrorTrace.isAttached) {
      rpClient.sendAttachmentLogs(evalStatus, errorAttachment.toList)
    }
    if (SendEnvTrace.isAttached) {
      rpClient.sendAttachmentLogs(evalStatus, envAttachment.toList)
    }
  }

  def injectErrorTrail(endTime: ju.Date, step: Step, parentUuid: String, callTrail: List[Step], scopes: ScopedDataStack): Unit = {

    def inject(startTime: ju.Date, scenario: Scenario, parentUuid: String, callTrail: List[Step]): ju.Date = {
      var time = startTime
      if (scenario.isStepDef) {
        beforeStepDef(time, scenario, parentUuid, SendFailedStepDefs.isInlined)
      } else {
        beforeScenario(time, scenario, parentUuid, callTrail, scopes, SendFailedStepDefs.isInlined)
      }
      scenario.steps foreach { step => 
        val sCallTrail = if (callTrail.lastOption.map(_.name != step.name).getOrElse(true)) callTrail ++ List(step) else callTrail
        beforeStep(time, step, scenario.uuid, sCallTrail, scopes, SendFailedStepDefs.isInlined)
        time = new ju.Date(time.getTime + durationMsecs(step.evalStatus.duration))
        afterStep(time, step, step.uuid, sCallTrail, scopes)
        
      }
      scenario.examples foreach { examples => 
        beforeExamples(time, examples, scenario.uuid, callTrail, SendFailedStepDefs.isInlined)
        examples.scenarios foreach { scenario => 
          time = inject(time, scenario, examples.uuid, callTrail)
        }
        time = new ju.Date(time.getTime + durationMsecs(examples.evalStatus.duration))
        afterExamples(time, examples, examples.uuid, callTrail)
      }
      if (scenario.isStepDef) {
        afterStepDef(time, scenario, scenario.uuid, callTrail)
      } else {
        afterScenario(time, scenario, scenario.uuid, callTrail)
      }
      time
    }

    def durationMsecs(duration: Duration): Long = {
      val msecs = step.evalStatus.duration.toMillis
      if (msecs < 1) 1L else msecs
    }

    step.stepDef foreach { case (stepDef, _) => 
      val startTime = new ju.Date(endTime.getTime - stepDef.steps.map(_.evalStatus.duration.toMillis).sum)
      inject(startTime, stepDef, parentUuid, callTrail)
    }

  }

  private def isInlined(node: SpecNode, callTrail: List[Step]): Boolean = { 
    SendStepDefs.isInlined && callTrail.size > (if (node.isInstanceOf[Step]) 1 else 0)
  }

  private def formatDescriptionWithError(node: SpecNode, callTrail: List[Step]): String = {
    val desc = formatDescription(node)
    val evalStatus = node.evalStatus
      if (EvalStatus.isError(evalStatus.status) && callTrail.size > 0) {
      val errors = {
        if (AppendErrorBlocks.all || (AppendErrorBlocks.leaf && isLeafNode(node))) {
          errorMessages(callTrail, Step.errorTrails(node)) match {
            case Nil => s"```error\r\n${evalStatus.message}\r\n```"
            case msgs => msgs.map(msg => s"```error\r\n$msg\r\n```").mkString("\r\n") 
          }
        } else {
          ""
        }
      }
      s"${if (desc.size > 0) s"$desc\r\n\r\n" else ""}$errors"
    } else desc
  }

  private def formatDescription(node: SpecNode): String = {
    node match {
      case f: Feature => f.description.mkString(" ")
      case b: Background => b.description.mkString(" ")
      case r: Rule => r.description.mkString(" ")
      case s: Scenario => s.description.mkString(" ")
      case e: Examples => 
        val examplesDesc = Option(e.description.mkString(" "))
        val tableDesc = e.table match {
          case Nil => None
          case table => Some(s"<pre>${Formatting.formatTable(table)}</pre>")
        }
        (examplesDesc ++ tableDesc).mkString("<br><br>")
      case s: Step => 
        s.table match {
          case Nil => 
            s.docString.map(ds => s"<pre>${Formatting.formatDocString(ds)}</pre>").getOrElse("")
          case table => s"<pre>${Formatting.formatTable(table)}</pre>"
        }
      case _ => ""
    }
  }

  private def errorMessages(callTrail: List[Step], errorTrails: List[List[Step]]): List[String] = {
    errorTrails.map(errorTrail => errorMessage(callTrail, errorTrail))
  }

  private def errorMessage(callTrail: List[Step], errorTrail: List[Step]): String = {
    val message = errorTrail.last.evalStatus.message
    val hierarchy = {
      if (SendHierarchy.isInlined) errorHierarchy(callTrail, errorTrail).map(h => s"\r\n\r\n$h").getOrElse("") 
      else ""
    }
    s"${message}${hierarchy}"
  }

  private def errorHierarchy(callTrail: List[Step], errorTrail: List[Step]): Option[String] = {
    if (isLeafNode(errorTrail.head)) {
      val callChain = callTrail ++ errorTrail.tail
      val hierarchy = if (callChain.size > 1) callChain.filter(s => !s.isVirtual).map(_.name) else Nil
      if (hierarchy.nonEmpty) Some(s"Hierarchy:\r\n\r\n${hierarchy.mkString("\r\n")}") else None
    } else None
  }

  private val breadcrumbBindings = List(
    ("gwen.feature.name" -> "feature"), 
    ("gwen.rule.name" -> "rule"), 
    ("gwen.scenario.name" -> "scenario")
  )

  private def breadcrumbAtts[T](sourceRef: Option[SourceRef], callTrail: List[Step], scopes: ScopedDataStack): Map[String, String] = {
    val isFeature = sourceRef.map(_.isFeature).getOrElse(false)
    if (RPSettings.`gwen.rp.send.breadcrumbs` && isFeature && callTrail.size < 2) {
      breadcrumbBindings flatMap { case (bindingName, name) =>
        scopes.getOpt(bindingName) map { value => (name -> value) }
      } toMap
    } else {
      Map()
    }
  }

  private def parseTags(tags: List[Tag]): List[Tag] = {
    if (RPSettings.`gwen.rp.send.tags`) tags else Nil
  }

  def close(evalStatus: EvalStatus): Unit = {
    rpClient.close(evalStatus) foreach { url =>
      logger.info(s"${RPReportConfig.name} launch report generated: $url")
    }
  }
  
}
