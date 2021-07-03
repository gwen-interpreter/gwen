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

package gwen.core.report.rp

import RPConfig._
import gwen._
import gwen.core._
import gwen.core.node._
import gwen.core.node.gherkin._
import gwen.core.node.event.NodeEvent
import gwen.core.node.event.NodeEventListener
import gwen.core.result.SpecResult
import gwen.core.state.ScopedDataStack
import gwen.core.status.Disabled
import gwen.core.status.EvalStatus
import gwen.core.status.Skipped

import scala.concurrent.duration.Duration
import scala.io.Source

import com.typesafe.scalalogging.LazyLogging
import com.epam.reportportal.listeners.LogLevel

import java.io.File
import java.{util => ju}

class RPReporter(rpClient: RPClient) 
    extends NodeEventListener("Report Portal Reporter", RPConfig.bypassNodeTypes) 
    with LazyLogging {

  override def beforeUnit(event: NodeEvent[FeatureUnit]): Unit = { 
    val unit = event.source
    val name = unit.name
    val callChain = event.callChain
    val nodePath = callChain.nodePath
    rpClient.startItem(event.time, None, unit, nodePath, name, "", Nil, Map(), Nil, false)
  } 

  override def afterUnit(event: NodeEvent[FeatureUnit]): Unit = {
    val unit = event.source
    val callChain = event.callChain
    val parent = callChain.last
    val evalStatus = unit.result.map(_.spec.evalStatus).getOrElse(Skipped)
    rpClient.finishItem(event.time, "", parent, evalStatus)
  }
    
  override def beforeSpec(event: NodeEvent[Spec]): Unit = {
    val spec = event.source
    val feature = spec.feature
    val name = s"${spec.specType}: ${feature.name}"
    val desc = formatDescription(feature)
    val tags = filterTags(feature.tags)
    val callChain = event.callChain
    val parent = callChain.previous
    val nodePath = callChain.nodePath
    val breadcrumbs = breadcrumbAtts(feature.sourceRef, callChain.steps, event.scopes)
    val atts = Map("language" -> feature.language) ++ breadcrumbs
    rpClient.startItem(event.time, Some(parent), spec, nodePath, name, desc, tags, atts, Nil, false)
  }

  override def afterSpec(event: NodeEvent[SpecResult]): Unit = {
    val result = event.source
    val evalStatus = result.spec.evalStatus
    val callChain = event.callChain
    val parent = callChain.last
    rpClient.finishItem(event.time, "", parent, evalStatus)
  }

  override def beforeBackground(event: NodeEvent[Background]): Unit = {
    val background = event.source
    val name = s"${background.keyword}: ${background.name}"
    val desc = formatDescription(background)
    val callChain = event.callChain
    val parent = callChain.previous
    val nodePath = callChain.nodePath
    val inlined = isInlined(background, callChain)
    rpClient.startItem(
      event.time, Some(parent), background, nodePath, name, desc, Nil, Map(), Nil, inlined
    )
  }

  override def afterBackground(event: NodeEvent[Background]): Unit = {
    val background = event.source
    val evalStatus = background.evalStatus
    val callChain = event.callChain
    val parent = callChain.last
    val desc = formatDescriptionAndMore(background, callChain)
    rpClient.finishItem(event.time, desc, parent, evalStatus)
  }

  override def beforeScenario(event: NodeEvent[Scenario]): Unit = {
    val scenario = event.source
    val callChain = event.callChain
    val inlined = isInlined(scenario, callChain)
    beforeScenario(event.time, scenario, callChain, event.scopes, inlined)
  }

  private def beforeScenario(startTime: ju.Date, scenario: Scenario, callChain: NodeChain, scopes: ScopedDataStack, inlined: Boolean): Unit = {
    val name = s"${scenario.keyword}: ${scenario.name}"
    val desc = formatDescription(scenario)
    val tags = filterTags(scenario.tags)
    val params = scenario.cumulativeParams
    val atts = breadcrumbAtts(scenario.sourceRef, callChain.steps, scopes)
    val parent = callChain.previous
    val nodePath = callChain.nodePath
    rpClient.startItem(startTime, Some(parent), scenario, nodePath, name, desc, tags, atts, params, inlined)
  }

  override def afterScenario(event: NodeEvent[Scenario]): Unit = {
    afterScenario(event.time, event.source, event.callChain)
  }

  private def afterScenario(endTime: ju.Date, scenario: Scenario, callChain: NodeChain): Unit = {
    val evalStatus = scenario.evalStatus
    val desc = formatDescriptionAndMore(scenario, callChain)
    val parent = callChain.last
    rpClient.finishItem(endTime, desc, parent, evalStatus)
  }

  override def beforeExamples(event: NodeEvent[Examples]): Unit = {
    val examples = event.source
    val callChain = event.callChain
    val inlined = isInlined(examples, callChain)
    beforeExamples(event.time, examples, callChain, inlined, event.scopes)
  }

  private def beforeExamples(startTime: ju.Date, examples: Examples, callChain: NodeChain, inlined: Boolean, scopes: ScopedDataStack) = {
    val parent = callChain.previous
    val name = s"${examples.keyword}: ${examples.name}"
    val desc = formatDescription(examples)
    val tags = filterTags(examples.tags)
    rpClient.startItem(startTime, Some(parent), examples, callChain.nodePath, name, desc, tags, Map(), Nil, inlined)
    if (examples.table.nonEmpty) {
      rpClient.sendItemLog(LogLevel.INFO, Formatting.formatTable(examples.table))
    }
  }

  override def afterExamples(event: NodeEvent[Examples]): Unit = {
    afterExamples(event.time, event.source, event.callChain)
  }

  private def afterExamples(endTime: ju.Date, examples: Examples, callChain: NodeChain): Unit = {
    val evalStatus = examples.evalStatus
    val desc = formatDescriptionAndMore(examples, callChain)
    val parent = callChain.last
    rpClient.finishItem(endTime, desc, parent, evalStatus)
  }

  override def beforeRule(event: NodeEvent[Rule]): Unit = {
    val rule = event.source
    val name = s"${rule.keyword}: ${rule.name}"
    val desc = formatDescription(rule)
    val callChain = event.callChain
    val parent = callChain.previous
    val nodePath = callChain.nodePath
    val atts = breadcrumbAtts(rule.sourceRef, callChain.steps, event.scopes)
    rpClient.startItem(event.time, Some(parent), rule, nodePath, name, desc, Nil, atts, Nil, false)
  }

  override def afterRule(event: NodeEvent[Rule]): Unit = {
    val rule = event.source
    val evalStatus = rule.evalStatus
    val callChain = event.callChain
    val parent = callChain.last
    rpClient.finishItem(event.time, "", parent, evalStatus)
  }

  override def beforeStepDef(event: NodeEvent[Scenario]): Unit = {
    val stepDef = event.source
    val callChain = event.callChain
    val inlined = isInlined(stepDef, callChain)
    beforeStepDef(event.time, stepDef, callChain, inlined, event.scopes)
  }

  private def beforeStepDef(startTime: ju.Date, stepDef: Scenario, callChain: NodeChain, inlined: Boolean, scopes: ScopedDataStack): Unit = {
    val name = s"${if (stepDef.isForEach) "ForEach" else stepDef.keyword}: ${stepDef.name}"
    val desc = formatDescription(stepDef)
    val tags = filterTags(stepDef.tags)
    val params = stepDef.cumulativeParams
    val parent = callChain.previous
    val nodePath = callChain.nodePath
    rpClient.startItem(startTime, Some(parent), stepDef, nodePath, name, desc, tags, Map(), params, inlined)
  }

  override def afterStepDef(event: NodeEvent[Scenario]): Unit = {
    afterStepDef(event.time, event.source, event.callChain)
  }

  private def afterStepDef(endTime: ju.Date, stepDef: Scenario, callChain: NodeChain): Unit = {
    val evalStatus = stepDef.evalStatus
    val parent = callChain.last
    val desc = formatDescriptionAndMore(stepDef, callChain)
    rpClient.finishItem(endTime, desc, parent, evalStatus)
  }

  override def beforeStep(event: NodeEvent[Step]): Unit = {
    val step = event.source
    val callChain = event.callChain
    val inlined = isInlined(step, callChain)
    beforeStep(event.time, step, callChain, event.scopes, inlined)
  }

  private def beforeStep(startTime: ju.Date, step: Step, callChain: NodeChain, scopes: ScopedDataStack, inlined: Boolean) = {
    val name = s"${step.keyword} ${step.name}"
    val desc = formatDescription(step)
    val parent = callChain.previous
    val nodePath = callChain.nodePath
    val breadcrumbs = breadcrumbAtts(step.sourceRef, callChain.steps, scopes)
    val atts = if (breadcrumbs.nonEmpty) { breadcrumbs ++ Map("step" -> step.name) } else breadcrumbs
    val params = step.cumulativeParams
    rpClient.startItem(startTime, Some(parent), step, nodePath, name, desc, Nil, atts, params, inlined)
    step.docString foreach { docString =>
      rpClient.sendItemLog(LogLevel.INFO, Formatting.formatDocString(docString))
    }
    if (step.table.nonEmpty) {
      rpClient.sendItemLog(LogLevel.INFO, Formatting.formatTable(step.table))
    }
  }

  override def afterStep(event: NodeEvent[Step]): Unit = {
    afterStep(event.time, event.source, event.callChain, event.scopes)
  }

  private def afterStep(endTime: ju.Date, step: Step, callChain: NodeChain, scopes: ScopedDataStack): Unit = {
    val evalStatus = step.evalStatus
    if (SendStepDefs.isNone && !SendFailedStepDefs.isNone && evalStatus.isError) {
      injectErrorTrail(endTime, step, callChain, scopes)
    }
    logNonErrorAttachments(step)
    if (isLeafNode(step)) {
      if (step.evalStatus.isError) {
        step.errorTrails foreach { errorTrail =>
          logFailedStepResult(callChain.steps, errorTrail)
        }
      } else { 
        logNonErrorMessage(step)
      }
    }
    val desc = formatDescriptionAndMore(step, callChain)
    val parent = callChain.last
    rpClient.finishItem(endTime, desc, parent, evalStatus)
  }

  override def healthCheck(event: NodeEvent[Step]): Unit = { 
    if (RPSettings.`gwen.rp.heartbeat`) {
      rpClient.healthCheck(RPSettings.`gwen.rp.heartbeat.timeoutSecs`)
    }
  }

  private def isLeafNode(node: GwenNode): Boolean = {
    (SendStepDefs.isNone && SendFailedStepDefs.isNone) || (node match {
      case step: Step => step.stepDef.isEmpty
      case _ => false
    })
  }

  private def logNonErrorAttachments(step: Step): Unit = {
    val steps = {
      if (SendStepDefs.isNone && (SendFailedStepDefs.isNone || !step.evalStatus.isError)) {
        step.deepSteps
      } else {
        List(step)
      }
    }
    steps filter { 
      s => !s.evalStatus.isError
    } foreach { s => 
      rpClient.sendAttachmentLogs(s.evalStatus, s.attachments)
    }
  }

  private def logNonErrorMessage(step: Step): Unit = {
    val evalStatus = step.evalStatus
    val message = {
      if (evalStatus == Disabled) Some(Disabled.keyword.toString)
      else if (evalStatus.cause.nonEmpty) Some(evalStatus.message)
      else None
    }
    message foreach { msg => 
      rpClient.sendItemLog(evalStatus, msg, None) 
    }
  }

  private def logFailedStepResult(callSteps: List[Step], errorTrail: List[Step]): Unit = {
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
      s"${errorMessage(callSteps, errorTrail)}$errorTrace$envTrace"
    } else {
      errorMessage(callSteps, errorTrail)
    }
    rpClient.sendItemLog(evalStatus, message, screenshotAttachment map { case (_, file) => file } )
    if (SendHierarchy.isAttached) {
      errorHierarchy(callSteps, errorTrail).foreach { hierarchy =>
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

  def injectErrorTrail(endTime: ju.Date, step: Step, callChain: NodeChain, scopes: ScopedDataStack): Unit = {

    def inject(startTime: ju.Date, scenario: Scenario, callChain: NodeChain): ju.Date = {
      var time = startTime
      if (scenario.isStepDef) {
        beforeStepDef(time, scenario, callChain, SendFailedStepDefs.isInlined, scopes)
      } else {
        beforeScenario(time, scenario, callChain, scopes, SendFailedStepDefs.isInlined)
      }
      scenario.steps foreach { step => 
        val sCallChain = callChain.add(step)
        beforeStep(time, step, sCallChain, scopes, SendFailedStepDefs.isInlined)
        time = new ju.Date(time.getTime + durationMsecs(step.evalStatus.duration))
        afterStep(time, step, sCallChain, scopes)
        
      }
      scenario.examples foreach { examples => 
        val exsCallChain = callChain.add(examples)
        beforeExamples(time, examples, exsCallChain, SendFailedStepDefs.isInlined, scopes)
        examples.scenarios foreach { scenario => 
          time = inject(time, scenario, exsCallChain.add(scenario))
        }
        time = new ju.Date(time.getTime + durationMsecs(examples.evalStatus.duration))
        afterExamples(time, examples, exsCallChain)
      }
      if (scenario.isStepDef) {
        afterStepDef(time, scenario, callChain)
      } else {
        afterScenario(time, scenario, callChain)
      }
      time
    }

    def durationMsecs(duration: Duration): Long = {
      val msecs = step.evalStatus.duration.toMillis
      if (msecs < 1) 1L else msecs
    }

    step.stepDef foreach { stepDef => 
      val startTime = new ju.Date(endTime.getTime - stepDef.steps.map(_.evalStatus.duration.toMillis).sum)
      inject(startTime, stepDef, callChain.add(stepDef))
    }

  }

  private def isInlined(node: GwenNode, callChain: NodeChain): Boolean = { 
    SendStepDefs.isInlined && callChain.steps.size > (if (node.isInstanceOf[Step]) 1 else 0)
  }

  private def formatDescriptionAndMore(node: GwenNode, callChain: NodeChain): String = {
    val desc = formatDescription(node)
    val evalStatus = node.evalStatus
    if (evalStatus.isError && callChain.steps.size > 0) {
      val errors = {
        if (AppendErrorBlocks.all || (AppendErrorBlocks.leaf && isLeafNode(node))) {
          errorMessages(callChain.steps, Step.errorTrails(node)) match {
            case Nil => s"```error\r\n${Formatting.escapeHtml(evalStatus.message)}\r\n```"
            case msgs => msgs.map(msg => s"```error\r\n${Formatting.escapeHtml(msg)}\r\n```").mkString("\r\n") 
          }
        } else {
          ""
        }
      }
      s"${if (desc.size > 0) s"$desc\r\n\r\n" else ""}$errors"
    } else desc
  }

  private def formatDescription(node: GwenNode): String = {
    val desc = node match {
      case f: Feature => Formatting.escapeHtml(f.description.mkString(" "))
      case b: Background => Formatting.escapeHtml(b.description.mkString(" "))
      case r: Rule => Formatting.escapeHtml(r.description.mkString(" "))
      case s: Scenario => Formatting.escapeHtml(s.description.mkString(" "))
      case e: Examples => 
        val examplesDesc = Option(Formatting.escapeHtml(e.description.mkString(" ")))
        val tableDesc = e.table match {
          case Nil => None
          case table => Some(s"<pre>${Formatting.escapeHtml(Formatting.formatTable(table))}</pre>")
        }
        (examplesDesc ++ tableDesc).mkString("<br><br>")
      case s: Step => 
        s.table match {
          case Nil => 
            s.docString.map(ds => s"<pre>${Formatting.escapeHtml(Formatting.formatDocString(ds))}</pre>").getOrElse("")
          case table => s"<pre>${Formatting.escapeHtml(Formatting.formatTable(table))}</pre>"
        }
      case _ => ""
    }
    desc + (
      node.sourceRef filter { sref => 
        sref.toString.length > RPConfig.attributeMaxChars
      } map { sref => 
        s"""|${if (desc.size > 0) "<br>" else ""}
            |sourceRef: ${Formatting.escapeHtml(sref.toString)}""".stripMargin
      } getOrElse ""
    )

  }

  private def errorMessages(callSteps: List[Step], errorTrails: List[List[Step]]): List[String] = {
    errorTrails.map(errorTrail => errorMessage(callSteps, errorTrail))
  }

  private def errorMessage(callSteps: List[Step], errorTrail: List[Step]): String = {
    val message = errorTrail.last.evalStatus.message
    val hierarchy = {
      if (SendHierarchy.isInlined) errorHierarchy(callSteps, errorTrail).map(h => s"\r\n\r\n$h").getOrElse("") 
      else ""
    }
    s"${message}${hierarchy}"
  }

  private def errorHierarchy(callSteps: List[Step], errorTrail: List[Step]): Option[String] = {
    if (isLeafNode(errorTrail.head)) {
      val callChain = callSteps ++ errorTrail.tail
      val hierarchy = if (callChain.size > 1) callSteps.map(_.expression) else Nil
      if (hierarchy.nonEmpty) Some(s"Hierarchy:\r\n\r\n${hierarchy.mkString("\r\n")}") else None
    } else None
  }

  private val breadcrumbBindings = List(
    ("gwen.feature.name" -> "feature"), 
    ("gwen.rule.name" -> "rule"), 
    ("gwen.scenario.name" -> "scenario")
  )

  private def breadcrumbAtts[T](sourceRef: Option[SourceRef], callSteps: List[Step], scopes: ScopedDataStack): Map[String, String] = {
    val isFeature = sourceRef.map(_.isFeature).getOrElse(false)
    if (RPSettings.`gwen.rp.send.breadcrumbs` && isFeature && callSteps.size < 2) {
      breadcrumbBindings flatMap { case (bindingName, name) =>
        scopes.getOpt(bindingName) map { value => (name -> value) }
      } toMap
    } else {
      Map()
    }
  }

  private def filterTags(tags: List[Tag]): List[Tag] = {
    tags filter { tag => 
      (RPSettings.`gwen.rp.send.tags` && tag.isMarker) ||
      (RPSettings.`gwen.rp.send.annotations` && tag.isAnnotation)
    }
  }

  def close(evalStatus: EvalStatus): Unit = {
    rpClient.close(evalStatus) foreach { url =>
      logger.info(s"${RPReportConfig.name} launch report generated: $url")
    }
  }
  
}
