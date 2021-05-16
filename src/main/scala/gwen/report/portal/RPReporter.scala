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
    val name = unit.featureFile.getPath
    val params = event.scopes.stepScope.params
    rpClient.startItem(event.time, None, unit, name, "", Nil, Map(), params, false)
  } 

  override def afterUnit(event: LifecycleEvent[FeatureUnit]): Unit = {
    val unit = event.source
    val evalStatus = unit.result.map(_.spec.evalStatus).getOrElse(Skipped)
    rpClient.finishItem(event.time, "", event.parent, evalStatus)
  }
    
  override def beforeFeature(event: LifecycleEvent[FeatureSpec]): Unit = {
    val spec = event.source
    val feature = spec.feature
    val name = s"${spec.specType}: ${feature.name}"
    val desc = formatDescription(feature)
    val tags = parseTags(feature.tags)
    val params = event.scopes.stepScope.params
    val breadcrumbs = breadcrumbAtts(feature.sourceRef, event.callTrail, event.scopes)
    val atts = Map("language" -> feature.language) ++ breadcrumbs
    rpClient.startItem(event.time, Some(event.parent), spec, name, desc, tags, atts, params, false)
  }

  override def afterFeature(event: LifecycleEvent[FeatureResult]): Unit = {
    val result = event.source
    val evalStatus = result.spec.evalStatus
    rpClient.finishItem(event.time, "", event.parent, evalStatus)
  }

  override def beforeBackground(event: LifecycleEvent[Background]): Unit = {
    val background = event.source
    val name = s"${background.keyword}: ${background.name}"
    val desc = formatDescription(background)
    val params = event.scopes.stepScope.params
    val inlined = isInlined(background, event.callTrail)
    rpClient.startItem(
      event.time, Some(event.parent), background, name, desc, Nil, Map(), params, inlined
    )
  }

  override def afterBackground(event: LifecycleEvent[Background]): Unit = {
    val background = event.source
    val evalStatus = background.evalStatus
    val desc = formatDescriptionWithError(background, event.callTrail)
    rpClient.finishItem(event.time, desc, event.parent, evalStatus)
  }

  override def beforeScenario(event: LifecycleEvent[Scenario]): Unit = {
    val scenario = event.source
    val inlined = isInlined(scenario, event.callTrail)
    beforeScenario(event.time, scenario, event.parent, event.callTrail, event.scopes, inlined)
  }

  private def beforeScenario(startTime: ju.Date, scenario: Scenario, parent: Identifiable, callTrail: List[Step], scopes: ScopedDataStack, inlined: Boolean): Unit = {
    val name = s"${scenario.keyword}: ${scenario.name}"
    val desc = formatDescription(scenario)
    val tags = parseTags(scenario.tags)
    val params = scopes.stepScope.params
    val atts = breadcrumbAtts(scenario.sourceRef, callTrail, scopes)
    rpClient.startItem(startTime, Some(parent), scenario, name, desc, tags, atts, params, inlined)
  }

  override def afterScenario(event: LifecycleEvent[Scenario]): Unit = {
    afterScenario(event.time, event.source, event.parent, event.callTrail)
  }

  private def afterScenario(endTime: ju.Date, scenario: Scenario, parent: Identifiable, callTrail: List[Step]): Unit = {
    val evalStatus = scenario.evalStatus
    val desc = formatDescriptionWithError(scenario, callTrail)
    rpClient.finishItem(endTime, desc, parent, evalStatus)
  }

  override def beforeExamples(event: LifecycleEvent[Examples]): Unit = {
    val examples = event.source
    val inlined = isInlined(examples, event.callTrail)
    beforeExamples(event.time, examples, event.parent, event.callTrail, inlined, event.scopes)
  }

  private def beforeExamples(startTime: ju.Date, examples: Examples, parent: Identifiable, callTrail: List[Step], inlined: Boolean, scopes: ScopedDataStack) = {
    val name = s"${examples.keyword}: ${examples.name}"
    val desc = formatDescription(examples)
    val tags = parseTags(examples.tags)
    val params = scopes.stepScope.params
    rpClient.startItem(startTime, Some(parent), examples, name, desc, tags, Map(), params, inlined)
    if (examples.table.nonEmpty) {
      rpClient.sendItemLog(LogLevel.INFO, Formatting.formatTable(examples.table))
    }
  }

  override def afterExamples(event: LifecycleEvent[Examples]): Unit = {
    afterExamples(event.time, event.source, event.parent, event.callTrail)
  }

  private def afterExamples(endTime: ju.Date, examples: Examples, parent: Identifiable, callTrail: List[Step]): Unit = {
    val evalStatus = examples.evalStatus
    val desc = formatDescriptionWithError(examples, callTrail)
    rpClient.finishItem(endTime, desc, parent, evalStatus)
  }

  override def beforeRule(event: LifecycleEvent[Rule]): Unit = {
    val rule = event.source
    val name = s"${rule.keyword}: ${rule.name}"
    val desc = formatDescription(rule)
    val params = event.scopes.stepScope.params
    val atts = breadcrumbAtts(rule.sourceRef, event.callTrail, event.scopes)
    rpClient.startItem(event.time, Some(event.parent), rule, name, desc, Nil, atts, params, false)
  }

  override def afterRule(event: LifecycleEvent[Rule]): Unit = {
    val rule = event.source
    val evalStatus = rule.evalStatus
    rpClient.finishItem(event.time, "", event.parent, evalStatus)
  }

  override def beforeStepDef(event: LifecycleEvent[Scenario]): Unit = {
    val stepDef = event.source
    val inlined = isInlined(stepDef, event.callTrail)
    beforeStepDef(event.time, stepDef, event.parent, inlined, event.scopes)
  }

  private def beforeStepDef(startTime: ju.Date, stepDef: Scenario, parent: Identifiable, inlined: Boolean, scopes: ScopedDataStack): Unit = {
    val name = s"${if (stepDef.isForEach) "ForEach" else stepDef.keyword}: ${stepDef.name}"
    val desc = formatDescription(stepDef)
    val tags = parseTags(stepDef.tags)
    val params = scopes.stepScope.params
    rpClient.startItem(startTime, Some(parent), stepDef, name, desc, tags, Map(), params, inlined)
  }

  override def afterStepDef(event: LifecycleEvent[Scenario]): Unit = {
    afterStepDef(event.time, event.source, event.parent, event.callTrail)
  }

  private def afterStepDef(endTime: ju.Date, stepDef: Scenario, parent: Identifiable, callTrail: List[Step]): Unit = {
    val evalStatus = stepDef.evalStatus
    val desc = formatDescriptionWithError(stepDef, callTrail)
    rpClient.finishItem(endTime, desc, parent, evalStatus)
  }

  override def beforeStep(event: LifecycleEvent[Step]): Unit = {
    val step = event.source
    val inlined = isInlined(step, event.callTrail)
    beforeStep(event.time, step, event.parent, event.callTrail, event.scopes, inlined)
  }

  private def beforeStep(startTime: ju.Date, step: Step, parent: Identifiable, callTrail: List[Step], scopes: ScopedDataStack, inlined: Boolean) = {
    val name = s"${step.keyword} ${step.name}"
    val desc = formatDescription(step)
    val params = scopes.stepScope.params
    val breadcrumbs = breadcrumbAtts(step.sourceRef, callTrail, scopes)
    val atts = if (breadcrumbs.nonEmpty) { breadcrumbs ++ Map("step" -> step.name) } else breadcrumbs
    rpClient.startItem(startTime, Some(parent), step, name, desc, Nil, atts, params, inlined)
    step.docString foreach { docString =>
      rpClient.sendItemLog(LogLevel.INFO, Formatting.formatDocString(docString))
    }
    if (step.table.nonEmpty) {
      rpClient.sendItemLog(LogLevel.INFO, Formatting.formatTable(step.table))
    }
  }

  override def afterStep(event: LifecycleEvent[Step]): Unit = {
    afterStep(event.time, event.source, event.parent, event.callTrail, event.scopes)
  }

  private def afterStep(endTime: ju.Date, step: Step, parent: Identifiable, callTrail: List[Step], scopes: ScopedDataStack): Unit = {
    val evalStatus = step.evalStatus
    if (SendStepDefs.isNone && !SendFailedStepDefs.isNone && EvalStatus.isError(evalStatus.status)) {
      injectErrorTrail(endTime, step, parent, callTrail, scopes)
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
    rpClient.finishItem(endTime, desc, parent, evalStatus)
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

  def injectErrorTrail(endTime: ju.Date, step: Step, parent: Identifiable, callTrail: List[Step], scopes: ScopedDataStack): Unit = {

    def inject(startTime: ju.Date, scenario: Scenario, parent: Identifiable, callTrail: List[Step]): ju.Date = {
      var time = startTime
      if (scenario.isStepDef) {
        beforeStepDef(time, scenario, parent, SendFailedStepDefs.isInlined, scopes)
      } else {
        beforeScenario(time, scenario, parent, callTrail, scopes, SendFailedStepDefs.isInlined)
      }
      scenario.steps foreach { step => 
        val sCallTrail = if (callTrail.lastOption.map(_.name != step.name).getOrElse(true)) callTrail ++ List(step) else callTrail
        beforeStep(time, step, scenario, sCallTrail, scopes, SendFailedStepDefs.isInlined)
        time = new ju.Date(time.getTime + durationMsecs(step.evalStatus.duration))
        afterStep(time, step, step, sCallTrail, scopes)
        
      }
      scenario.examples foreach { examples => 
        beforeExamples(time, examples, scenario, callTrail, SendFailedStepDefs.isInlined, scopes)
        examples.scenarios foreach { scenario => 
          val params = scenario.params
          if (params.nonEmpty) scopes.stepScope.push(scenario.name, params)
          try {
            time = inject(time, scenario, examples, callTrail)
          } finally {
            if (params.nonEmpty) scopes.stepScope.pop
          }
        }
        time = new ju.Date(time.getTime + durationMsecs(examples.evalStatus.duration))
        afterExamples(time, examples, examples, callTrail)
      }
      if (scenario.isStepDef) {
        afterStepDef(time, scenario, scenario, callTrail)
      } else {
        afterScenario(time, scenario, scenario, callTrail)
      }
      time
    }

    def durationMsecs(duration: Duration): Long = {
      val msecs = step.evalStatus.duration.toMillis
      if (msecs < 1) 1L else msecs
    }

    step.stepDef foreach { stepDef => 
      val startTime = new ju.Date(endTime.getTime - stepDef.steps.map(_.evalStatus.duration.toMillis).sum)
      val params = stepDef.params
      if (params.nonEmpty) scopes.stepScope.push(stepDef.name, params)
      try {
        inject(startTime, stepDef, parent, callTrail)
      } finally {
        if (params.nonEmpty) scopes.stepScope.pop
      }
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
