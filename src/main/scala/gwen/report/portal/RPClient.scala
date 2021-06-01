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

import gwen._
import gwen.dsl._
import gwen.Errors._
import gwen.eval.GwenOptions

import scala.concurrent.duration.Duration
import scala.io.Source
import scala.jdk.CollectionConverters._

import com.epam.reportportal.listeners.ItemStatus
import com.epam.reportportal.listeners.ItemType
import com.epam.reportportal.listeners.LogLevel
import com.epam.reportportal.message.ReportPortalMessage;
import com.epam.reportportal.service.Launch
import com.epam.reportportal.service.ReportPortal
import com.epam.ta.reportportal.ws.model.attribute.ItemAttributesRQ
import com.epam.ta.reportportal.ws.model.launch.StartLaunchRQ
import com.epam.ta.reportportal.ws.model.FinishExecutionRQ
import com.epam.ta.reportportal.ws.model.FinishTestItemRQ
import com.epam.ta.reportportal.ws.model.StartTestItemRQ
import com.typesafe.scalalogging.LazyLogging
import io.reactivex.Maybe
import io.reactivex.observers.DisposableMaybeObserver

import java.io.File
import java.net.HttpURLConnection
import java.net.SocketTimeoutException
import java.net.URL
import java.{util => ju}
import com.epam.ta.reportportal.ws.model.ParameterResource

/**
  * Connects to report portal and performs all reporting operations.
  *
  * @param options Gwen options
  * @param info Gwen info
  */
class RPClient(options: GwenOptions) extends LazyLogging with GwenInfo {
  info: GwenInfo =>
  
  private val startTime = ju.Calendar.getInstance.getTime
  private val rpids = new ju.concurrent.ConcurrentHashMap[String, Maybe[String]]()
  private val launchLock = new ju.concurrent.Semaphore(1)
  private var launchUuid: Option[String] = None

  private lazy val session: Launch = init()

  private def init(): Launch = {
    RPSettings.init()
    val reportPortal = ReportPortal.builder().build()
    val parameters = reportPortal.getParameters
    val rq = new StartLaunchRQ()
    rq.setStartTime(startTime)
    rq.setName(parameters.getLaunchName)
    rq.setDescription(parameters.getDescription)
    rq.setMode(parameters.getLaunchRunningMode)
    rq.setRerun(parameters.isRerun)
    Option(parameters.getRerunOf).filter(_.size > 0) foreach { rq.setRerunOf }
    val atts = Option(parameters.getAttributes).getOrElse(new ju.HashSet[ItemAttributesRQ]())
    rq.setAttributes(atts)
    launchLock.acquire()
    reportPortal.newLaunch(rq) tap { launch => 
      launch.start.subscribeWith(new DisposableMaybeObserver[String]() {
        override def onStart(): Unit = { }
        override def onSuccess(uuid: String): Unit = { 
          logger.info(s"Connected to report portal [Launch uuid $uuid]")
          launchUuid = Some(uuid)
          sendLaunchLog(LogLevel.INFO, s"Started [${info.implName} v${info.implVersion}] ${options.commandString(info)}")
          RPSettings.writeRerunFile(uuid)
          launchLock.release()
        }
        override def onError(error: Throwable): Unit = { launchLock.release() }
        override def onComplete(): Unit = { launchLock.release() }
      })
    }
  }

  def healthCheck(timeoutSecs: Int): Unit = {
    val endpoint = RPSettings.`rp.endpoint`
    val healthUrl = s"${RPSettings.`rp.endpoint`}${if (RPSettings.`rp.endpoint`.endsWith("/")) "" else "/" }health"
    try {
      val conn = new URL(healthUrl).openConnection().asInstanceOf[HttpURLConnection]
      conn.setConnectTimeout(timeoutSecs * 1000)
      conn.setReadTimeout(timeoutSecs * 1000)
      conn.setRequestMethod("GET");
      val code = conn.getResponseCode
      if (code != 200) {
        serviceHealthCheckError(s"Report Portal unavailable or unreachable at $endpoint")
      } else {
        val body = Source.fromInputStream(conn.getInputStream()).mkString.trim
        if (body.replaceAll("""\s""", "") == """{"status":"UP"}""") {
          logger.info(s"Report Portal heartbeat OK: $code $body")
        } else {
          logger.error(s"Report Portal heartbeat FAILED")
          serviceHealthCheckError(s"Report Portal health check at $healthUrl failed with response: $code $body")
        }
      }
    } catch {
      case e: SocketTimeoutException =>
        logger.error(s"Report Portal heartbeat FAILED")
        serviceHealthCheckError(s"Report Portal health check at $healthUrl timed out after ${timeoutSecs} second(s)")
      case e: Throwable => 
        logger.error(s"Report Portal heartbeat FAILED")
        serviceHealthCheckError(s"Report Portal health check FAILED: $e")
    }
  }

  def close(evalStatus: EvalStatus): Option[String] = {
    launchLock.acquire()
    launchUuid map { launchId =>
      logger.info(s"Closing Report Portal connection..")
      sendLaunchLog(mapLevel(evalStatus), s"Finished ${evalStatus}")
      val rq = new FinishExecutionRQ()
      rq.setEndTime(ju.Calendar.getInstance.getTime)
      val start = System.nanoTime
      session.finish(rq)
      val duration = Formatting.formatDuration(Duration.fromNanos(System.nanoTime - start))
      logger.info(s"[$duration] Report Portal connection closed${launchUuid.map(uuid => s" [Launch uuid $uuid]").getOrElse("")}")
      val endpoint = RPSettings.`rp.endpoint`
      //s"$endpoint${if (endpoint.endsWith("/")) "" else "/"}ui/#${RPSettings.`rp.project`}/launches/all/$launchId"
      s"$endpoint${if (endpoint.endsWith("/")) "" else "/"}ui/#${RPSettings.`rp.project`}/launches/all"
    }
  }
  
  def startItem(
      startTime: ju.Date,
      parent: Option[Identifiable],
      node: Identifiable,
      name: String, 
      desc: String, 
      tags: List[Tag], 
      atts: Map[String, String],
      params: List[(String, String)],
      inlined: Boolean): Unit = {

    val rq = createStartRequest(startTime, node, name, desc, tags, atts, params, inlined)
    val rpid = parent map { p => 
      session.startTestItem(rpids.get(p.uuid), rq) // child
    } getOrElse {
      session.startTestItem(rq) // root
    }
    rpids.put(node.uuid, rpid)
    truncate(name) foreach { _ =>
      sendItemLog(LogLevel.INFO, name)
    }

  }

  private def createStartRequest(
      startTime: ju.Date,
      node: Identifiable,
      name: String, 
      desc: String, 
      tags: List[Tag], 
      atts: Map[String, String],
      params: List[(String, String)],
      inlined: Boolean): StartTestItemRQ = {

    val sourceRef = node match {
      case specNode : SpecNode => specNode.sourceRef
      case _ => None
    }
    new StartTestItemRQ() tap { rq =>
      rq.setStartTime(startTime)
      rq.setType(mapItemType(node).name)
      rq.setHasStats(!inlined)
      if (desc.size > 0) rq.setDescription(desc)
      sourceRef foreach { sref =>
        rq.setCodeRef(sref.toString)
      }
      addAtts(rq, tags, atts, sourceRef)
      addName(rq, name, inlined) 
      addParams(rq, params)
      addTestCaseId(rq, sourceRef, params)
    }
    
  }

  private def addAtts(rq: StartTestItemRQ, tags: List[Tag], atts: Map[String, String], sourceRef: Option[SourceRef]): Unit = {
    val attributes = new ju.HashSet[ItemAttributesRQ]()
    attributes.addAll(
      (tags map { tag => 
        new ItemAttributesRQ(null, tag.toString)
      }).toSet.asJava
    )
    attributes.addAll((atts.map { case (key, value) => new ItemAttributesRQ(key, value) }).toSet.asJava)
    sourceRef.filter(_.toString.length <= RPConfig.attributeMaxChars) foreach { sref =>
      attributes.add(new ItemAttributesRQ("sourceRef", sref.toString))
    }
    if (attributes.size > 0) rq.setAttributes(attributes)
  }

  private def addName(rq: StartTestItemRQ, name: String, inlined: Boolean): Unit = {
    val truncatedName = truncate(name).getOrElse(name)
    val encodedName = encode(truncatedName, inlined)
    rq.setName(encodedName)
  }

  private def addParams(rq: StartTestItemRQ, params: List[(String, String)]): Unit = {
    val rpParams = params map { case (name, value) =>
      new ParameterResource() tap { param =>
        param.setKey(name)
        param.setValue(value)
      }
    }
    rq.setParameters(rpParams.asJava)
  }

  private def addTestCaseId(rq: StartTestItemRQ, sourceRef: Option[SourceRef], params: List[(String, String)]): Unit = {
    sourceRef foreach { srcRef => 
      RPSettings.`gwen.rp.testCaseId.keys` match {
        case RPConfig.TestCaseIdKeys.`nodePath+params` if srcRef.nodePath.nonEmpty =>
          srcRef.nodePath foreach { nodePath => 
            val paramValues = params map { case (_, v) => v } mkString ""
            rq.setTestCaseId(Formatting.sha256Hash(nodePath + paramValues))
          }
        case RPConfig.TestCaseIdKeys.`sourceRef+params` =>
          val codeRef = srcRef.toString
          val paramValues = params map { case (_, v) => v } mkString ""
          rq.setTestCaseId(Formatting.sha256Hash(codeRef + paramValues))
        case _ =>  
          // test case ID auto generated by client-java lib
      }
    }
  }

  def finishItem(endTime: ju.Date, desc: String, parent: Identifiable, evalStatus: EvalStatus): Unit = {
    session.getStepReporter.finishPreviousStep()
    val status = mapStatus(evalStatus)
    val rq = new FinishTestItemRQ()
    rq.setEndTime(endTime)
    rq.setStatus(status.name)
    if (desc.length > 0) rq.setDescription(desc)
    session.finishTestItem(rpids.get(parent.uuid), rq)
  }

  def sendLaunchLog(level: LogLevel, msg: String): Unit = {
    logger.debug(s"sendLaunchLog(level=$level, msg=$msg)")
    ReportPortal.emitLaunchLog(
      s"${launchUuid.map(uuid => s"Launch uuid $uuid - ").getOrElse("")}$msg",
      level.name, 
      ju.Calendar.getInstance.getTime)
  }

  def sendAttachmentLogs(evalStatus: EvalStatus, attachments: List[(String, File)]): Unit = {
    attachments foreach { attachment =>
      sendAttachmentLog(evalStatus, attachment)
    }
  }

  def sendAttachmentLog(evalStatus: EvalStatus, attachment: (String, File)): Unit = {
    val level = mapLevel(evalStatus)
    val (name, file) = attachment
    sendItemLog(level, s"$name (attachment)", Some(file))
  }

  def sendItemLog(level: LogLevel, msg: String): Unit = {
    sendItemLog(level, msg, None)
  }

  def sendItemLog(evalStatus: EvalStatus, msg: String, file: Option[File]): Unit = {
    val level = mapLevel(evalStatus)
    sendItemLog(level, msg, file)
  }

  def sendItemLog(level: LogLevel, msg: String, file: Option[File]): Unit = {
    logger.debug(s"sendItemLog(level=$level, msg=$msg, file=${file})")
    //val encodedMsg = encode(msg, true)
    file match {
      case Some(f) => 
        val rpMessage = new ReportPortalMessage(f, msg)
        ReportPortal.emitLog(rpMessage, level.name, ju.Calendar.getInstance.getTime)
      case None => 
        ReportPortal.emitLog(msg, level.name, ju.Calendar.getInstance.getTime)
    }
  }

  private def mapItemType(node: Identifiable): ItemType = {
    node match {
      case specNode: SpecNode => 
        specNode.nodeType match {
          case NodeType.Feature | NodeType.Meta => ItemType.STORY
          case NodeType.Step => ItemType.STEP
          case _ => ItemType.SCENARIO
        }
      case _ => ItemType.SUITE
    }
  }

  private def mapStatus(evalStatus: EvalStatus): ItemStatus = {
    evalStatus.status match {
      case StatusKeyword.Passed | StatusKeyword.Loaded | StatusKeyword.Sustained => ItemStatus.PASSED
      case StatusKeyword.Skipped | StatusKeyword.Pending | StatusKeyword.Disabled => ItemStatus.SKIPPED
      case _ => ItemStatus.FAILED
    }
  }

  private def mapLevel(evalStatus: EvalStatus): LogLevel = {
    evalStatus.status match {
      case StatusKeyword.Failed => LogLevel.ERROR
      case StatusKeyword.Passed => LogLevel.INFO
      case _ => LogLevel.WARN
    }
  }

  private def encode(text: String, markdownable: Boolean): String = {
    (if (markdownable && RPSettings.`gwen.rp.send.markdownBlocks`) {
      s"""|```
          |$text
          |```""".stripMargin
    } else {
      text
    }).replaceAll(s"$ZeroChar", "")
  }

  private def truncate(text: String): Option[String] = {
    val max = if (RPSettings.`gwen.rp.send.markdownBlocks`) RPConfig.nameMaxChars - 10 else RPConfig.nameMaxChars
    if (text.length > max) Some(s"${text.substring(0, max - 4)}... ")
    else None
  }
  
}
