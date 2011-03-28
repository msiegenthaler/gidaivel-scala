package ch.inventsoft
package gidaivel

import scala.xml._
import scalabase.process._
import scalabase.time._
import scalabase.oip._
import scalaxmpp._
import scalaxmpp.component._
import passadi._


/**
 * Gidaivel agent representing an avieul service.
 * See the documententation under /docs/devices.
 */
trait AvieulBasedDevice extends GidaivelAgent {
  val avieul: Avieul
  val avieulService: AvieulService
  protected val timeout = 5 s

  private val namespace = "urn:gidaivel:avieul"

  protected override def features = super.features :+ namespace
  protected override def identities = super.identities :+ XmppIdentity("gidaivel", "avieul", Some(avieul.id))

  protected override def iqGet = super.iqGet :+ info :+ signal
  protected override def message = super.message :+ resync

  protected val info = mkIqGet {
    case (get @ FirstElem(ElemName("info", namespace)),state) =>
      val st = "0x"+avieulService.serviceType.toHexString
      val sv = avieulService.version.toString
      val xml = <info xmlns={namespace}>
        <avieul>{avieul.id}</avieul>
        <avieul-service>
          <type>{st}</type>
          <version>{sv}</version>
        </avieul-service></info>
      get.resultOk(xml)
  }
  protected val signal = mkIqGet {
    case (get @ FirstElem(ElemName("signal", namespace)),state) =>
      val status = avieul.status.receiveWithin(timeout)
      status match {
        case Some(status) =>
          val an = avieul.toString
          val xml = <info xmlns={namespace}>
            <avieul>{an}</avieul>
            <signal-quality><percent>{status.quality.percentage}</percent></signal-quality>
            <last-contact>{status.lastContact.asXmlDateTime}</last-contact></info>
          get.resultOk(xml)
        case None =>
          val xml = <info xmlns={namespace}><unknown/></info>
          get.resultOk(xml)
      }
  }
  protected val resync = mkMsg {
    case (FirstElem(ElemName("resync", namespace)),state) =>
      doResync
  }
  protected def doResync: Unit @process = noop


  protected type Unsubscribe = () => Unit @process

  /** functions relating to the device */
  protected object device {
    def id = avieulService.id
    def call(callType: Short, data: Seq[Byte]=Nil, retries: Int=3): Unit @process = {
      val error = avieulService.call(callType, data).map(_.fold(_ => None, e => Some(e.toString))).or {
        case Timeout => Some("Timeout")
      }.receiveWithin(timeout)
      error match {
        case Some(error) =>
          log.trace("Could not call {} on avieul {}: {}. {} retries left.", callType, avieulService.id, error, retries)
          if (retries == 0) {
            noop
            throw new AvieulCommunicationException(avieulService, "Error calling "+callType+": "+error)
          } else call(callType, data, retries-1)
        case None => noop
      }
    }
    def request(requestType: Short, data: Seq[Byte]=Nil, retries: Int=3): Seq[Byte] @process = {
      val r = avieulService.request(requestType, data).map(_.fold(r => Left(r), e => Right(e.toString))).or {
        case Timeout => Right("Timeout")
      }.receiveWithin(timeout)
      r match {
        case Left(result) =>
          noop
          result
        case Right(error) => 
          log.trace("Could not request {} on avieul {}: {}. {} retries left.", requestType, avieulService.id, error, retries)
          if (retries == 0) {
            noop
            throw new AvieulCommunicationException(avieulService, "Error requesting "+requestType+": "+error)
          } else request(requestType, data, retries-1)
      }
    }
    def subscribe(subscription: Short, fun: Seq[Byte] => Unit @process) = {
      avieulService.subscribe(subscription, fun).or {
        case Timeout =>
          noop
          log.trace("Could not subscribe to {} on avieul {}", subscription, avieulService.id)
          throw new AvieulCommunicationException(avieulService, "Error subscribing to "+subscription)
      }.receive
    }
  }
  override def toString = "AvieulBasedDevice("+avieulService.id+")"
}

case class AvieulCommunicationException(avieulService: AvieulService, message: String)
     extends RuntimeException("Could not communicate with "+avieulService.id+": "+message)
