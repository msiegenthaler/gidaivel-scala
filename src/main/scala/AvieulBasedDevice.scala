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
  val avieulService: AvieulService
  protected val timeout = 5 s
  protected def avieul = avieulService.providedBy.receiveWithin(timeout)

  private val namespace = "urn:gidaivel:avieul"

  protected override def iqGet(state: State) = super.iqGet(state) :+ info :+ signal

  protected val info = mkIqGet {
    case (get @ FirstElem(ElemName("info", namespace)),state) =>
      val st = "0x"+avieulService.serviceType.toHexString
      val sv = avieulService.version.toString
      val avieul = avieulService.providedBy.toString
      val xml = <info xmlns={namespace}>
        <avieul>{avieul}</avieul>
        <avieul-service>
          <type>{st}</type>
          <version>{sv}</version>
        </avieul-service></info>
      (get.resultOk(xml), state)
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
          (get.resultOk(xml), state)
        case None =>
          noop
          val xml = <info xmlns={namespace}><unknown/></info>
          (get.resultOk(xml), state)
      }
  }
}
