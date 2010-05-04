package ch.inventsoft.gidaivel.avieul.xbee

import ch.inventsoft.scalabase.process._
import Messages._
import ch.inventsoft.xbee._
import ch.inventsoft.scalabase.oip._
import ch.inventsoft.gidaivel.avieul._
import AvieulProtocol._


/**
 * Gateway to avieuls over a locally attached xbee-device.
 */
trait PassadiDAvieulsXBee extends PassadiDAvieuls with StateServer[PDAXState] {
  protected[this] val xbee: LocalXBee
  private type State = PDAXState
  protected[this] override def initialState = {
    xbee.incomingMessageProcessor(Some(process))
    PDAXState(Map())
  }
  protected[this] override def messageHandler(state: State) = {
    case XBeeDataPacket(`xbee`, address, _, _, data) => data match {
      case AnnounceService(services, _) =>
	val avieul = XBeeAvieul(address, services)
	state.avieuls.get(address) match {
	  case Some(existing) =>
	    // already exists
	    // TODO check the services!
	    Some(state)
	  case None => 
	    //new avieul
	    val newState = state.withAvieuls(state.avieuls + ((address, avieul)))
	    Some(newState)
	}

      case packet =>
	state.avieuls.get(address) match {
	  case Some(avieul) =>
	    () //TODO
	  case None =>
	    () //TODO
	}
	Some(state)
    }
  }

  def close = cast_ { state =>
    xbee.incomingMessageProcessor(None)
    None
  }

  override def findAvieuls = call_? { (state,reply) =>
    reply(null)
    Some(state)
  }
}

private[xbee] case class PDAXState(avieuls: Map[XBeeAddress,XBeeAvieul]) {
  def withAvieuls(avieuls: Map[XBeeAddress,XBeeAvieul]) = PDAXState(avieuls)
}

private[xbee] class XBeeAvieul(val address: XBeeAddress, services: List[XBeeAvieulService]) extends Avieul with ConcurrentObject {
  override def queryServices = null //TODO
/*replyInCallerProcess {
    val s: List[AvieulService] = services
    s
  }*/
}
private[xbee] object XBeeAvieul {
  def apply(address: XBeeAddress, serviceDefs: Seq[(Byte,Int,Byte)]) = {
    val services = serviceDefs.map { s =>
      val (index, t, v) = s
      new XBeeAvieulService(index, t, ServiceVersion(v))
    }.toList
    new XBeeAvieul(address, services)
  }
}

private[xbee] class XBeeAvieulService(index: Byte, val serviceType: Int, val version: ServiceVersion) extends AvieulService {
}
