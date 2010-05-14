package ch.inventsoft.gidaivel.avieul.xbee

import ch.inventsoft.scalabase.process._
import Messages._
import ch.inventsoft.xbee._
import ch.inventsoft.scalabase.oip._
import ch.inventsoft.scalabase.extcol.ListUtil._
import ch.inventsoft.gidaivel.avieul._
import AvieulProtocol._
import ch.inventsoft.scalabase.time._


/**
 * Gateway to avieuls over a locally attached xbee-device.
 */
class PassadiDAvieulsXBee(protected[this] val xbee: LocalXBee) extends PassadiDAvieuls with StateServer[PDAXState] {
  private type State = PDAXState
  protected[this] override def initialState = {
    xbee.incomingMessageProcessor(Some(process))
    discoverAvieuls
    PDAXState(Map(), Nil)
  }
  protected[this] def discoverAvieuls = {
    xbee.broadcastPacket(RequestInfo())
  }
  protected[this] override def messageHandler(state: State) = {
    case XBeeDataPacket(`xbee`, from, _, _, AnnounceService(services, _)) =>
      val avieul = makeAvieul(from, services)
      state.avieuls.get(from) match {
	case Some(existing) =>
	  // already exists
	  // TODO check the services as they might have changed
	  Some(state)
	case None => 
	  //new avieul
	  val newState = state.withAvieuls(state.avieuls + ((from, avieul)))
	Some(newState)
      }
      
    case XBeeDataPacket(`xbee`, from, _, _, RequestInfo((), _)) =>
      //no services offered
      xbee.sendPacket(from, AnnounceService(Nil))
      Some(state)
    case XBeeDataPacket(`xbee`, from, _, _, ServiceCall(_, _)) =>
      log.info("PassadiDAvieuls doesn't offer services. Ignoring ServiceCall")
      Some(state)
    case XBeeDataPacket(`xbee`, from, _, _, ServiceRequest(_, _)) =>
      log.info("PassadiDAvieuls doesn't offer services. Ignoring ServiceRequest")
      Some(state)
    case XBeeDataPacket(`xbee`, from, _, _, ServiceSubscribe(_, _)) =>
      log.info("PassadiDAvieuls doesn't offer services. Ignoring ServiceSubscribe")
      Some(state)

    case otherMsg =>
      val newState = state.handle(otherMsg)
      if (newState.isEmpty) {
	log.info("Ignoring message {}", otherMsg)
	Some(state)
      } else newState
  }

  def close = cast_ { state =>
    xbee.incomingMessageProcessor(None)
    None
  }

  override def findAvieuls = get(_.avieuls.values.toList)
  override def findServices = get(_.avieuls.values.flatMap(_.services).toList)

  private[xbee] def sendCall(toXBee: XBeeAddress, serviceIndex: Byte)(callType: Short, payload: Seq[Byte]): MessageSelector[Either[Unit,AvieulError]] = call_? { (state,reply) =>
    val packet = ServiceCall(serviceIndex, callType, payload)
    val selector = xbee.sendTrackedPacket(toXBee, packet)
    val handler = (state: State) => selector { tx =>
      noop
      if (tx.isSuccess) {
	reply(Left())
      } else {
	reply(Right(TransmitFailed))
      }
      state
    }
    Some(state.addHandler(handler))
  }

  private[xbee] def sendRequest(toXBee: XBeeAddress, serviceIndex: Byte)(requestType: Short, payload: Seq[Byte]): MessageSelector[Either[Seq[Byte],AvieulError]] = call_? { (state,reply) =>
    val packet = ServiceRequest(serviceIndex, requestType, payload)
    val selector = xbee.sendTrackedPacket(toXBee, packet)
    val handler = (state: State) => selector { tx =>
      if (tx.isSuccess) {
	state.addHandler { state => {
	  case XBeeDataPacket(`xbee`, `toXBee`, _, _, ServiceResponse((`serviceIndex`, `requestType`, responseData), _)) =>
	    reply(Left(responseData))
	    noop
	    state
	  case XBeeDataPacket(`xbee`, `toXBee`, _, _, ServiceRequestUnknown((`serviceIndex`, `requestType`), _)) =>
	    reply(Right(UnknownAvieulServiceRequest))
	    noop
	    state
	  case XBeeDataPacket(`xbee`, `toXBee`, _, _, ServiceUnknown(`serviceIndex`, _)) =>
	    reply(Right(UnknownAvieulService))
	    noop
	    state
	}}
      } else {
	reply(Right(TransmitFailed))
	noop
	state
      }
    }
    Some(state.addHandler(handler))
  }

  private[xbee] def sendSubscribe(toXBee: XBeeAddress, serviceIndex: Byte)(subscriptionType: Short, payload: Seq[Byte], handler: (Seq[Byte]) => Unit @processCps): MessageSelector[Either[() => MessageSelector[Unit],AvieulError]] = call_? { (state,reply) =>
//TODO
    Some(state)
  }


  private[xbee] def makeAvieul(xbeeAddress: XBeeAddress, serviceDefs: Seq[(Byte,Int,Byte)]) = {
    new XBeeAvieul {
      override val services = serviceDefs.map { s =>
	val (index, t, v) = s
        new XBeeAvieulService(index, this, t, ServiceVersion(v))
      }.toList
      override val address = xbeeAddress
    }
  }
  private[xbee] trait XBeeAvieul extends Avieul {
    val address: XBeeAddress
    val services: List[XBeeAvieulService]
    def serviceForIndex(index: Byte): Option[XBeeAvieulService] = {
      services.filter(s => s.index==index).headOption
    }
  }
  private[xbee] class XBeeAvieulService(val index: Byte, override val providedBy: XBeeAvieul, override val serviceType: Int, override val version: ServiceVersion) extends AvieulService {
    override def call(callType: Short, payload: Seq[Byte]) = sendCall(providedBy.address,index)(callType, payload)
    override def request(requestType: Short, payload: Seq[Byte]) = sendRequest(providedBy.address,index)(requestType, payload)
    override def subscribe(subscriptionType: Short, payload: Seq[Byte], handler: Seq[Byte] => Unit @processCps) = sendSubscribe(providedBy.address,index)(subscriptionType, payload, handler)
  }
}
private[xbee] case class PDAXState(avieuls: Map[XBeeAddress,Avieul], protected[this] val handlers: List[Function1[PDAXState,PartialFunction[Any,PDAXState @processCps]]]) extends HandlerState[Any] {
  override type State = PDAXState
  def withAvieuls(avieuls: Map[XBeeAddress,Avieul]) = PDAXState(avieuls, handlers)
  protected[this] def withHandlers(handlers: List[Handler]) = PDAXState(avieuls, handlers)
}


trait HandlerState[M] {
  type State <: HandlerState[M]
  type Handler = Function1[State,PartialFunction[Any,State @processCps]]

  def addHandler(handler: Handler): State = {
    withHandlers(handler :: handlers)
  }
  def handle(msg: M) = {
    handlers.view.map(_.apply(unchanged)).find(_.isDefinedAt(msg)) match {
      case Some(handler) =>
	val newState = handler(msg)
	Some(newState.withHandlers(newState.handlers.filterNot(_ == handler)))
      case None =>
	None
    }
  }

  protected val handlers: List[Handler]
  protected[this] def unchanged: State = this.asInstanceOf[State]
  protected def withHandlers(handlers: List[Handler]): State
}
object HandlerState {
  def stateless[State,A](body : => A @processCps): State => State @processCps = { state =>
    body
    state
  }
}
object PassadiDAvieulsXBee extends SpawnableCompanion[PassadiDAvieulsXBee] {
  def apply(xbee: LocalXBee, as: SpawnStrategy) = {
    start(as)(new PassadiDAvieulsXBee(xbee))
  }
}
