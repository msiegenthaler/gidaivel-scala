package ch.inventsoft.gidaivel.avieul.xbee

import ch.inventsoft.scalabase.process._
import Messages._
import ch.inventsoft.xbee._
import ch.inventsoft.scalabase.oip._
import ch.inventsoft.scalabase.extcol.ListUtil._
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
    PDAXState(Map(), Nil)
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

  override def findAvieuls = call_? { (state,reply) =>
    reply(null)
    Some(state)
  }

  private[xbee] def sendCall(toXBee: XBeeAddress, serviceIndex: Byte)(callType: Short, payload: Seq[Byte]): MessageSelector[Either[Unit,AvieulError]] = call_? { (state,reply) =>
    val packet = ServiceCall(serviceIndex, callType, payload)
    def successHandler(state: State): State = {
      reply(Left(()))
      state
    }
    def failHandler(state: State): State = {
      reply(Right(TransmitFailed))
      state
    }
    val handler = xbee.sendTrackedPacket(toXBee, packet)(tx => if (tx.isSuccess) successHandler _ else failHandler _)
    Some(state.addHandler(handler.asInstanceOf[state.Handler]))
  }

  private[xbee] def sendRequest(toXBee: XBeeAddress, serviceIndex: Byte)(requestType: Short, payload: Seq[Byte]): MessageSelector[Either[Seq[Byte],AvieulError]] = call_? { (state,reply) =>
    def successHandler(state: State): State = {
      state.addHandler {
	case XBeeDataPacket(`xbee`, `toXBee`, _, _, ServiceResponse((`serviceIndex`, `requestType`, responseData), _)) =>
	  HandlerState.stateless(reply(Left(responseData)))
	case XBeeDataPacket(`xbee`, `toXBee`, _, _, ServiceRequestUnknown((`serviceIndex`, `requestType`), _)) =>
	  HandlerState.stateless(reply(Right(UnknownAvieulServiceRequest)))
	case XBeeDataPacket(`xbee`, `toXBee`, _, _, ServiceUnknown(`serviceIndex`, _)) =>
	  HandlerState.stateless(reply(Right(UnknownAvieulService)))
      }
    }
    def failHandler(state: State): State = {
      reply(Right(TransmitFailed))
      state
    }
    val packet = ServiceRequest(serviceIndex, requestType, payload)
    val handler = xbee.sendTrackedPacket(toXBee, packet)(tx => if (tx.isSuccess) successHandler _ else failHandler _)
    Some(state.addHandler(handler.asInstanceOf[state.Handler]))
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
private[xbee] case class PDAXState(avieuls: Map[XBeeAddress,Avieul], protected[this] val handlers: List[PartialFunction[Any,Function1[PDAXState,PDAXState @processCps]]]) extends HandlerState[Any] {
  override type State = PDAXState
  def withAvieuls(avieuls: Map[XBeeAddress,Avieul]) = PDAXState(avieuls, handlers)
  protected[this] def withHandlers(handlers: List[Handler]) = PDAXState(avieuls, handlers)
}


trait HandlerState[M] {
  type State <: HandlerState[M]
  type Handler = PartialFunction[Any,Function1[State,State @processCps]]

  def addHandler(handler: Handler): State = {
    withHandlers(handler :: handlers)
  }
  def handle(msg: M) = {
    removeLast(handlers, (h: Handler) => h.isDefinedAt(msg)) match {
      case Some((handler, rest)) =>
	val state = withHandlers(rest)
	val hfun = handler(msg)
	Some(hfun(state))
      case None =>
	None
    }
  }

  protected[this] val handlers: List[Handler]
  protected[this] def withHandlers(handlers: List[Handler]): State
}
object HandlerState {
  def stateless[State,A](body : => A @processCps): State => State @processCps = { state =>
    body
    state
  }
}

