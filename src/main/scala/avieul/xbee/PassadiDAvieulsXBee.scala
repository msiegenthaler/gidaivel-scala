package ch.inventsoft.gidaivel.avieul.xbee

import ch.inventsoft.scalabase.process._
import Messages._
import ch.inventsoft.xbee._
import ch.inventsoft.scalabase.oip._
import ch.inventsoft.scalabase.extcol.ListUtil._
import ch.inventsoft.gidaivel.avieul._
import AvieulProtocol._
import ch.inventsoft.scalabase.time._
import cps.CpsUtils._


/**
 * Gateway to avieuls over a locally attached xbee-device.
 */
class PassadiDAvieulsXBee(protected[this] val xbee: LocalXBee) extends PassadiDAvieuls with StateServer[PDAXState] {
  private[this] type State = PDAXState
  protected[this] override def initialState = {
    xbee.incomingMessageProcessor(Some(process))
    discoverAvieuls //Announce us and tell everybody to register itself to us
    PDAXState(Map(), new XBeeMessageDistributor(Nil))
  }
  protected[this] override def messageHandler(state: State) = {
    case XBeeDataPacket(`xbee`, from, _, _, AnnounceService(services, _)) =>
      val avieul = makeAvieul(from, services)
      Some(state.addAvieul(avieul))
    case rest =>
      val newDist = state.distributor.handle(rest)
      Some(state.withDistributor(newDist))
  }

  protected[this] def discoverAvieuls = {
    xbee.broadcastPacket(RequestInfo())
  }
  override def findAvieuls = get(_.avieuls.values.toList)
  override def findServices = get(_.avieuls.values.flatMap(_.services).toList)
  def close = cast_ { state => 
    xbee.incomingMessageProcessor(None)
    None
  }

  protected type SubFun = Seq[Byte] => Unit @processCps

  protected[this] class SubscriptionManager(val forXBee: XBeeAddress, val serviceIndex: Byte, val subscriptionType: Short) extends Spawnable {
    protected[this] override def body = {
      subscribe
      //TODO handle the error code
      loop(Nil)
      unsubscribe
    }
    protected[this] def loop(subscriptions: List[SubFun]): Unit = receive {
      case AddSubscription(sub) => loop(sub :: subscriptions)
      case RemoveSubscription(sub) => 
	val subs = subscriptions.filterNot(_ == sub)
      //TODO send the outer a request to remove us, instead of terminating us ourself
	if (subs.isEmpty) ()
	else loop(subs)
      case ServicePublish((`serviceIndex`, `subscriptionType`, data), _) =>
	subscriptions.foreach_cps(_(data))
    }
    protected[this] def subscribe = {
      val sent = send(ServiceSubscribe(serviceIndex, subscriptionType))
      if (sent.isSuccess) {
	receiveWithin(1 minute) {
	  case ServiceSubscriptionConfirm((`serviceIndex`, `subscriptionType`), _) =>
	    Left(())
	  case ServiceSubscriptionUnknown((`serviceIndex`, `subscriptionType`), _) =>
	    Right(UnknownAvieulServiceSubscription)
	  case ServiceUnknown(`serviceIndex`, _) =>
	    Right(UnknownAvieulService)
	  case Timeout =>
	    Right(TransmitFailed)
	}
      } else {
	noop
	Right(TransmitFailed)
      }
    }
    protected[this] def unsubscribe = {
      xbee.sendPacket(forXBee, ServiceUnsubscribe(serviceIndex, subscriptionType))
    }
    protected[this] def send(data: Seq[Byte]): TransmitStatus @processCps = {
      val selector = xbee.sendTrackedPacket(forXBee, data)
      val res = receiveWithin(5 s)(selector.option)
      res.getOrElse(TransmitStatusNoAckReceived)
    }
  }



  protected[this] def makeAvieul(xbeeAddress: XBeeAddress, serviceDefs: Seq[(Byte,Int,Byte)]): XBeeAvieul = {
    new XBeeAvieul {
      override val address = xbeeAddress
      override val services = serviceDefs.map { s =>
	val (index, t, v) = s
	val outer = this
        new XBeeAvieulService {
	  override val serviceIndex = index
	  override val serviceType = t
	  override val version = ServiceVersion(v)
	  override val providedBy = outer
	  private[this] def child[A](body: => A @processCps) = call_? { (state: State, reply: A => Unit) => {
	    val p = spawnChild(Monitored) {
	      val result = body
	      reply(result)
	    }
	    val newDist = state.distributor.add(outer.address, serviceIndex, p)
	    Some(state.withDistributor(newDist))
	  }}
	  private[this] def send(data: Seq[Byte]) = {
	    val selector = xbee.sendTrackedPacket(forXBee, data)
	    val res = receiveWithin(5 s)(selector.option)
	    res.getOrElse(TransmitStatusNoAckReceived)
	  }
	  override def call(callType: Short, payload: Seq[Byte]) = child {
	    val sent = send(ServiceCall(serviceIndex, callType, payload))
	    if (sent.isSuccess) Left(())
	    else Right(TransmitFailed)
	  }
	  override def request(requestType: Short, payload: Seq[Byte]) = child {
	    val sent = send(ServiceRequest(serviceIndex, requestType, payload))
	    if (sent.isSuccess) {
	      receiveWithin(5 minutes) {
		case ServiceResponse((`serviceIndex`, `requestType`, data), _) =>
		  Left(data)
		case ServiceUnknown(`serviceIndex`, _) =>
		  Right(UnknownAvieulService)
		case ServiceRequestUnknown((`serviceIndex`, `requestType`), _) =>
		  Right(UnknownAvieulServiceRequest)
		case Timeout =>
		  Right(TransmitFailed)
	      }
	    } else {
	      noop
	      Right(TransmitFailed)
	    }
	  }
	  override def subscribe(subscriptionType: Short, handler: (Seq[Byte]) => Unit @processCps) = child {
	    //TODO keep a state!! don't subscribe twice to the same

	    val sent = send(ServiceSubscribe(serviceIndex, subscriptionType))
	    if (sent.isSuccess) {
	      receiveWithin(1 minute) {
		case ServiceSubscriptionConfirm((`serviceIndex`, `subscriptionType`), _) =>
		  //TODO
		  Left(() => ())
		case ServiceSubscriptionUnknown((`serviceIndex`, `subscriptionType`), _) =>
		  Right(UnknownAvieulServiceSubscription)
		case ServiceUnknown(`serviceIndex`, _) =>
		  Right(UnknownAvieulService)
		case Timeout =>
		  Right(TransmitFailed)
	      }
	    } else {
	      noop
	      Right(TransmitFailed)
	    }
	  }
	}
      }.toList
    }
  }
}
private[xbee] trait XBeeAvieul extends Avieul {
  val address: XBeeAddress
}
private[xbee] trait XBeeAvieulService extends AvieulService {
  val serviceIndex: Byte
}

private[xbee] class XBeeMessageDistributor(processes: List[(XBeeAddress,Byte,Process)]) {
  def add(forXBee: XBeeAddress, forServiceIndex: Byte, process: Process) = {
    val newList = (forXBee, forServiceIndex, process) :: processes
    new XBeeMessageDistributor(newList)
  }
  protected[this] def all(body: Process => Unit) =
    processes.view.map(_._3).foreach(body)
  protected[this] def service(xbee: XBeeAddress, serviceIndex: Byte)(body: Process => Unit) =
    processes.view.filter(e => e._1==xbee && e._2==serviceIndex).map(_._3).foreach(body)

  def handle(msg: Any): XBeeMessageDistributor @processCps = msg match {
    case XBeeDataPacket(_, from, _, _, payload) => payload match {
      case AnnounceService(_, _) => //ignore
	this //TODO renew subscriptions
      case msg @ GenericAvieulMessage((msgType, data), _) if (msgType >= 0x10 && msgType <= 0x4F && !data.isEmpty) => // call, request or subscription
	val serviceIndex = data.head
	service(from, serviceIndex)(_ ! msg)
	this
      case other => this //do not forward
    }
    case status: TransmitStatus =>
      all(_ ! status)
      this
    case end: ProcessEnd =>
      val newList = processes.filterNot(_._3 == end.process)
      new XBeeMessageDistributor(newList)
  }
}

private[xbee] trait Subscription {
}
private[xbee] class SubscriptionManager(val xbee: XBeeAddress, val serviceIndex: Byte) extends Spawnable {
  protected[this] override def body = {
    
  }
  protected[this] def subscribe = {
    
  }


/*
  protected[this] def initialState = SubscriptionManagerState(Nil)
  
  def add(subscription: Seq[Byte] => Unit @processCps): MessageSelector[Either[()=>Unit,AvieulError]] = call { state =>
    //TODO register 
    //TODO async
    val unsub = () => remove(subscription)
    (Left(unsub), state.withSubscriptions(subscription :: state.subscriptions))
  }
  protected def remove(subscription: Seq[Byte] => Unit @processCps) = cast_ { state =>
    val newSubs = state.subscriptions.filterNot(_ == subscription)
    if (newSubs.isEmpty) {
      unsubscribe
      None
    } else Some(state.withSubscriptions(newSubs))
  }
  protected[this] def subscribe() = {
  }
  */
}
private[xbee] case class SubscriptionManagerState(subscriptions: List[Seq[Byte]=>Unit @processCps]) {
  def withSubscriptions(subscriptions: List[Seq[Byte]=>Unit @processCps]) =
    SubscriptionManagerState(subscriptions)
}

private[xbee] case class PDAXState(avieuls: Map[XBeeAddress,XBeeAvieul], distributor: XBeeMessageDistributor) {
  protected[this] def withAvieuls(avieuls: Map[XBeeAddress,XBeeAvieul]) = PDAXState(avieuls, distributor)
  def addAvieul(avieul: XBeeAvieul) = withAvieuls(avieuls.updated(avieul.address, avieul))
  def withDistributor(distributor: XBeeMessageDistributor) = PDAXState(avieuls, distributor)
}

/*
class PassadiDAvieulsXBee(protected[this] val xbee: LocalXBee) extends PassadiDAvieuls with StateServer[PDAXState] {
  private type State = PDAXState
  protected[this] override def initialState = {
    xbee.incomingMessageProcessor(Some(process))
    discoverAvieuls
    PDAXState(Map(), Nil, Map())
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

  private[xbee] def sendSubscribe(toXBee: XBeeAddress, serviceIndex: Byte)(subscriptionType: Short, handler: (Seq[Byte]) => Unit @processCps): MessageSelector[Either[() => Unit,AvieulError]] = call_? { (state,reply) =>
    val key = SubscriptionKey(xbee, toXBee, serviceIndex, subscriptionType)
    val manager = state.subscriptionManagers.get(key) match {
      case Some(manager) =>
	reply(Left(() => unsubscribe(key, handler)))
	manager.addSubscriber(handler)
      case None =>
	//TODO
	new SubscriptionManager(key, handler :: Nil)
    }
    //TODO reply
    Some(state.updateSubscriptionManager(manager))
  }
  protected[this] def unsubscribe(key: SubscriptionKey, fun: Seq[Byte] => Unit @processCps) = cast { state =>
    state.subscriptionManagers.get(key) match {
      case Some(manager) =>
	manager.removeSubscriber(fun) match {
	  case Some(newManager) =>
 	    state.updateSubscriptionManager(newManager)
	  case None =>
	    state.removeSubscriptionManager(key)
	}
      case None => state
    }
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
    override def call(callType: Short, payload: Seq[Byte]) =
      sendCall(providedBy.address,index)(callType, payload)
    override def request(requestType: Short, payload: Seq[Byte]) =
      sendRequest(providedBy.address,index)(requestType, payload)
    override def subscribe(subscriptionType: Short, handler: Seq[Byte] => Unit @processCps) =
      sendSubscribe(providedBy.address,index)(subscriptionType, handler)
  }
}
private[xbee] case class PDAXState(avieuls: Map[XBeeAddress,Avieul], protected[this] val handlers: List[Function1[PDAXState,PartialFunction[Any,PDAXState @processCps]]], subscriptionManagers: Map[SubscriptionKey,SubscriptionManager]) extends HandlerState[Any] {
  override type State = PDAXState
  def withAvieuls(avieuls: Map[XBeeAddress,Avieul]) =
    PDAXState(avieuls, handlers, subscriptionManagers)
  protected[this] def withHandlers(handlers: List[Handler]) =
    PDAXState(avieuls, handlers, subscriptionManagers)
  def updateSubscriptionManager(manager: SubscriptionManager) =
    withSubscriptionManagers(subscriptionManagers.updated(manager.key, manager))
  def removeSubscriptionManager(key: SubscriptionKey) =
    withSubscriptionManagers(subscriptionManagers - key)
  private def withSubscriptionManagers(subscriptionManagers: Map[SubscriptionKey,SubscriptionManager]) =
    PDAXState(avieuls, handlers, subscriptionManagers)
}

case class SubscriptionKey(xbee: LocalXBee, remoteXBee: XBeeAddress, serviceIndex: Byte, subscription: Short)
class SubscriptionManager(val key: SubscriptionKey, protected[this] val subscribers: List[Seq[Byte]=>Unit @processCps]) {
  protected type PublishFun = Seq[Byte] => Unit @processCps
  
  def setup = {
    val selector = xbee.sendTrackedPacket(key.removeXBee, ServiceSubscribe(key.serviceIndex, key.subscription, data))
    
  }

  def handle(msg: Any): PartialFunction[Any,Unit @processCps] = {
    case XBeeDataPacket(key.xbee, key.remoteXBee, _, _, ServicePublish((key.serviceIndex, key.subscription, data), _)) =>
      subscribers.foreach_cps(_(data))
  }
  def addSubscriber(fun: PublishFun): SubscriptionManager =
    new SubscriptionManager(key, subscribers ::: fun :: Nil)
  def removeSubscriber(fun: PublishFun): Option[SubscriptionManager] = {
    subscribers.filterNot(_ == fun) match {
      case Nil => None
      case list => Some(new SubscriptionManager(key, list))
    }
  }
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
*/
object PassadiDAvieulsXBee extends SpawnableCompanion[PassadiDAvieulsXBee] {
  def apply(xbee: LocalXBee, as: SpawnStrategy) = {
    start(as)(new PassadiDAvieulsXBee(xbee))
  }
}
