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
object PassadiDAvieulsXBee extends SpawnableCompanion[PassadiDAvieuls with Spawnable] {
  def apply(xbee: LocalXBee, as: SpawnStrategy) = {
    start(as)(new PassadiDAvieulsXBee(xbee))
  }

  protected class PassadiDAvieulsXBee(protected[this] val xbee: LocalXBee) extends PassadiDAvieuls with StateServer[State] {
    protected[this] override def initialState = {
      xbee.incomingMessageProcessor(Some(process))
      discoverAvieuls //Announce us and tell everybody to register itself to us
      State(Map(), new XBeeMessageDistributor(Nil), Nil, Map())
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

    protected[this] def internalSubscribe(sub: Subscription) = call { state => {
      val subs = sub :: state.subscriptions
      val s1 = state.withSubscriptions(subs)
      
      val s2 = if (state.subMgrs.contains(sub.key)) s1
        else {
          //Setup subscription manager
          val mgr = XBeeSubscriptionManager(sub.key)
          val dist = state.distributor.add(sub.key.xbee)(mgr.handlerProcess)
          state.withSubMgrs(state.subMgrs.updated(sub.key, mgr)).withDistributor(dist)
        }

      val unsub = () => internalUnsubscribe(sub)
      (unsub, s2)
    }}
    protected[this] def internalUnsubscribe(sub: Subscription) = cast { state => {
      val subs = state.subscriptions.filterNot(_ == sub)
      val s1 = state.withSubscriptions(subs)
      if (state.subscriptions.find(_.key == sub.key).isEmpty) {
        //Stop the subscription manager since nobody is interested in that anymore
        state.subMgrs.get(sub.key).foreach(_.terminate)
        state.withSubMgrs(state.subMgrs - sub.key)
      } else s1
    }}
      

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
	      val newDist = state.distributor.add(outer.address, serviceIndex)(p)
	      Some(state.withDistributor(newDist))
	    }}
	    private[this] def send(data: Seq[Byte]) = {
	      val selector = xbee.sendTrackedPacket(address, data)
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
	    override def subscribe(subscriptionType: Short, handler: (Seq[Byte]) => Unit) = {
              val sub = Subscription(SubscriptionKey(address, serviceIndex, subscriptionType), handler)
              internalSubscribe(sub)
	    }
	  }
	}.toList
      }
    }

    protected class XBeeSubscriptionManager protected(override val key: SubscriptionKey) extends SubscriptionManager with Spawnable {
      protected[this] override def body = {
        //TODO
      }
      override def terminate = process ! Terminate
      override def handlerProcess = process
    }
    protected object XBeeSubscriptionManager extends SpawnableCompanion[XBeeSubscriptionManager] {
      def apply(key: SubscriptionKey) = start(SpawnAsMonitoredChild)(new XBeeSubscriptionManager(key))
    }
  }

  protected trait XBeeAvieul extends Avieul {
    val address: XBeeAddress
  }
  protected trait XBeeAvieulService extends AvieulService {
    val serviceIndex: Byte
  }

  protected class XBeeMessageDistributor(processes: List[(XBeeAddress,Option[Byte],Process)]) {
    def add(forXBee: XBeeAddress, forServiceIndex: Byte)(process: Process) = {
      val newList = (forXBee, Some(forServiceIndex), process) :: processes
      new XBeeMessageDistributor(newList)
    }
    def add(forXBee: XBeeAddress)(process: Process) = {
      //TODO
      new XBeeMessageDistributor(processes)
    }

    type Element = (XBeeAddress,Option[Byte],Process)
    protected[this] def forwardTo(filter: Element => Boolean, msg: Any) = {
      processes.view.filter(e => filter(e)).map(_._3).foreach(_ ! msg)
      this
    }
    def all(item: Element) = true
    def service(xbee: XBeeAddress, serviceIndex: Byte)(item: Element) = {
      item._1 == xbee && item._2.filter(_ != serviceIndex).isEmpty
    }
    def xbee(xbee: XBeeAddress)(item: Element) = item._1 == xbee

    def handle(msg: Any): XBeeMessageDistributor @processCps = msg match {
      case XBeeDataPacket(_, from, _, _, payload) => payload match {
	case AnnounceService(_, _) =>
          forwardTo(xbee(from), msg)
	case msg @ GenericAvieulMessage((msgType, data), _) if (msgType >= 0x10 && msgType <= 0x9F && !data.isEmpty) => // call, request or subscription etc. (everything relating to service)
	  val serviceIndex = data.head
          forwardTo(service(from, serviceIndex), msg)
	case other =>
          //do not forward
          this
      }
      case end: ProcessEnd =>
	val newList = processes.filterNot(_._3 == end.process)
        new XBeeMessageDistributor(newList)
      case other =>
        //ignore
        this
    }
  }

  protected case class SubscriptionKey(xbee: XBeeAddress, serviceIndex: Byte, subscription: Short)
  protected case class Subscription(key: SubscriptionKey, handler: Seq[Byte] => Unit)

  protected trait SubscriptionManager {
    val key: SubscriptionKey
    def handlerProcess: Process
    def terminate: Unit
  }

  protected case class State(avieuls: Map[XBeeAddress,XBeeAvieul], distributor: XBeeMessageDistributor, subscriptions: List[Subscription], subMgrs: Map[SubscriptionKey,SubscriptionManager]) {
    def addAvieul(avieul: XBeeAvieul) = withAvieuls(avieuls.updated(avieul.address, avieul))
    protected[this] def withAvieuls(avieuls: Map[XBeeAddress,XBeeAvieul]) = State(avieuls, distributor, subscriptions, subMgrs)
    def withDistributor(distributor: XBeeMessageDistributor) = State(avieuls, distributor, subscriptions, subMgrs)
    def withSubscriptions(subscriptions: List[Subscription]) = State(avieuls, distributor, subscriptions, subMgrs)
    def withSubMgrs(subMgrs: Map[SubscriptionKey,SubscriptionManager]) = State(avieuls, distributor, subscriptions, subMgrs)
  }
}


/*
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
*/
