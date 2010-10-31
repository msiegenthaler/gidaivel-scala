package ch.inventsoft
package gidaivel
package passadi

import scalabase.process._
import Messages._
import scalabase.log._
import scalabase.oip._
import scalabase.time._
import scalabase.extcol.ListUtil._
import xbee._
import AvieulProtocol._


/**
 * Implementation of a xbee=avieul passadi.
 */
trait PassadiXBee extends Passadi with StateServer with Log {
  protected override type State = PassadiState
  protected type ChangeListener = Function1[PassadiChange, Unit @process]
  protected case class PassadiState(xbee: LocalXBee, 
                                    avieuls: Map[XBeeAddress,AvieulState],
                                    distributor: MessageDistributor,
                                    subscriptions: List[Subscription],
                                    subMgrs: Map[SubscriptionKey,SubscriptionManager],
                                    listener: ChangeListener) {
    def distributeAndUpdate(packet: ReceivedXBeeDataPacket, avieul: AvieulState) = {
      val nd = distributor.handle(packet)
      val na = avieuls.updated(avieul.address, avieul.update(packet.signalStrength))
      copy(distributor=nd, avieuls=na)
    }
  }
  protected case class AvieulState(avieul: XBeeAvieul,
                                   signal: Option[SignalStrength] = None,
                                   override val lastContact: TimePoint = TimePoint.current)
                 extends AvieulStatus {
    def signalStrength = signal.getOrElse(SignalStrength(0))
    def address = avieul.address
    override def quality = signalStrength.asPercent
    def update(signal: Option[SignalStrength]) = {
      copy(signal=signal, lastContact=TimePoint.current)
    }
  }

  val requestTimeout: Duration = 1 minute
  val refreshTimeout: Duration = 30 seconds
  val sendTimeout: Duration = 8 seconds

  protected[this] def openXBee: LocalXBee @process
  protected[this] override def init = {
    val xbee = ResourceManager[LocalXBee](openXBee, _.close).receive.resource
    xbee.setMessageHandler(process ! _)
    refresh
    PassadiState(xbee, Map(), new MessageDistributor(), Nil, Map(), _ => noop)
  }
  protected[this] override def handler(state: State) = super.handler(state).orElse_cps {
    case packet @ ReceivedXBeeDataPacket(from, ss, _, data) => 
      data match {
        case AnnounceServices(services, _) =>
          log.debug("Announce from {} has been received ({} services)", from, services.size)
          state.avieuls.get(from) match {
            case Some(avst) =>
              if (!avst.avieul.sameServices(services)) {
                //changed --> update
                val avieul = new XBeeAvieul(from, services, state.xbee)
                val avst = AvieulState(avieul, ss)
                val na = state.avieuls.updated(from, avst)
                fireListener(ChangedAvieul(proxy(avieul.address)))
                Some(state.copy(avieuls=na))
              } else {
                //unchanged
                val newState = state.distributeAndUpdate(packet, avst)
                Some(newState)
              }
            case None =>
              //new avieul
              val avieul = new XBeeAvieul(from, services, state.xbee)
              val avst = AvieulState(avieul, ss)
              val na = state.avieuls.updated(from, avst)
              fireListener(NewAvieul(proxy(avieul.address)))
              Some(state.copy(avieuls=na))
          }
        case otherData => state.avieuls.get(from) match {
          case Some(avst) =>
            //update state
            val newState = state.distributeAndUpdate(packet, avst)
            Some(newState)
          case None =>
            //unknown avieul, request info from it
            state.xbee.send(from, RequestInfo())
            Some(state)
        }
      }
    case other => //mainly ProcessEnds for children
      val nd = state.distributor.handle(other)
      Some(state.copy(distributor=nd))
  }

  override def avieuls = get(internalAvieuls(_))
  protected[this] def internalAvieuls(state: State): Iterable[Avieul] =
    state.avieuls.keys.map(proxy(_))

  protected[this] def proxy(address: XBeeAddress) = new AvieulProxy(address)

  override def services = get { state =>
    val avieuls = state.avieuls.values.map(_.avieul)
    avieuls.flatMap(_.services).toList
  }

  override def refresh = async { state =>
    val addresses = Set() ++ state.avieuls.keys
    discoverAvieuls
    val started = TimePoint.current
    receiveWithin(refreshTimeout) { case Timeout => () }
    discardIfNoResponseSince(addresses, started).receive
  }

  override def close = stopAndWait

  protected[this] def discardIfNoResponseSince(who: Set[XBeeAddress], t: TimePoint) = call { state =>
    val (old,na) = state.avieuls.span { e =>
      val (address, avst) = e
      who.contains(address) && avst.lastContact < t
    }
    old.keys.foreach_cps { a =>
      log.info("Avieul with address {} did not respond to our requests, removing it", a)
      fireListener(RemovedAvieul(proxy(a)))
    }
    val newState = state.copy(avieuls = na)
    (internalAvieuls(newState), newState)
  }
  protected[this] def discoverAvieuls = cast { state =>
    state.xbee.broadcast(RequestInfo())
    state
  }

  override def changeListener(l: ChangeListener) = cast { state =>
    state.copy(listener = l)
  }
  protected[this] def fireListener(change: => PassadiChange) = async { state =>
    state.listener(change)
  }

  protected[this] def internalSubscribe(sub: Subscription) = call { state => {
    val subs = sub :: state.subscriptions
    val s1 = state.copy(subscriptions=subs)
      
    val s2 = {
      if (s1.subMgrs.contains(sub.key)) s1
      else {
        //Setup subscription manager
        val mgr = XBeeSubscriptionManager(sub.key, state.xbee)
        val dist = s1.distributor.add(sub.key.xbee)(mgr.handlerProcess)
        val nsm = s1.subMgrs.updated(sub.key, mgr)
        s1.copy(subMgrs = nsm, distributor = dist)
      }
    }

    val unsub = () => internalUnsubscribe(sub)
    (unsub, s2)
  }}
  protected[this] def internalUnsubscribe(sub: Subscription) = cast { state => {
    val subs = state.subscriptions.filterNot(_ == sub)
    val s1 = state.copy(subscriptions=subs)
    if (s1.subscriptions.find(_.key == sub.key).isEmpty) {
      //Stop the subscription manager since nobody is interested in that anymore
      s1.subMgrs.get(sub.key).foreach_cps(_.terminate)
      s1.copy(subMgrs = s1.subMgrs - sub.key)
    } else s1
  }}
  protected[this] def internalPublish(sub: SubscriptionKey, data: Seq[Byte]) = cast { state => {
    log.trace("Publishing data for {} to enlisted subscribers", sub)
    state.subscriptions.view.filter(_.key == sub).foreach_cps(_.handler(data))
    state
  }}

  protected[this] def internalGetState(forAvieul: XBeeAddress) = get { state =>
    state.avieuls.get(forAvieul)
  }


  /**
   * If we could not send data to an xbee then try to reach it and if it does
   * not respond within the refreshTimeout remove it from the list.
   */
  protected [this] def markMaybeGone(address: XBeeAddress) = asyncCast { state =>
    state.avieuls.get(address) match {
      case Some(avst) =>
        if (avst.notHeardFromIn > refreshTimeout) {
          val started = TimePoint.current
          state.xbee.send(address, RequestInfo())
          receiveWithin(refreshTimeout) { case Timeout => () }
          discardIfNoResponseSince(Set(address), started)
        } else ()
      case None => ()
    }
  }


  /**
   * Spawns a (monitored) child process and adds it as a receiver for all messages sent to
   * that xbee/service combination
   */
  protected[this] def childReceiver[A](forXBee: XBeeAddress, serviceIndex: Byte)(body: => A @process):
      Selector[A] @process = {
    this ! new ModifyStateMessage with MessageWithSimpleReply[A] {
      override def execute(state: State) = {
        val p = spawnChild(Required) {
          val r = body
          reply(r)
        }
        val nd = state.distributor.add(forXBee, serviceIndex)(p)
        state.copy(distributor=nd)
      }
    }
  }

  /**
   * Proxy to an xbee avieul.
   * This class exists so that XBeeAvieul can be immutable.
   */
  protected[this] class AvieulProxy(val address: XBeeAddress) extends Avieul {
    override def services = get { state =>
      val res: Iterable[AvieulService] = state.avieuls.get(address) match {
        case Some(avst) => avst.avieul.services.toList
        case None => Nil
      }
      res
    }
    override def status = internalGetState(address)

    override def equals(that: Any) = that match {
      case that: AvieulProxy => 
        if (that.canEqual(this)) address == that.address
        else false
      case other => false
    }
    override def canEqual(that: Any) = that.isInstanceOf[AvieulProxy]
    override def hashCode = address.hashCode
    override def toString = "Avieul("+address+")"
  }

  /**
   * XBee that acts as an avieul.
   */
  protected[this] class XBeeAvieul(val address: XBeeAddress, serviceDefs: Seq[(Byte,Int,Byte)], xbee: LocalXBee) {
    val services = serviceDefs.map { sd =>
      val (sindex, stype, sversion) = sd
      new XBeeAvieulService {
        override val serviceType = stype
        override val version = ServiceVersion(sversion)
        override val index = sindex
        override val xbee = XBeeAvieul.this.xbee
        override val address = XBeeAvieul.this.address
      }
    }
    def service(index: Byte): Option[XBeeAvieulService] = {
      services.find(_.index == index)
    }
    def sameServices(sd2: Seq[(Byte,Int,Byte)]) = {
      if (services.size == sd2.size) {
        sd2.find { s =>
          val (i, t, v) = s
          service(i) match {
            case Some(service) =>
              service.serviceType != t || service.version != ServiceVersion(v)
            case None => true //new service
          }
        }.isEmpty
      } else false
    }
    override def toString = "XBeeAvieul("+address+")"
  }

  /**
   * Service offered by an avieul
   */
  protected[this] trait XBeeAvieulService extends AvieulService {
    val address: XBeeAddress
    val xbee: LocalXBee
    val index: Byte
    override def providedBy = replyInCallerProcess(proxy(address))
    protected[this] def sendTo(data: Seq[Byte]) = {
      val result = xbee.sendTracked(address, data).receiveOption(sendTimeout).
                        getOrElse(TransmitStatusNoAckReceived)
      if (result == TransmitStatusNoAckReceived) markMaybeGone(address)
      result
    }
    override def call(callType: Short, payload: Seq[Byte]) = concurrentWithReply {
      log.debug("Call to service {}-{}-{}", address, index, callType)
      val sent = sendTo(ServiceCall(index, callType, payload))
      if (sent.isSuccess) Left()
      else Right(TransmitFailed)
    }
    override def request(requestType: Short, payload: Seq[Byte]) = childReceiver(address, index) {
      log.debug("Request for service {}-{}-{}", address, index, requestType)
      val sent = sendTo(ServiceRequest(index, requestType, payload))
      if (sent.isSuccess) {
        receiveWithin(requestTimeout) { 
          case XBeeMessage(`address`, ServiceRequestUnknown((`index`, `requestType`), _)) =>
            log.trace("Request for service {}-{}-{} => service request unknown", address, index, requestType)
            noop
            Right(UnknownAvieulServiceRequest)
          case XBeeMessage(`address`, ServiceUnknown(`index`, _)) =>
            log.trace("Request for service {}-{}-{} => service unknown", address, index, requestType)
            noop
            Right(UnknownAvieulService)
          case XBeeMessage(`address`, ServiceResponse((`index`, `requestType`, resp), _)) =>
            log.trace("Request for service {}-{}-{} => response received {}",
                      address, index, requestType, byteListToHex(resp))
            noop
            Left(resp)
          case Timeout =>
            noop
            Right(TransmitFailed)
        }
      } else {
        noop
        Right(TransmitFailed)
      }
    }
    override def subscribe(subscriptionType: Short, handler: Seq[Byte] => Unit @process) = {
      log.debug("Subscription for service {}-{}-{}", address, index, subscriptionType)
      val key = SubscriptionKey(address, index, subscriptionType)
      val sub = Subscription(key, handler)
      internalSubscribe(sub)
    }

    override def hashCode = {
      address.hashCode ^ serviceType ^ version.hashCode
    }
    override def equals(that: Any) = that match {
      case o: XBeeAvieulService =>
        o.canEqual(this) && (address==o.address) && (serviceType==o.serviceType) && (version==o.version)
      case other => false
    }
    override def canEqual(that: Any) = that.isInstanceOf[XBeeAvieulService]
    override def toString = {
      "AvieulService("+serviceType+"-"+version+", "+address+")"
    }
  }

  /**
   * Distributes the incoming ReceivedXBeeDataPacket's to the interested parties.
   * Manages its subscriber list itself by listening to ProcessEnd's.
   */
  protected class MessageDistributor(processes: List[(XBeeAddress,Option[Byte],Process)] = Nil) extends Log {
    def add(forXBee: XBeeAddress, forServiceIndex: Byte)(process: Process) = {
      val newList = (forXBee, Some(forServiceIndex), process) :: processes
      new MessageDistributor(newList)
    }
    def add(forXBee: XBeeAddress)(process: Process) = {
      val newList = (forXBee, None, process) :: processes
      new MessageDistributor(newList)
    }

    type Element = (XBeeAddress,Option[Byte],Process)
    protected[this] def forwardTo(filter: Element => Boolean, msg: => XBeeMessage) = {
      processes.view.filter(e => filter(e)).map(_._3).foreach_cps(_ ! msg)
      this
    }
    def all(item: Element) = true
    def service(xbee: XBeeAddress, serviceIndex: Byte)(item: Element) = {
      item._1 == xbee && item._2.filter(_ != serviceIndex).isEmpty
    }
    def xbee(xbee: XBeeAddress)(item: Element) = item._1 == xbee

    def handle(msg: Any): MessageDistributor @process = msg match {
      case ReceivedXBeeDataPacket(from, _, _, payload) => payload match {
        case packet @ AnnounceServices(_, _) =>
          forwardTo(xbee(from), XBeeMessage(from, packet))
        case packet @ GenericAvieulMessage((msgType, data), _)
             if (msgType >= 0x10 && msgType <= 0x9F && !data.isEmpty) =>
          // call, request or subscription etc. (everything relating to service)
          log.trace("Passadi distributor got service message of type {}: {}", msgType, packet)
          val serviceIndex = data.head
          forwardTo(service(from, serviceIndex), XBeeMessage(from, packet))
        case other =>
          this //do not forward
      }
      case end: ProcessEnd =>
        end match {
          case ProcessExit(_) => ()
          case ProcessKill(_, _, _) => ()
          case ProcessCrash(_, cause) =>
            log.warn("A passadi d'avieuls child process crashed: {}", cause)
        }
        val newList = processes.filterNot(_._3 == end.process)
        new MessageDistributor(newList)
      case other => this
    }
  }
  protected case class XBeeMessage(from: XBeeAddress, data: Seq[Byte])


  protected case class SubscriptionKey(xbee: XBeeAddress, serviceIndex: Byte, subscription: Short) {
    override def toString = "Subscription "+xbee+"-"+serviceIndex+"-"+subscription
  }
  protected[this] case class Subscription(key: SubscriptionKey, handler: Seq[Byte] => Unit @process)
  /** manages a subscription */
  protected[this] trait SubscriptionManager {
    val key: SubscriptionKey
    def handlerProcess: Process
    def terminate: Unit @process
  }
  /**
   * Implementation of the subscription manager
   */
  protected[this] class XBeeSubscriptionManager protected(override val key: SubscriptionKey, xbee: LocalXBee) extends SubscriptionManager with Spawnable {
    protected[this] override def body = {
      def runLoop: Unit @process = {
        log.trace("Establishing subscription {}", key)
        subscribeWithRetry
        log.debug("Subscription {} has been set up", key)
        if (run) runLoop
        else noop
      }
      runLoop
      unsubscribe
    }
    protected[this] def subscribeWithRetry: Boolean @process = {
      emptyMsgs
      val r = subscribe
      r match {
        case Successful => noop; true
        case Failed =>
          log.trace("Could not setup {}. Retrying", key)
          receive {
            case XBeeMessage(key.xbee, AnnounceServices(_, _)) => subscribeWithRetry
            case Terminate => noop; false
            case Timeout => subscribeWithRetry
            case other => noop; false
          }
        case UnknownSubOrService =>
          log.trace("Could not setup {} because service or subscriptionType is unknown. Retrying..", key)
          receive {
            case XBeeMessage(key.xbee, AnnounceServices(_, _)) => subscribeWithRetry
            case Terminate => noop; false
            case Timeout => subscribeWithRetry
          }
      }
    }
    protected[this] def subscribe: SubscriptionResult @process = {
      val sent = send(ServiceSubscribe(key.serviceIndex, key.subscription))
      if (sent.isSuccess) {
        receiveWithin(1 minute) {
          case XBeeMessage(key.xbee, ServiceSubscriptionConfirm((key.serviceIndex, key.subscription), _)) =>
            Successful
          case XBeeMessage(key.xbee, ServiceSubscriptionUnknown((key.serviceIndex, key.subscription), _)) =>
            UnknownSubOrService
          case XBeeMessage(key.xbee, ServiceUnknown(key.serviceIndex, _)) =>
            UnknownSubOrService
          case Timeout =>
            Failed
        }
      } else {
        noop
        Failed
      }
    }
    protected sealed trait SubscriptionResult
    object Successful extends SubscriptionResult
    object Failed extends SubscriptionResult
    object UnknownSubOrService extends SubscriptionResult

    protected[this] def unsubscribe = {
      log.debug("Subscription {} has been stopped", key)
      xbee.send(key.xbee, ServiceUnsubscribe(key.serviceIndex, key.subscription))
    }

    protected[this] def run: Boolean @process = receive {
      case Terminate => false
      case XBeeMessage(key.xbee, ServicePublish((key.serviceIndex, key.subscription, data), _)) =>
        internalPublish(key, data)
        run
      case XBeeMessage(key.xbee, AnnounceServices(_,_ )) => 
        //Device reannounces its services, it was probably restarted
        // renew subscription
        log.debug("Refreshing subscription since xbee has reannounced itself")
        true
      case other => run
    }

    protected[this] def send(data: Seq[Byte]): TransmitStatus @process = {
      val selector = xbee.sendTracked(key.xbee, data)
      val res = receiveWithin(sendTimeout)(selector.option).getOrElse(TransmitStatusNoAckReceived)
      if (res == TransmitStatusNoAckReceived) markMaybeGone(key.xbee)
      res
    }
    protected[this] def emptyMsgs: Unit @process = receiveNoWait {
      case Timeout => ()
      case something => emptyMsgs
    }

    override def terminate = process ! Terminate
    override def handlerProcess = process
  }
  protected[this] object XBeeSubscriptionManager {
    def apply(key: SubscriptionKey, xbee: LocalXBee): SubscriptionManager @process = {
      Spawner.start(new XBeeSubscriptionManager(key, xbee), SpawnAsMonitoredChild)
    }
  }
}

object PassadiXBee {
  def apply(xbee: => LocalXBee @process, as: SpawnStrategy = SpawnAsRequiredChild) = {
    val passadi = new PassadiXBee {
      override def openXBee = xbee
    }
    Spawner.start(passadi, as)
  }
}
