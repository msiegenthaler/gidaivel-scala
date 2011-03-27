package ch.inventsoft
package gidaivel
package passadi
package xbee

import scalabase.process._
import Messages._
import scalabase.log._
import scalabase.oip._
import scalabase.time._
import scalabase.extcol.ListUtil._
import ch.inventsoft.xbee._
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
    override def quality = new SignalQuality {
    	val dBm = signalStrength.dBm
    	val percentage = signalStrength.asPercent
    }
    def update(signal: Option[SignalStrength]) = {
      copy(signal=signal, lastContact=TimePoint.current)
    }
  }

  val requestTimeout: Duration = 1 minute
  val refreshTimeout: Duration = 30 seconds
  val sendTimeout: Duration = 8 seconds
  @volatile var id: Option[String] = None

  protected def openXBee: LocalXBee @process
  protected override def init = {
    val xbee = ResourceManager[LocalXBee](openXBee, _.close).receive.resource
    xbee.setMessageHandler(process ! _)
    refresh
    id = Some(xbee.address.map(_.toString).receiveWithin(sendTimeout))
    log.info("Initialized PassadiXBee with local address {}", id)
    PassadiState(xbee, Map(), new MessageDistributor(), Nil, Map(), _ => noop)
  }
  protected override def handler(state: State) = super.handler(state).orElse_cps {
    case packet @ ReceivedXBeeDataPacket(from, ss, _, data) => 
      data match {
        case AnnounceServices(services, _) =>
          log.info("Announce from {} has been received ({} services)", from, services.size)
          state.avieuls.get(from) match {
            case Some(avst) =>
              if (!avst.avieul.sameServices(services)) {
                //changed --> update
                val avieul = new XBeeAvieul(from, services, state.xbee)
                val avst = AvieulState(avieul, ss)
                val na = state.avieuls.updated(from, avst)
                fireListener(ChangedAvieul(proxy(avieul.address)))
                Some(state.copy(avieuls=na).distributeAndUpdate(packet, avst))
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
              Some(state.copy(avieuls=na).distributeAndUpdate(packet, avst))
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
  protected def internalAvieuls(state: State): Iterable[Avieul] =
    state.avieuls.keys.map(proxy(_))

  protected def proxy(address: XBeeAddress) = new AvieulProxy(address)

  override def services = get { state =>
    val avieuls = state.avieuls.values.map(_.avieul)
    avieuls.flatMap(_.services).toList
  }

  override def refresh = async { state =>
    log.debug("Refresh: Contacting all Avieuls")
    val addresses = Set() ++ state.avieuls.keys
    discoverAvieuls
    val started = TimePoint.current
    receiveWithin(refreshTimeout) { case Timeout => () }
    discardIfNoResponseSince(addresses, started).receive
  }

  override def close = stopAndWait

  override def toString = id.getOrElse("unknown")

  protected def discardIfNoResponseSince(who: Set[XBeeAddress], t: TimePoint) = call { state =>
    val (old,na) = state.avieuls.span { e =>
      val (address, avst) = e
      who.contains(address) && avst.lastContact < t
    }
    old.keys.foreach_cps { a =>
      log.info("Avieul with address {} did not respond to our requests, removing it.", a)
      fireListener(RemovedAvieul(proxy(a)))
    }
    val newState = state.copy(avieuls = na)
    (internalAvieuls(newState), newState)
  }
  protected def discoverAvieuls = cast { state =>
    state.xbee.broadcast(RequestInfo())
    state
  }

  override def changeListener(l: ChangeListener) = cast { state =>
    state.copy(listener = l)
  }
  protected def fireListener(change: => PassadiChange) = async { state =>
    state.listener(change)
  }

  protected def internalSubscribe(sub: Subscription) = call { state => {
    val subs = sub :: state.subscriptions
    log.trace("New Subscription for {} (total {} for this, {} total)", sub.key, subs.filter(_.key==sub.key).size, subs.size)
    val s1 = state.copy(subscriptions=subs)
      
    val s2 = {
      if (s1.subMgrs.contains(sub.key)) {
        log.trace("Already a subscription manager for {}", sub.key)
        s1
      } else {
        //Setup subscription manager
        log.trace("Need to set up a new subscription manager for {}", sub.key)
        val mgr = Spawner.start(new XBeeSubscriptionManager {
          override val key = sub.key
          override val xbee = state.xbee
          override def onNoAckReceived = markMaybeGone(key.xbee)
          override def publish(data: Seq[Byte]) = internalPublish(key, data)
        }, SpawnAsRequiredChild)
        val dist = s1.distributor.add(sub.key.xbee)(mgr.handlerProcess)
        val nsm = s1.subMgrs.updated(sub.key, mgr)
        s1.copy(subMgrs = nsm, distributor = dist)
      }
    }

    val unsub = () => internalUnsubscribe(sub)
    (unsub, s2)
  }}
  protected def internalUnsubscribe(sub: Subscription) = cast { state => {
    val subs = state.subscriptions.filterNot(_ == sub)
    log.trace("Unsubscribe for {} ({} left for this key)", sub.key, subs.find(_.key==sub.key).size)
    val s1 = state.copy(subscriptions=subs)
    if (s1.subscriptions.find(_.key == sub.key).isEmpty) {
      //Stop the subscription manager since nobody is interested in that anymore
      log.trace("No more subscriptions for manager {}. Terminating it.", sub.key)
      s1.subMgrs.get(sub.key).foreach_cps(_.terminate)
      s1.copy(subMgrs = s1.subMgrs - sub.key)
    } else s1
  }}
  protected def internalPublish(sub: SubscriptionKey, data: Seq[Byte]) = cast { state => {
    log.trace("Publishing data for {} to enlisted subscribers", sub)
    state.subscriptions.view.filter(_.key == sub).foreach_cps(_.handler(data))
    state
  }}

  protected def internalGetState(forAvieul: XBeeAddress) = get { state =>
    state.avieuls.get(forAvieul)
  }

  /**
   * If we could not send data to an xbee then try to reach it and if it does
   * not respond within the refreshTimeout remove it from the list.
   */
  protected [this] def markMaybeGone(address: XBeeAddress) = concurrent { state =>
    state.avieuls.get(address) match {
      case Some(avst) =>
        log.debug("Marking {} as maybe gone, trying to reach it", address)
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
  protected def childReceiver[A](forXBee: XBeeAddress, serviceIndex: Byte)(body: => A @process):
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
  protected class AvieulProxy(val address: XBeeAddress) extends Avieul {
    override val id = address.asHex
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
    override def toString = "Avieul("+id+")"
  }


  /**
   * XBee that acts as an avieul.
   */
  protected class XBeeAvieul(adr: XBeeAddress, serviceDefs: Seq[(Byte,Int,Byte)], xbee: LocalXBee) {
    val address = adr
    val services = serviceDefs.map { sd =>
      val (sindex, stype, sversion) = sd
      new XBeeAvieulService {
        override val id = adr.asHex+"."+stype
        override val serviceType = stype
        override val version = ServiceVersion(sversion)
        override val index = sindex
        override val xbee = XBeeAvieul.this.xbee
        override val address = adr
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
    override def toString = "XBeeAvieul("+address.asHex+")"
  }

  /**
   * Service offered by an avieul
   */
  protected trait XBeeAvieulService extends AvieulService {
    val address: XBeeAddress
    val xbee: LocalXBee
    val index: Byte
    override def providedBy = replyInCallerProcess(proxy(address))
    protected def sendTo(data: Seq[Byte]) = {
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
}

object PassadiXBee {
  def apply(xbee: => LocalXBee @process, as: SpawnStrategy = SpawnAsRequiredChild) = {
    val passadi = new PassadiXBee {
      override def openXBee = xbee
    }
    Spawner.start(passadi, as)
  }
}

