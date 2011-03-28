package ch.inventsoft
package gidaivel
package passadi
package xbee


import scala.concurrent.SyncVar
import org.scalatest._
import matchers._
import scalabase.process._
import Messages._
import scalabase.oip._
import scalabase.time._
import ch.inventsoft.xbee._
import AvieulProtocol._


class PassadiDAvieulsXBeeSpec extends ProcessSpec with ShouldMatchers {
  describe("Passadi d'avieuls XBee") {
    it_("should discover 2 avieuls if two are available") {
      val (passadi,xbee) = init
      val avieul1 = addAvieul(xbee)
      val avieul2 = addAvieul(xbee)
      avieul1.addHandler(requestInfo(Nil))
      avieul2.addHandler(requestInfo(Nil))
      sleep(500 ms)

      val avieuls = passadi.avieuls.receiveWithin(1 s)
      avieuls.toList match {
	case a1 :: a2 :: Nil =>
	  a1 should not be(null)
	  a2 should not be(null)
	case x => fail("wrong count: "+x)
      }
      stop(xbee,passadi)
    }
    it_("should discover no avieuls if there are none") {
      val (passadi,xbee) = init
      sleep(500 ms)

      val avieuls = passadi.avieuls.receiveWithin(1 s)
      avieuls.size should be(0)
      
      stop(xbee, passadi)
    }
    it_("should discover one offered service") {
      val (passadi,xbee) = init
      val avieul1 = addAvieul(xbee)
      avieul1.addHandler(requestInfo((31, 1.toByte) :: Nil))
      sleep(500 ms)

      val services = passadi.services.receiveWithin(1 s).toList
      services match {
	case s1 :: Nil =>
	  s1 should not be(null)
	  s1.serviceType should be(31)
	case other => fail("Got "+other)
      }
      stop(xbee, passadi)
    }
    it_("should discover three services from two xbees") {
      val (passadi,xbee) = init
      val avieul1 = addAvieul(xbee)
      val avieul2 = addAvieul(xbee)
      avieul1.addHandler(requestInfo((40, 1.toByte) :: (41, 1.toByte) :: Nil))
      avieul2.addHandler(requestInfo((55, 3.toByte) :: Nil))
      sleep(500 ms)

      val services = passadi.services.receiveWithin(1 s)
      services.size should be(3)
      services.filter(_.serviceType == 40).size should be(1)
      services.filter(_.serviceType == 41).size should be(1)
      services.filter(_.serviceType == 55).size should be(1)

      val avieuls = passadi.avieuls.receiveWithin(1 s)
      avieuls.size should be(2)
      avieuls.foreach_cps { a =>
        val s = a.services.receiveWithin(1 s)
        s.size should be >= 1
      }
      val s2 = avieuls.flatMap_cps(_.services.receiveWithin(1 s))
      s2.size should be(3)

      stop(xbee, passadi)
    }

    it_("should support calling a service") {
      val (passadi,xbee) = init
      val avieul1 = addAvieul(xbee)
      avieul1.addHandler(requestInfo((10,1.toByte) :: Nil))
      sleep(500 ms)

      val service = passadi.services.receiveWithin(1 s).head

      val x = new SyncVar[Seq[Byte]]
      avieul1.addHandler {
	case ServiceCall((0, 13, data), Nil) =>
	  avieul => x.set(data)
      }
      val data = 1 :: 2 :: 3 :: Nil map(_.toByte)
      service.call(13, data)
      x.get(1000) should be(Some(data))

      stop(xbee, passadi)
    }
    it_("should return TransmitFailed on calling a service from an unavailable xbee") {
      val (passadi,xbee) = init
      val avieul1 = addAvieul(xbee)
      avieul1.addHandler(requestInfo((10,1.toByte) :: Nil))
      sleep(500 ms)

      val service = passadi.services.receiveWithin(1 s).head
      val data = 1 :: 2 :: Nil map(_.toByte)

      xbee.remove(avieul1)

      val res = receiveWithin(1 s)(service.call(12, data))
      res match {
	case Right(r) =>
	  r should be(TransmitFailed)
	case Left(_) => fail
      }
      stop(xbee, passadi)
    }

    it_("should support requesting a service") {
      val (passadi,xbee) = init
      val avieul1 = addAvieul(xbee)
      avieul1.addHandler(requestInfo((10,1.toByte) :: Nil))
      sleep(500 ms)

      val service = passadi.services.receiveWithin(1 s).head

      avieul1.addHandler {
	case ServiceRequest((0, 12, data), Nil) => avieul => {
	  val res = data.foldLeft(0)(_ + _) :: Nil map(_.toByte)
	  avieul.outgoingMessage(ServiceResponse(0, 12, res))
	}
      }
      val data = 1 :: 2 :: Nil map(_.toByte)
      val res = service.request(12, data).receiveWithin(1 s)
      res match {
	case Left(response) =>
	  response should be(3 :: Nil map(_.toByte))
	case Right(_) => fail
      }
      stop(xbee, passadi)
    }
    it_("should return TransmitFailed on requesting a service from an unavailable xbee") {
      val (passadi,xbee) = init
      val avieul1 = addAvieul(xbee)
      avieul1.addHandler(requestInfo((10,1.toByte) :: Nil))
      sleep(500 ms)

      val service = passadi.services.receiveWithin(1 s).head
      val data = 1 :: 2 :: Nil map(_.toByte)

      xbee.remove(avieul1)

      val res = service.request(12, data).receiveWithin(1 s)
      res match {
	case Right(r) =>
	  r should be(TransmitFailed)
	case Left(_) => fail
      }
      stop(xbee, passadi)
    }
    it_("should return UnknownAvieulService on requesting an unknown service") {
      val (passadi,xbee) = init
      val avieul1 = addAvieul(xbee)
      avieul1.addHandler(requestInfo((10,1.toByte) :: Nil))
      sleep(500 ms)

      avieul1.addHandler {
	case ServiceRequest((0, 12, data), Nil) => avieul => {
	  avieul.outgoingMessage(ServiceUnknown(0))
	}
      }
      val service = passadi.services.receiveWithin(1 s).head
      val data = 1 :: 2 :: Nil map(_.toByte)
      val res = service.request(12, data).receiveWithin(1 s)
      res match {
	case Right(r) =>
	  r should be(UnknownAvieulService)
	case Left(_) => fail
      }
      stop(xbee, passadi)
    }
    it_("should return UnknownAvieulServiceRequest sending an unsupported request to a service") {
      val (passadi,xbee) = init
      val avieul1 = addAvieul(xbee)
      avieul1.addHandler(requestInfo((10,1.toByte) :: Nil))
      sleep(500 ms)

      avieul1.addHandler {
	case ServiceRequest((0, 12, data), Nil) => avieul => {
	  avieul.outgoingMessage(ServiceRequestUnknown(0, 12))
	}
      }
      val service = passadi.services.receiveWithin(1 s).head
      val data = 1 :: 2 :: Nil map(_.toByte)
      val res = service.request(12, data).receiveWithin(1 s)
      res match {
	case Right(r) =>
	  r should be(UnknownAvieulServiceRequest)
	case Left(_) => fail
      }
      stop(xbee, passadi)
    }

    it_("should support subscribing to a service") {
      val (passadi,xbee) = init
      val avieul1 = addAvieul(xbee)
      avieul1.addHandler(requestInfo((10,1.toByte) :: Nil))
      sleep(500 ms)

      val service = passadi.services.receiveWithin(1 s).head

      avieul1.addHandler {
	case ServiceSubscribe((0, 12), Nil) => avieul => {
	  avieul.outgoingMessage(ServiceSubscriptionConfirm(0, 12))
	}
      }
      val data = 1 :: 2 :: Nil map(_.toByte)
      val counter = new java.util.concurrent.atomic.AtomicInteger
      def publishFun(data: Seq[Byte]): Unit = {
	counter.addAndGet(data.head)
      }
      val unsubscribe = service.subscribe(12, publishFun).receiveWithin(1 s)
      sleep(200 ms)

      counter.get should be(0)

      avieul1.outgoingMessage(ServicePublish(0, 12, 1 :: Nil map(_.toByte)))
      sleep(200 ms)
      counter.get should be(1)

      avieul1.outgoingMessage(ServicePublish(0, 12, 2 :: Nil map(_.toByte)))
      sleep(200 ms)
      counter.get should be(3)

      stop(xbee, passadi)
    }
    it_("publish from an unsubscribed service should not be received") {
      val (passadi,xbee) = init
      val avieul1 = addAvieul(xbee)
      avieul1.addHandler(requestInfo((10,1.toByte) :: Nil))
      sleep(500 ms)

      val service = passadi.services.receiveWithin(1 s).head

      avieul1.addHandler {
	case ServiceSubscribe((0, 12), Nil) => avieul => {
	  avieul.outgoingMessage(ServiceSubscriptionConfirm(0, 12))
	}
      }
      avieul1.addHandler {
	case ServiceUnsubscribe((0, 12), Nil) => avieul => {
	  ()
	}
      }
      val counter = new java.util.concurrent.atomic.AtomicInteger
      def publishFun(data: Seq[Byte]): Unit = {
	counter.addAndGet(data.head)
      }
      val unsubscribe = service.subscribe(12, publishFun).receiveWithin(1 s)

      counter.get should be(0)

      avieul1.outgoingMessage(ServicePublish(0, 12, 1 :: Nil map(_.toByte)))
      sleep(200 ms)
      counter.get should be(1)

      unsubscribe()
      sleep(200 ms)
      
      avieul1.outgoingMessage(ServicePublish(0, 12, 2 :: Nil map(_.toByte)))
      sleep(200 ms)
      counter.get should be(1)

      stop(xbee, passadi)
    }

    it_("should support subscribing to a service twice") {
      val (passadi,xbee) = init
      val avieul1 = addAvieul(xbee)
      avieul1.addHandler(requestInfo((10,1.toByte) :: Nil))
      sleep(500 ms)

      val service = passadi.services.receiveWithin(1 s).head

      avieul1.addHandler {
	case ServiceSubscribe((0, 12), Nil) => avieul => {
	  avieul.outgoingMessage(ServiceSubscriptionConfirm(0, 12))
	}
      }
      val counter1 = new java.util.concurrent.atomic.AtomicInteger
      def publishFun1(data: Seq[Byte]): Unit = {
	counter1.addAndGet(data.head)
      }
      val unsubscribe1 = receiveWithin(1 s)(service.subscribe(12, publishFun1))
      val counter2 = new java.util.concurrent.atomic.AtomicInteger
      def publishFun2(data: Seq[Byte]): Unit = {
	counter2.addAndGet(data.head)
      }
      val unsubscribe2 = service.subscribe(12, publishFun2).receiveWithin(1 s)

      counter1.get should be(0)
      counter2.get should be(0)

      avieul1.outgoingMessage(ServicePublish(0, 12, 1 :: Nil map(_.toByte)))
      sleep(200 ms)
      counter1.get should be(1)
      counter2.get should be(1)

      avieul1.outgoingMessage(ServicePublish(0, 12, 2 :: Nil map(_.toByte)))
      sleep(200 ms)
      counter1.get should be(3)
      counter2.get should be(3)

      stop(xbee, passadi)
    }
    it_("should not block on subscribing a service from an unavailable xbee") {
      val (passadi,xbee) = init
      val avieul1 = addAvieul(xbee)
      avieul1.addHandler(requestInfo((10,1.toByte) :: Nil))
      sleep(500 ms)

      val service = passadi.services.receiveWithin(1 s).head

      xbee.remove(avieul1)

      val unsubscribe = service.subscribe(12, a => ()).receiveWithin(1 s)
      unsubscribe()
      stop(xbee, passadi)
    }
    it_("should support subscribing to a at that time not available xbee") {
      val (passadi,xbee) = init

      val avieul1 = addAvieul(xbee)
      avieul1.addHandler(requestInfo((10,1.toByte) :: Nil))
      avieul1.addHandler(requestInfo((10,1.toByte) :: Nil))
      sleep(500 ms)

      val service = passadi.services.receiveWithin(1 s).head

      xbee.remove(avieul1)
      sleep(200 ms)

      avieul1.addHandler {
	case ServiceSubscribe((0, 12), Nil) => avieul => {
	  avieul.outgoingMessage(ServiceSubscriptionConfirm(0, 12))
	}
      }
      avieul1.addHandler {
	case ServiceUnsubscribe((0, 12), Nil) => avieul => {
	  ()
	}
      }
      val counter = new java.util.concurrent.atomic.AtomicInteger
      def publishFun(data: Seq[Byte]): Unit = {
	counter.addAndGet(data.head)
      }
      val unsubscribe = service.subscribe(12, publishFun).receiveWithin(1 s)

      counter.get should be(0)

      avieul1.outgoingMessage(ServicePublish(0, 12, 1 :: Nil map(_.toByte)))
      sleep(200 ms)
      counter.get should be(0)

      xbee.addRemote(avieul1)
      sleep(200 ms)

      avieul1.outgoingMessage(ServicePublish(0, 12, 1 :: Nil map(_.toByte)))
      sleep(200 ms)
      counter.get should be(1)

      unsubscribe()
      stop(xbee, passadi)
    }
  }

  def requestInfo(services: List[(Int,Byte)]): PartialFunction[Seq[Byte],MockAvieul=>Unit @process] = {
    case RequestInfo((), Nil) => avieul => {
      val counter = new java.util.concurrent.atomic.AtomicInteger(-1)
      val serviceData = services.map(t => (counter.incrementAndGet.toByte, t._1, t._2))
      val bytes = AnnounceServices(serviceData)
      avieul.outgoingMessage(bytes)
    }
  }

  private val addressSource = new java.util.concurrent.atomic.AtomicLong()
  def addAvieul(xbee: LocalXBeeMock) = {
    val avieul = MockAvieul()
    xbee.addRemote(avieul)
    avieul
  }
  class MockAvieul(override val address: XBeeAddress64) extends Avieul with RemoteXBee with StateServer {
    override val id = address.toString
    protected override type State = MockAvieulState
    case class MockAvieulState(in: List[Seq[Byte]],
                               processor: Option[Process],
                               handlers: List[PartialFunction[Seq[Byte],MockAvieul => Unit @process]])
    protected override def init = MockAvieulState(Nil, None, Nil)
    override def stop = super.stop
    override def services = replyInCallerProcess(Nil)
    override def status = replyInCallerProcess(Some(new AvieulStatus {
      override val lastContact = TimePoint.current
      override def quality = new SignalQuality {
    	  override val percentage = 90
    	  override val dBm: Int = -30
      }
    }))
    override def incomingMessage(msg: Seq[Byte]) = cast { state =>
      val avieul = this
      state.handlers.reverse.find(_.isDefinedAt(msg)) match {
	case Some(handler) =>
	  handler(msg)(avieul)
	  state.copy(handlers = state.handlers.filterNot(_ == handler))
	case None =>
	  state.copy(in = msg :: state.in)
      }
    }
    override def setProcessor(processor: Option[Process]) = cast { state => state.copy(processor = processor) }
    override def outgoingMessage(msg: Seq[Byte]) = cast { state =>
      state.processor.foreach_cps(_ ! RemoteXBeeMessage(address, msg))
      state
    }
    def addHandler(handler: PartialFunction[Seq[Byte],MockAvieul => Unit @process]) = cast { state =>
      val avieul = this
      state.in.reverse.find(in => handler.isDefinedAt(in)) match {
	case Some(in) =>
	  handler(in)(avieul)
	  state
	case None => state.copy(handlers = handler :: state.handlers)
      }
    }
    override def equals(o: Any) = o match {
      case o: MockAvieul =>
        if (o.canEqual(this)) address == o.address
        else false
      case other => false
    }
    override def canEqual(that: Any) = that.isInstanceOf[MockAvieul]
    override def hashCode = address.hashCode
    override def toString = "MockAvieul"
  }
  object MockAvieul {
    def apply(): MockAvieul @process = apply(XBeeAddress64(addressSource.incrementAndGet))
    def apply(address: XBeeAddress64): MockAvieul @process  = {
      Spawner.start(new MockAvieul(address), SpawnAsRequiredChild)
    }
  }

  def init = {
    val xbee = LocalXBeeMock()
    val passadi = PassadiXBee(xbee, SpawnAsRequiredChild)
    sleep(100 ms)
    (passadi, xbee)
  }
  def stop(xbee: LocalXBee, passadi: Passadi) = {
    passadi.close.await
  }

  trait RemoteXBee {
    val address: XBeeAddress64
    def incomingMessage(msg: Seq[Byte]): Unit @process
    def outgoingMessage(msg: Seq[Byte]): Unit @process
    def setProcessor(processor: Option[Process]): Unit @process
    def stop: Unit @process
  }
  case class RemoteXBeeMessage(from: XBeeAddress, data: Seq[Byte])
  class LocalXBeeMock extends LocalXBee with StateServer {
    protected override type State = LocalXBeeMockState
    protected case class LocalXBeeMockState(remotes: List[RemoteXBee], 
                                            handler: ReceivedXBeeDataPacket => Unit @process)    
    def addRemote(remote: RemoteXBee) = cast { state =>
      remote.setProcessor(Some(self))
      remote.incomingMessage(RequestInfo())
      state.copy(remotes = remote :: state.remotes)
    }
    def remove(remote: RemoteXBee) = cast { state =>
      state.copy(remotes = state.remotes.filterNot(_ == remote))
    }
    protected override def init = LocalXBeeMockState(Nil, _ => noop)
    protected override def handler(state: LocalXBeeMockState) = super.handler(state).orElse_cps {
      case RemoteXBeeMessage(from, data) =>
        val packet = ReceivedXBeeDataPacket(from, None, false, data)
	state.handler(packet)
	Some(state)
    }
    protected override def termination(state: State) = state.remotes.foreach_cps(_.stop)
    override def address = get { state => XBeeAddress64(1234L) }
    override def alias = get { state => None }
    override def alias(alias: Option[XBeeAddress16]) = call { state => ((), state) }
    override val maxDataPerPacket = 100
    override def close = stopAndWait
    override def send(to: XBeeAddress, data: Seq[Byte]) = cast { state =>
      state.remotes.filter(_.address == to).foreach_cps(_.incomingMessage(data))
      state
    }
    override def sendTracked(to: XBeeAddress, data: Seq[Byte]) = call { state =>
      val count = state.remotes.filter(_.address == to).foldLeft_cps(0)((s,r) => {
	r.incomingMessage(data)
	s+1
      })
      if (count>0) (TransmitStatusSuccess, state) else (TransmitStatusNoAckReceived, state)
    }
    override def broadcast(data: Seq[Byte]) = cast { state =>
      state.remotes.foreach_cps(_.incomingMessage(data))
      state
    }
    override def discover(timeout: Duration = 2500 ms) = call { state =>
      val discovered = state.remotes.map(r => DiscoveredXBeeDevice(r.address, None, None))
      (discovered, state)
    }		      
    override def setMessageHandler(handler: ReceivedXBeeDataPacket => Unit @process) = cast { state =>
      state.copy(handler = handler)
    }
  }

  object LocalXBeeMock {
    def apply() = Spawner.start(new LocalXBeeMock, SpawnAsRequiredChild)
  }
}
