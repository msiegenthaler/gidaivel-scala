package ch.inventsoft.gidaivel.avieul.xbee

import org.scalatest._
import matchers._
import ch.inventsoft.scalabase.process._
import Messages._
import ch.inventsoft.scalabase.oip._
import ch.inventsoft.scalabase.time._
import ch.inventsoft.xbee._
import ch.inventsoft.gidaivel.avieul._
import AvieulProtocol._
import scala.concurrent.SyncVar


class PassadiDAvieulsXBeeSpec extends ProcessSpec with ShouldMatchers {

  describe("Passadi d'avieuls XBee") {
    it_("should discover 2 avieuls if two are available") {
      val (passadi,xbee) = init
      val avieul1 = addAvieul(xbee)
      val avieul2 = addAvieul(xbee)
      avieul1.addHandler(requestInfo(Nil))
      avieul2.addHandler(requestInfo(Nil))
      sleep(500 ms)

      val avieuls = receiveWithin(1000 ms) { passadi.findAvieuls }
      avieuls match {
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

      val avieuls = receiveWithin(1000 ms) { passadi.findAvieuls }
      avieuls should be(Nil)
      
      stop(xbee, passadi)
    }
    it_("should discover one offered service") {
      val (passadi,xbee) = init
      val avieul1 = addAvieul(xbee)
      avieul1.addHandler(requestInfo((31, 1.toByte) :: Nil))
      sleep(500 ms)

      val services = receiveWithin(1000 ms) { passadi.findServices }
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

      val services = receiveWithin(1 s)(passadi.findServices)
      services.size should be(3)
      services.filter(_.serviceType == 40).size should be(1)
      services.filter(_.serviceType == 41).size should be(1)
      services.filter(_.serviceType == 55).size should be(1)

      val avieuls = receiveWithin(1 s)(passadi.findAvieuls)
      avieuls.size should be(2)
      avieuls.foreach( a => a.services.size should be >= (1) )
      val s2 = avieuls.flatMap(_.services)
      s2.size should be(3)

      stop(xbee, passadi)
    }

    it_("should support calling a service") {
      val (passadi,xbee) = init
      val avieul1 = addAvieul(xbee)
      avieul1.addHandler(requestInfo((10,1.toByte) :: Nil))
      sleep(500 ms)

      val service = receiveWithin(1 s)(passadi.findServices).head

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
    it_("should support requesting a service") {
      val (passadi,xbee) = init
      val avieul1 = addAvieul(xbee)
      avieul1.addHandler(requestInfo((10,1.toByte) :: Nil))
      sleep(500 ms)
      
      val service = receiveWithin(1 s)(passadi.findServices).head

      avieul1.addHandler {
	case ServiceRequest((0, 12, data), Nil) => avieul => {
	  val res = data.foldLeft(0)(_ + _) :: Nil map(_.toByte)
	  avieul.outgoingMessage(ServiceResponse(0, 12, res))
	}
      }
      val data = 1 :: 2 :: Nil map(_.toByte)
      val res = receiveWithin(1 s)(service.request(12, data))
      res match {
	case Left(response) =>
	  response should be(3 :: Nil map(_.toByte))
	case Right(_) => fail
      }
      stop(xbee, passadi)
    }
  }

  def requestInfo(services: List[(Int,Byte)]): PartialFunction[Seq[Byte],MockAvieul=>Unit] = {
    case RequestInfo((), Nil) => avieul => {
      val counter = new java.util.concurrent.atomic.AtomicInteger(-1)
      val serviceData = services.map(t => (counter.incrementAndGet.toByte, t._1, t._2))
      val bytes = AnnounceService(serviceData)
      avieul.outgoingMessage(bytes)
    }
  }

  private val addressSource = new java.util.concurrent.atomic.AtomicLong()
  def addAvieul(xbee: LocalXBeeMock) = {
    val avieul = MockAvieul()
    xbee.addRemote(avieul)
    avieul
  }
  class MockAvieul(override val address: XBeeAddress64) extends Avieul with RemoteXBee with StateServer[MockAvieulState] {
    protected[this] override def initialState = MockAvieulState(Nil, None, Nil)
    override def stop = cast_( state => None )
    override val services = Nil
    override def incomingMessage(msg: Seq[Byte]) = cast { state =>
      val avieul = this
      state.handlers.reverse.find(_.isDefinedAt(msg)) match {
	case Some(handler) =>
	  handler(msg)(avieul)
	  state.withHandlers(state.handlers.filterNot(_ == handler))
	case None =>
	  state.withIn(msg :: state.in)
      }
    }
    override def setProcessor(processor: Option[Process]) = cast { state => state.withProcessor(processor) }
    override def outgoingMessage(msg: Seq[Byte]) = cast { state =>
      state.processor.foreach(_ ! RemoteXBeeMessage(address, msg))
      state
    }
    def addHandler(handler: PartialFunction[Seq[Byte],MockAvieul => Unit]) = cast { state =>
      val avieul = this
      state.in.reverse.find(in => handler.isDefinedAt(in)) match {
	case Some(in) =>
	  handler(in)(avieul)
	  state
	case None => state.withHandlers(handler :: state.handlers)
      }
    }
  }
  case class MockAvieulState(in: List[Seq[Byte]], processor: Option[Process], handlers: List[PartialFunction[Seq[Byte],MockAvieul => Unit]]) {
    def withIn(in: List[Seq[Byte]]) = MockAvieulState(in, processor, handlers)
    def withProcessor(processor: Option[Process]) = MockAvieulState(in, processor, handlers)
    def withHandlers(handlers: List[PartialFunction[Seq[Byte],MockAvieul => Unit]]) = MockAvieulState(in, processor, handlers)
  }
  object MockAvieul extends SpawnableCompanion[MockAvieul] {
    def apply() = {
      start(SpawnAsRequiredChild)(new MockAvieul(XBeeAddress64(addressSource.incrementAndGet)))
    }
  }

  def init = {
    val xbee = LocalXBeeMock()
    val passadi = PassadiDAvieulsXBee(xbee, SpawnAsRequiredChild)
    sleep(100 ms)
    (passadi, xbee)
  }
  def stop(xbee: LocalXBee, passadi: PassadiDAvieulsXBee) = {
    passadi.close
    xbee.close
  }

  trait RemoteXBee {
    val address: XBeeAddress64
    def incomingMessage(msg: Seq[Byte]): Unit
    def outgoingMessage(msg: Seq[Byte]): Unit
    def setProcessor(processor: Option[Process]): Unit
    def stop: Unit
  }
  case class RemoteXBeeMessage(from: XBeeAddress, data: Seq[Byte])
  class LocalXBeeMock extends LocalXBee with StateServer[LocalXBeeMockState] {
    def addRemote(remote: RemoteXBee) = cast { state =>
      remote.setProcessor(Some(self))
      remote.incomingMessage(RequestInfo())
      state.withRemotes(remote :: state.remotes)
    }
    protected[this] override def initialState = LocalXBeeMockState(Nil, None)
    protected[this] override def messageHandler(state: LocalXBeeMockState) = {
      case RemoteXBeeMessage(from, data) =>
	state.processor.foreach(_ ! XBeeDataPacket(this, from, None, false, data))
	Some(state)
    }
    override def address = get { state => XBeeAddress64(1234L) }
    override def alias = get { state => None }
    override def alias(alias: Option[XBeeAddress16]) = cast { state => state }
    override val maxDataPerPacket = 100
    override def close = cast_ { state => 
      state.remotes.foreach(_.stop)
      None
    }
    override def sendPacket(to: XBeeAddress, data: Seq[Byte]) = cast { state =>
      state.remotes.filter(_.address == to).foreach(_.incomingMessage(data))
      state
    }
    override def sendTrackedPacket(to: XBeeAddress, data: Seq[Byte]) = call { state =>
      val count = state.remotes.filter(_.address == to).foldLeft(0)((s,r) => {
	r.incomingMessage(data)
	s+1
      })
      if (count>0) (TransmitStatusSuccess, state) else (TransmitStatusNoAckReceived, state)
    }
    override def broadcastPacket(data: Seq[Byte]) = cast { state =>
      state.remotes.foreach(_.incomingMessage(data))
      state
    }
    override def discover(timeout: Duration = 2500 ms) = call { state =>
      val discovered = state.remotes.map(r => DiscoveredXBeeDevice(r.address, None, None))
      (discovered, state)
    }		      
    override def incomingMessageProcessor(processor: Option[Process]) = cast { state =>
      state.withProcessor(processor)
    }
  }
  case class LocalXBeeMockState(remotes: List[RemoteXBee], processor: Option[Process]) { 
    def withRemotes(remotes: List[RemoteXBee]) = LocalXBeeMockState(remotes, processor)
    def withProcessor(processor: Option[Process]) = LocalXBeeMockState(remotes, processor)
  }
  object LocalXBeeMock extends SpawnableCompanion[LocalXBeeMock] {
    def apply() = start(SpawnAsRequiredChild)(new LocalXBeeMock)
  }
}
