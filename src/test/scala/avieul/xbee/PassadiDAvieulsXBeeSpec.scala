package ch.inventsoft.gidaivel.avieul.xbee

import org.scalatest._
import matchers._
import ch.inventsoft.scalabase.process._
import Messages._
import ch.inventsoft.scalabase.oip._
import ch.inventsoft.scalabase.time._
import ch.inventsoft.xbee._

class PassadiDAvieulsXBeeSpec extends ProcessSpec with ShouldMatchers {

  describe("Passadi d'avieuls XBee") {
    it_("should discover all the avieuls") {
    }
    it_("should discover all the offered services") {
    }
  }

  trait RemoteXBee {
    val address: XBeeAddress64
    def sendMessage(msg: Seq[Byte]): Unit
  }
  class LocalXBeeMock extends LocalXBee with StateServer[LocalXBeeMockState] {
    def addRemote(remote: RemoteXBee) = cast { state =>
      state.withRemotes(remote :: state.remotes)
    }
    protected[this] override def initialState = LocalXBeeMockState(Nil, None)
    override def address = get { state => XBeeAddress64(1234L) }
    override def alias = get { state => None }
    override def alias(alias: Option[XBeeAddress16]) = cast { state => state }
    override val maxDataPerPacket = 100
    override def close = cast_ { state => None }
    override def sendPacket(to: XBeeAddress, data: Seq[Byte]) = cast { state =>
      state.remotes.filter(_.address == to).foreach(_.sendMessage(data))
      state
    }
    override def sendTrackedPacket(to: XBeeAddress, data: Seq[Byte]) = call { state =>
      val count = state.remotes.filter(_.address == to).foldLeft(0)((s,r) => {
	r.sendMessage(data)
	s+1
      })
      if (count>0) (TransmitStatusSuccess, state) else (TransmitStatusNoAckReceived, state)
    }
    override def broadcastPacket(data: Seq[Byte]) = cast { state =>
      state.remotes.foreach(_.sendMessage(data))
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
}
