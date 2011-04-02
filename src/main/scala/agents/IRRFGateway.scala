package ch.inventsoft
package gidaivel
package agents

import scala.xml._
import scalabase.oip._
import scalabase.process._
import scalabase.binary.BytesParsing._
import scalabase.extcol.ListUtil._
import scalabase.log._
import scalabase.time._
import scalaxmpp._
import scalaxmpp.component._
import net.liftweb.json.JsonAST._


/**
 * Gateway to infrared and rf. Sends and receives rc-commands.
 */
trait IRRFGateway extends AvieulBasedDevice with Log {
  protected case class State(friends: Seq[JID], protocols: Seq[IRProtocol]) {
    def protocolIndex(name: String) = {
      val i = protocols.indexWhere(_.name==name)
      if (i != -1) Some(i) else None
    }
    def withFriends(friends: Seq[JID]) = copy(friends=friends)
    def persistent: JValue = JObject(List(
      StateMapper.friends.serialize(friends),
      StateMapper.protocols.serialize(protocols)
    ))
  }

  private object StateMapper {
    import Json._
    val friends = fromObject("friends", seqOf(Jid))
    val protocols = fromObject("protocols", seqOf(XmlElem.map(IRProtocol.fromXml _, IRProtocol.toXml _)))
  }

  protected override def init(stored: JValue) = {
    val f = StateMapper.friends.parse(stored).getOrElse(Nil)
    val p = StateMapper.protocols.parse(stored).getOrElse(Nil)

    log.debug("Initializing IRReceiver {}", avieul.id)
    ResourceManager[() => Unit @process](
      resource = {
        log.trace("Setting up subscription for received IR events")
        device.subscribe(0x0001, handleDetected _)
      },
      close = unsub => {
        log.trace("Removing subscription for received IR events")
        unsub()
      }
    ).receive

    loadProtocols
    State(f, p)
  }
  override def shutdown = {
    log.debug("Shutting down IRReceiver {}", avieul.id)
    stopAndWait.receive
  }
  import IRReceiverParsing._
  protected val namespace = "urn:gidaivel:irrfgateway"

  protected override def message = super.message :+ sendCommand
  protected override def iqSet = super.iqSet :+ loadProtocol

  protected val sendCommand = mkMsg {
    case (FirstElem(e @ ElemName("send-command", `namespace`)),state) =>
      val cmd = for {
        name <- (e \ "protocol").headOption.map(_.text)
        Long(cmd) <- (e \ "command").headOption.map(_.text)
        protocolId <- state.protocolIndex(name)
      } yield {
        command(protocolId.toByte, cmd)
      }
      cmd.foreach_cps { command =>
        device.call(0x0010, command)
      }
  }
  object Long {
    def unapply(string: String) = try {
      Some(string.toLong)
    }  catch {
      case _: NumberFormatException => None
    }
  }

  protected val loadProtocol = mkIqSet {
    case (set @ FirstElem(e @ ElemName("load-protocol", `namespace`)),state) =>
      FirstElem.firstElem(e \ "protocol").flatMap(IRProtocol.fromXml(_)) match {
        case Some(protocol) =>
          val id = addProtocol(protocol).receive
          id match {
            case Some(id) =>
              device_loadProtocol(id.toByte, protocol)
              set.resultOk(<ok/>)
            case None =>
              noop
              set.resultError(<rejected/>)
          }
        case None => set.resultError(StanzaError.badRequest)
      }
  }
  private def addProtocol(protocol: IRProtocol) = call { state =>
    val oi = state.protocols.indexWhere(_.name == protocol.name)
    val id = if (oi == -1) {
      val l = state.protocols.length
      if (l > 255) None else Some(l.toByte)
    } else Some(oi.toByte)
    val ps = id.map(i => state.protocols.updated(i, protocol)).getOrElse(state.protocols)
    saveState //we change the state, so save it
    (id, state.copy(protocols = ps))
  }

  protected override def status(state: State) = {
    val status = <status>{state.protocols.length} protocols loaded</status>
    val protocols = state.protocols.map(p => <protocol>{IRProtocol.toXml(p)}</protocol>)
    Status(<show>chat</show> ++ status ++ <protocols xmlns={namespace}>{protocols}</protocols>)
  }

  protected def handleDetected(data: Seq[Byte]) = concurrent { state => data match {
    case command((index, command), Nil) =>
      log.debug("Detected an IR-Command from {}: {}", index, command)
      state.protocols.drop(index).headOption.map(_.name).foreach_cps { name =>
        val content = <command-received xmlns={namespace}><protocol>{name}</protocol><command>{command}</command></command-received>
        state.friends.foreach_cps { friend =>
          val msg = MessageSend(None, None, services.jid, friend, content)
          services.send(msg)
        }
      }
    case other =>
      log.info("Received unknown data: {}", byteListToHex(other))
  }}

  protected def loadProtocols = concurrent { state =>
    log.info("Initializing service with {} protocols", state.protocols.size)
    unloadAllProtocols
    state.protocols.foldLeft_cps(0) { (i, p) => 
      device_loadProtocol(i.toByte, p)
      i + 1
    }
    noop
  }
  protected def device_loadProtocol(id: Byte, protocol: IRProtocol) = protocol match {
    case protocol: SingleBitFixedLengthIRProtocol =>
      implicit def intToByte(int: Int) = {
        if (int > 255) throw new IllegalArgumentException("byte overflow ("+int+" is bigger then 255)")
        int.toByte
      }
      val (unitUs, data) = IRProtocolUtils.durationConvert(Microseconds)(
        protocol.preample, protocol.bitZero, protocol.bitOne, protocol.suffix)
      val cmd = load_command_sf(
        (id, unitUs.toShort, protocol.repeats, 
         (protocol.preample.length, protocol.bitOne.length, protocol.suffix.length),
         protocol.bitCount, data)
      )
      val answer = device.request(0x0001, cmd)
      if (answer.length != 2 || answer(0) != id) throw new RuntimeException("unexpected answer to loadProtocol")
      if (answer(1) != 0x01) throw new RuntimeException("Loading of protocol "+protocol+" failed")
      log.info("Successfully loaded protocol {} to index {}", protocol, id)
      noop
  }
  protected def unloadAllProtocols = {
    log.info("Unloading all protocols")
    device.call(0x0002)
  }

  override def toString = "IRRFGateway("+avieulService.id+")"
}

private[agents] object IRReceiverParsing {
  /** device-id, command */
  val command = <<( byte, long )>>
  
  /* device id (1 byte): (device local) id of the IR-device (0-254)
   * unit in us (2 bytes): units used in the rest of the message in microseconds (0-65535)
   * repeats (1 byte): number of time the request is repeated
   * preample length (1 byte): number of pulses in the preample
   * bit length (1 byte): number of pulses for a bit
   * suffix length (1 byte): number of pulses in the suffix. (preample + 2*bit + suffix <= 89)
   * bit count (1 byte): Number of bits in a command (1-64)
   * preample pulse (1 byte)*: [ exactly preample length times ]
      pulses in units that make up a "zero" bit (0-25%). The "detection" starts with a low state.
   * bit "zero" pulse (1 byte)*: [ exactly bit length times ]
      pulses in units that make up a "zero" bit
   * bit "one" pulse (1 byte)*: [ exactly bit length times ]
      pulses in units that make up a "one" bit
   * suffix pulse (1 byte)*: [ exactly suffix length times ]
       pulses in units that make up the suffix
   */
  val load_command_sf_lengths = <<( byte, byte, byte )>>
  val load_command_sf = <<( byte, short, byte, load_command_sf_lengths, byte, list_to_end(byte) )>>
}

private[agents] object IRProtocolUtils {
  /**
   * Convert sequences of durations into a short "unit" (x times baseUnit) and a byte sequence with the same
   * durations converted to the "unit" (i. e. 320 us).
   * The unit is chosen automatically to minimize the error.
   * @return (unit length [in base unit], concated input [in units])
   */
  def durationConvert(baseUnit: TimeUnit)(inputs: Seq[Duration]*): (Int, List[Byte]) = {
    /**
     * Finds the optional unit so all the values can be represented as byte*unit (approx. least error).
     * Algorithm:
     *  - take largest value and divide by 255 to find minimum unit size => c
     *  - Experiment by varying the part-size between 20 values around c
     *  - Choose the one that gives the least squared difference (not squared)
     */
    def findOptimalUnit(all: Seq[Int]) = {
      def findLeastError(toTry: List[Int], values: Seq[Int], bestSoFar: Int = Int.MaxValue, bestValue: Int = 0): Int = toTry match {
        case current :: rest =>
          val error = values.foldLeft(0) { (t,e) =>
            val byteVal = (e.toDouble / current).round.min(255).max(0)
            val delta = e - byteVal*current
            t + delta.toInt.abs
          }
          if (error == 0) current
          else if (error < bestSoFar) findLeastError(rest, values, error, current)
          else findLeastError(rest, values, bestSoFar, bestValue)
        case Nil => bestValue
      }
      val max = all.max
      val min = all.foldLeft(0)((m,e) => if (e>0 && (e<m || m==0)) e else m)
      val c = (max / 255d).round.toInt
      val candidates = (c).max(1).min(min) to (c+20).min(Int.MaxValue).min(max)
      findLeastError(candidates.reverse.toList, all)      
    }    
    val all = inputs.foldLeft[Seq[Int]](Nil)((l,e) => l ++ e.map(_.amountAs(baseUnit).toInt))
    val unit = findOptimalUnit(all)
    val pulses = all.map(e => (e.toDouble / unit).round.toByte).toList
    (unit, pulses)
  }
}
