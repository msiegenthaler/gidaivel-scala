package ch.inventsoft
package gidaivel
package agents

import scalabase.oip._
import scalabase.process._
import scalabase.binary.BytesParsing._
import scalabase.extcol.ListUtil._
import scalabase.log._
import scalabase.time._
import scalaxmpp._
import net.liftweb.json.JsonAST._
import Json._


/**
 * Receiver for IR-Commands.
 */
trait IRReceiver extends AvieulBasedDevice with Log {
  protected case class State(friends: Seq[JID], protocols: Seq[IRProtocol]) {
    def withFriends(friends: Seq[JID]) = copy(friends=friends)
    def persistent: JValue = seqOf(Jid).serialize(friends)
  }
  //TODO save the protocols to the store

  protected override def init(stored: JValue) = {
    val f = seqOf(Jid).parse(stored).getOrElse(Nil)
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

    val ps = List(SonyIRProtocol) // TODO temp code
    loadProtocols
    State(f, ps)
  }
  override def shutdown = {
    log.debug("Shutting down IRReceiver {}", avieul.id)
    stopAndWait.receive
  }

  import IRReceiverParsing._

  protected def handleDetected(data: Seq[Byte]) = data match {
    case detected((index, command), Nil) =>
      log.info("Detected an IR-Command from {}: {}", index, command)
    case other =>
      log.info("Received unknown: {}", byteListToHex(other))
  }

  protected def loadProtocols = concurrent { state =>
    log.info("Initializing service with {} protocols", state.protocols.size)
    unloadAllProtocols
    state.protocols.foldLeft_cps(0) { (i, p) => 
      loadProtocol(i.toByte, p)
      i + 1
    }
    noop
  }
  protected def loadProtocol(id: Byte, protocol: IRProtocol) = protocol match {
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

  override def toString = "IRReceiver("+avieulService.id+")"
}

private[agents] object IRReceiverParsing {
  /** device-id, command */
  val detected = <<( byte, long )>>
  
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


sealed trait IRProtocol

/**
 * Protocol for IR-commands with a fixed length (in bits) and "single-bit" IR sequences.
 */
trait SingleBitFixedLengthIRProtocol extends IRProtocol {
  /** number of times a command is repeated */
  val repeats: Int
  /** pulses sent before each command */
  val preample: Seq[Duration]
  /** number of bits per command */
  val bitCount: Int
  /** pulses representing a binary 1 */
  val bitOne: Seq[Duration]
  /** pulses representing a binary 0 */
  val bitZero: Seq[Duration]
  /** pulses sent after each command */
  val suffix: Seq[Duration]
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
