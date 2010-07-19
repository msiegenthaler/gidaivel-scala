package ch.inventsoft.gidaivel

import ch.inventsoft.scalabase.binary._
import ch.inventsoft.scalabase.time._
import ch.inventsoft.scalabase.oip._
import ch.inventsoft.scalabase.process._
import ch.inventsoft.gidaivel.avieul._
import Messages._



trait IRDevice extends GidaivelDevice {
  val profile: IRDeviceProfile
}



case class IRCommand(device: IRDevice, command: Long)

sealed trait IRDeviceProfile
trait SingleBitFixedLengthDeviceProfile extends IRDeviceProfile {
  val repeats: Short
  val preample: Seq[Duration]
  val bits: Short
  val bitOne: Seq[Duration]
  val bitZero: Seq[Duration]
  val suffix: Seq[Duration]
}

case class DeviceCouldNotBeAdded() extends ServiceProblem

/**
 * Infrared command receiver.
 */
class IRReceiver(override protected[this] val service: AvieulService) extends AvieulDevice { // with StateServer[IRReceiverState] {
  override val serviceType = 0x10L

  //TODO reset on init (remove all devices from the service)
  
  //TODO Test!

  protected[this] def addDevice(device: IRDevice): MessageSelector[Either[Unit,GidaivelError]] = {
    //TODO code that gets a new device id and adds the device to the state
    device.profile match {
      case profile: SingleBitFixedLengthDeviceProfile =>
        val id: Byte = 0 //TODO find the id to assign
        val bitLen = profile.bitOne.size max profile.bitZero.size
        val (unit, pulses) = IRReceiver.durationConvert(Microseconds)(profile.preample, profile.bitZero.take(bitLen), profile.bitOne.take(bitLen), profile.suffix)
        val unit_low = (unit & 0xFF).toByte
        val unit_high = ((unit >> 8) & 0xFF).toByte
        val header = (
          id :: unit_high :: unit_low :: b(profile.repeats) :: 
          b(profile.preample.length) :: b(bitLen) :: b(profile.suffix.length) ::
          b(profile.bits) :: Nil)
        val data = header ::: pulses
        // if (data.length > 99) //TODO check for overflow
        request(0x01, data)(_ match {
          case 0x01 :: rest => Left()
          case other => Right(DeviceCouldNotBeAdded())
        })
    }
  }
  
  def subscribe(handlerFun: IRCommand => Unit) = {
    val handler = (data: Seq[Byte]) => data match {
      case deviceId :: commandData =>
        val command = commandData.foldLeft(0L)((t, b) => (t << 8) | b)
        val device: IRDevice = null //TODO get the device
        handlerFun(IRCommand(device, command))
    }
    service.subscribe(0x0001, handler)
  }
  
  //TODO
  
  private def b(a: Short) = a.toByte
  private def b(a: Int) = a.toByte  
}
case class IRReceiverState(devices: List[IRDevice])


object IRReceiver {
  /**
   * Convert sequences of durations into a short "unit" (x times baseUnit) and a byte sequence with the same
   * durations converted to the "unit" (i. e. 320 us).
   * The unit is chosen automatically to minimize the error.
   * @return (unit length [in base unit], concated input [in units])
   */
  protected[gidaivel] def durationConvert(baseUnit: TimeUnit)(inputs: Seq[Duration]*): (Int, List[Byte]) = {
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
