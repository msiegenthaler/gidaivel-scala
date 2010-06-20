package ch.inventsoft.gidaivel

import ch.inventsoft.scalabase.binary._
import ch.inventsoft.scalabase.time._
import ch.inventsoft.scalabase.oip._
import ch.inventsoft.scalabase.process._
import ch.inventsoft.gidaivel.avieul._
import Messages._


trait IRDevice {
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
  
  /**
   * Convert sequences of durations into a short "unit" (x times baseUnit) and a byte sequence with the same
   * durations converted to the "unit" (i. e. 320 us).
   * The unit is chosen automatically to minimize the error.
   * @return (unit length [in base unit], concated input [in units])
   */
  private def durationConvert(baseUnit: TimeUnit)(inputs: Seq[Duration]*): (Int, List[Byte]) = {
    /**
     * Finds the optional unit so all the values can be represented as byte*unit (approx. least error).
     * Algorithm:
     *  - Split the difference between the smallest (except 0) and the largest value into 255 parts (spans)
     *  - Experiment by varying the part-size between 20 values around span
     *  - Choose the one that gives the least absolute difference (not squared)
     */
    def findOptimalUnit(all: Seq[Int]) = {
      def findLeastError(toTry: Seq[Int], values: Seq[Int], bestSoFar: Int = Int.MaxValue, bestValue: Int = 0): Int = {
        if (toTry.isEmpty) bestValue
        else {
          val current = toTry.head
          val error = values.foldLeft(0) { (t,e) =>
            t + ((e.toDouble / current).round * current).abs.toInt
          }
          if (error < bestSoFar) findLeastError(toTry.tail, values, error, current)
          else findLeastError(toTry.tail, values, bestSoFar, bestValue)
        }
      }
      val max = all.max
      val min = all.view.filter(_ > 0).min
      val span = (max - min / 255d).round.toInt 
      val candidate = (span-10).max(1).max(min) to (span+10).min(Int.MaxValue).min(max)
      findLeastError(candidate, all)      
    }    
    val all = inputs.foldLeft[Seq[Int]](Nil)((l,e) => l ++ e.map(_.amountAs(baseUnit).toInt))
    val unit = findOptimalUnit(all)
    val pulses = all.map(e => (e.toDouble / unit).round.toByte).toList
    (unit, pulses)
  }
  
  def addDevice(device: IRDevice): MessageSelector[Either[Unit,GidaivelError]] = {
    //TODO code that gets a new device id and adds the device to the state
    device.profile match {
      case profile: SingleBitFixedLengthDeviceProfile =>
        val id: Byte = 0 //TODO find the id to assign
        val bitLen = profile.bitOne.size max profile.bitZero.size
        val (unit, pulses) = durationConvert(Microseconds)(profile.preample, profile.bitZero.take(bitLen), profile.bitOne.take(bitLen), profile.suffix)
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