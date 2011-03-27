package ch.inventsoft
package gidaivel
package passadi

import scalabase.process._
import scalabase.time._


/**
 * A lightweight device (i.e. based on arduino) that offers one or more
 * avieul service.
 */
trait Avieul extends Equals {
  /** unique identifier of this avieul */
  val id: String

  /** The services offered by this avieul */
  def services: Selector[Iterable[AvieulService]] @process

  /** The current status of this avieul. None if not active */
  def status: Selector[Option[AvieulStatus]] @process
}
trait AvieulStatus {
  /** last contact with this avieul */
  val lastContact: TimePoint
  /** time since we last heard from this avieul */
  def notHeardFromIn: Duration = TimePoint.current - lastContact
  /** quality of the signal received by this avieul */
  def quality: SignalQuality
}
trait SignalQuality extends Ordered[SignalQuality] with Equals {
  val dBm: Int
  /** 0 - 100: strength relative to expected maximum */
  val percentage: Int
  override def compare(to: SignalQuality) = {
    dBm.compare(to.dBm)
  }
  override def equals(that: Any) = that match {
    case that: SignalQuality =>
      that.canEqual(this) && dBm == that.dBm
    case other => false
  }
  override def canEqual(that: Any) = that.isInstanceOf[SignalQuality]
  override def hashCode = dBm.hashCode
  override def toString = dBm.toString + " dBm"
}

/**
 * A home automation service offered by an avieul device.
 */
trait AvieulService extends Equals {
  /** Unique identifier of this service (across all avieuls) */
  val id: String
  val serviceType: Int
  val version: ServiceVersion

  def providedBy: Selector[Avieul] @process

  /**
   * Call the service.
   */
  def call(callType: Short, payload: Seq[Byte]): Selector[Either[Unit,AvieulError]] @process

  /**
   * Request information from the service.
   * @return the response
   */
  def request(requestType: Short, payload: Seq[Byte]): Selector[Either[Seq[Byte],AvieulError]] @process

  /**
   * Subscribe to messages.
   * @return unsubscription function
   */
  def subscribe(subscriptionType: Short, handler: Seq[Byte] => Unit @process): Selector[()=>Unit @process] @process
}
object AvieulService {
  def unapply(service: AvieulService) = Some((service.serviceType, service.version))
}


/**
 * Version of an avieul service.
 */
class ServiceVersion private(protected val number: Int) extends Ordered[ServiceVersion] {
  override def compare(other: ServiceVersion) = number.compare(other.number)
  override def toString = number.toString
}
object ServiceVersion {
  def apply(number: Byte) = new ServiceVersion(number & 0xFF)
  def unapply(version: ServiceVersion) = {
    val b = version.number
    Some(b.toByte)
  }
}


sealed trait AvieulError
object TransmitFailed extends AvieulError {
  override def toString = "Transmit failed"
}
object UnknownAvieulService extends AvieulError {
  override def toString = "Unknown Avieul service"
}
object UnknownAvieulServiceRequest extends AvieulError {
  override def toString = "Unknown Avieul service request"
}
object UnknownAvieulServiceSubscription extends AvieulError {
  override def toString = "Unknown Avieul service subscription"
}
