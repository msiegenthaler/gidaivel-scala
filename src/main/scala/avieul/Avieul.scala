package ch.inventsoft.gidaivel.avieul

import ch.inventsoft.scalabase.process._
import Messages._


/**
 * A home-automation service provider.
 */
trait Avieul {
  /**
   * Query for the services of the avieul.
   */
  val services: List[AvieulService]
}

/**
 * A home automation service (i.e. a light-switch, a lamp or a IR sender).
 */
trait AvieulService {
  val serviceType: Int
  val version: ServiceVersion
  def providedBy: Avieul

  def call(callType: Short, payload: Seq[Byte]): MessageSelector[Either[Unit,AvieulError]]
  def request(requestType: Short, payload: Seq[Byte]): MessageSelector[Either[Seq[Byte],AvieulError]]
  def subscribe(subscriptionType: Short, handler: (Seq[Byte]) => Unit): MessageSelector[()=>Unit]

  override def toString = "AvieulService("+serviceType+", "+version+") of "+providedBy
}
sealed trait AvieulError
object TransmitFailed extends AvieulError
object UnknownAvieulService extends AvieulError
object UnknownAvieulServiceRequest extends AvieulError
object UnknownAvieulServiceSubscription extends AvieulError


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

