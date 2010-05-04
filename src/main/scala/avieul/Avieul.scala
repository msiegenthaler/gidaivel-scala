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
  def queryServices: MessageSelector[List[AvieulService]]
}

/**
 * A home automation service (i.e. a light-switch, a lamp or a IR sender).
 */
trait AvieulService {
  val serviceType: Int
  val version: ServiceVersion
}

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

