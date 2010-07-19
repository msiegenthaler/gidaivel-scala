package ch.inventsoft
import ch.inventsoft.scalabase.process._
import Messages._
import ch.inventsoft.scalabase.oip._


package object gidaivel {
  type Unregister = () => Unit
  type Answer[A] = MessageSelector[Either[A,GidaivelError]]
}
package gidaivel {


trait GidaivelDevice {
  val id: GidaivelId = null
}

/**
 * Unique identifier of a gidaivel device.
 * The identifiers are in the form "localname@domain". Both the local and the domain part are
 * checked on construction of the id (throws IllegalArgumentException)
 */
sealed trait GidaivelId { 
  val local: String
  val domain: String
  protected def local_low = local.toLowerCase
  protected def domain_low = domain.toLowerCase
  protected[gidaivel] def check = {
    if (!GidaivelId.localPattern.matcher(local).matches)
      throw new IllegalArgumentException("Local part is invalid ("+local+")")
    if (!GidaivelId.domainPattern.matcher(domain).matches)
      throw new IllegalArgumentException("Domain part is invalid ("+domain+")")
    this
  }
  def copy(local: String = local, domain: String = domain) = {
    val l = local; val d = domain
    new GidaivelId { 
      override val local = l
      override val domain = d
    }.check
  }
  override def hashCode = local_low.hashCode ^ domain_low.hashCode
  override def equals(other: Any) = other match {
    case other: GidaivelId =>
      local_low == other.local_low && domain_low == other.domain_low
    case other => false
  }
  override def toString = local+"@"+domain
}
object GidaivelId {
  def apply(local: String, domain: String): GidaivelId = {
    val l = local; val d = domain
    new GidaivelId {
      val local = l
      val domain = d
    }.check
  }
  def apply(id: String): GidaivelId = {
    id.split('@').toList match {
      case local :: domain :: Nil => apply(local, domain)
      case other => throw new IllegalArgumentException("Invalid GidaivelId structure in "+id)
    }
  }
  def unapply(id: GidaivelId): Option[(String,String)] = {
    Some((id.local,id.domain))
  }
  def parse(id: String) = {
    try {
      Some(this(id))
    } catch {
      case e: IllegalArgumentException => None
    }
  }
  def parse(local: String, domain: String) = {
    try {
      Some(this(local, domain))
    } catch {
      case e: IllegalArgumentException => None
    }
  }
  private val localPattern  = new scala.util.matching.Regex("[a-zA-Z0-9-+_%.]+").pattern
  private val domainPattern = new scala.util.matching.Regex("[a-zA-Z0-9-.:]+").pattern
   
}



/**
 * Errors used by GidaivelDevices.
 * Normally errors are reported as Either[Value,GidaivelError].
 */
sealed trait GidaivelError
object DeviceUnavailable extends GidaivelError
object UnknownRequest extends GidaivelError
sealed trait ProtocolError extends GidaivelError
case class UnknownReply(data: Seq[Byte]) extends ProtocolError
case class IllegalReply(text: String) extends ProtocolError
trait ServiceProblem extends GidaivelError




//TODO move to another source file
trait OnOffSwitch extends GidaivelDevice {
  def isOn: Answer[Boolean]
  def listenForChange(fun: Boolean => Unit @processCps): () => Unit
}

trait OnOffSwitchable extends GidaivelDevice {
  def switchOn = switch(true)
  def switchOff = switch(false)
  def switch(on: Boolean): Answer[Unit]
  def switch: Answer[Boolean]
  def isOn: Answer[Boolean]
}

trait Lamp {
}

trait SimpleLamp extends OnOffSwitchable with Lamp



/*
trait RgbLamp extends OnOffLamp {
  def setColor(color: Color): MessageSelector[Unit]
  def getColor: MessageSelector[Color]
}
case class Color(red: Byte, green: Byte, blue: Byte) {
  private def toList = red :: green :: blue :: Nil
  override def toString = "RGB(" + toList.map(_ & 0xFF).mkString(",") + ")"
}

trait TV extends GidaivelDevice {
  def switchChannel(to: Int): Unit
  def selectInput(to: Int): Unit
}
*/
}
