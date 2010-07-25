package ch.inventsoft
import ch.inventsoft.scalabase.process._
import Messages._
import ch.inventsoft.scalabase.oip._


package object gidaivel {
  type Unregister = () => Unit
  type Answer[A] = MessageSelector[Either[A,GidaivelError]]
}
package gidaivel {



trait GidaivelEnvironment {
  def all: List[GidaivelDevice]
//  def findByType[A <: GidaivelDeviceType[A]](dt: GidaivelDeviceType[A]): List[A] =
//    all.filter(_.deviceType == dt).map(_.asInstanceOf[A])
  def findById(id: GidaivelId): Option[GidaivelDevice] =
    all.filter(_.id == id).headOption

  def addDeviceListener(listener: DeviceListener): Unit
  def removeDeviceListener(listener: DeviceListener): Unit
}


trait GidaivelDevice {
  val id: GidaivelId
  val deviceType: GidaivelDeviceType

  /**
   * true if the device is active and ready to receive commands. This implies that the
   * device is active (has been #activate'd) and all its dependencies are ready.
   */
  def isReady: MessageSelector[Boolean] //TODO this should probably be a callback....
}
trait DeviceLifecycle {
  /**
   * Called by the environment after the device has been constructed or loaded from a
   * persistent store. The device should try to attain the ready state.
   * The method is asynchronous, its completion does not mean that the device has finished
   * activating.
   */
  def activate(environment: GidaivelEnvironment): Unit
  /**
   * Called by the environment when the device is to be put in a persistent store. The device
   * must save everything it needs to be reactivated to a store.
   */
  def passivate: MessageSelector[Unit]
  /**
   * The device is no longer needed and must be shutdown and delete its data from persistent
   * stores.
   */
  def destroy: MessageSelector[Unit]
}
trait ComposedDevice {
  def dependsOn: List[GidaivelDevice]
}

/**
 * Listener for changes of devices (added/removed/ready).
 */
trait DeviceListener {
  def deviceAdded(device: GidaivelDevice) = ()
  def deviceReadyChanged(device: GidaivelDevice, ready: Boolean) = ()
  def deviceDestroyed(device: GidaivelDevice) = ()
}


/**
 * Device type. Implement with an object per type of device.
 */
trait GidaivelDeviceType {
  type D <: GidaivelDevice
  def is(device: GidaivelDevice): Option[D]
  def filter(list: List[GidaivelDevice]): List[D] = {
    list.flatMap(is(_))
  }
}



/**
 * Unique identifier of a gidaivel device.
 * The identifiers are in the form "localname.parentname@domain". Both the local and the domain part are
 * checked on construction of the id (throws IllegalArgumentException)
 */
sealed trait GidaivelId { 
  val local: List[String]
  val domain: String
  protected def local_low = local.map(_.toLowerCase)
  protected def domain_low = domain.toLowerCase
  protected[gidaivel] def check = {
    if (!GidaivelId.domainPattern.matcher(domain).matches)
      throw new IllegalArgumentException("Domain part is invalid ("+domain+")")
    if (local.isEmpty) throw new IllegalArgumentException("Local part is empty")
    local.view.filterNot(GidaivelId.localPattern.matcher(_).matches).foreach { l =>
      throw new IllegalArgumentException("Local part is invalid ("+l+")")
    }
    this
  }
  def copy(local: List[String] = local, domain: String = domain) = {
    val l = local; val d = domain
    new GidaivelId { 
      override val local = l
      override val domain = d
    }.check
  }
  def subid(localname: String) = {
    copy(local=localname :: local)
  }
  def parent: Option[GidaivelId] = local match {
    case _ :: Nil => None
    case _ :: rest => Some(copy(local = rest))
  }
  override def hashCode = local_low.foldLeft(0)((s,e) => s ^ e.hashCode) ^ domain_low.hashCode
  override def equals(other: Any) = other match {
    case other: GidaivelId =>
      local_low.sameElements(other.local_low) && domain_low == other.domain_low
    case other => false
  }
  override def toString = local.mkString(".")+"@"+domain
}
object GidaivelId {
  def apply(local: String, domain: String): GidaivelId = {
    def splitLocal(rest: String, soFar: List[String]=Nil): List[String] = if (rest.nonEmpty) {
      val part = rest.takeWhile(_ != '.')
      val nr_len = rest.length - part.length - 1
      splitLocal(rest.takeRight(nr_len max 0), part :: soFar)
    } else soFar.reverse
    val localList = {
      val l = splitLocal(local)
      if (local.endsWith(".")) "" :: l
      else l
    }
    apply(localList, domain)
  }
  def apply(local: List[String], domain: String): GidaivelId = {
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
  def unapply(id: GidaivelId): Option[(List[String],String)] = {
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
  def parse(local: List[String], domain: String) = {
    try {
      Some(this(local, domain))
    } catch {
      case e: IllegalArgumentException => None
    }
  }
  private val localPattern  = new scala.util.matching.Regex("[a-zA-Z0-9-+_%]+").pattern
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


/*

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
*/


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
