package ch.inventsoft.gidaivel

import ch.inventsoft.scalabase.binary._
import ch.inventsoft.gidaivel.avieul._

/*

trait AvieulOnOffSwitchable extends OnOffSwitchable with AvieulDevice {
  override def isOn = {
    request(0x01, Nil)(mapOnOffReply)
  }
  override def switch(on: Boolean) = {
    val value: Byte = if (on) 0x01 else 0x00
    request(0x02, value :: Nil)(_ match {
      case 0x00 :: Nil => if (!on) Left(()) else Right(IllegalReply("Should be turned off"))
      case 0x01 :: Nil => if (on) Left(false) else Right(IllegalReply("Should be turned on"))
      case other => Right(UnknownReply(other))
    })
  }
  override def switch = {
    request(0x03, Nil)(mapOnOffReply)
  }

  protected[this] def mapOnOffReply(data: Seq[Byte]) = data match {
    case 0x00 :: Nil => Left(false)
    case 0x01 :: Nil => Left(true)
    case other => Right(UnknownReply(other))
  }
}

class DirectOnOffLight(override protected[this] val service: AvieulService) extends SimpleLamp with AvieulOnOffSwitchable {
  //TODO
  override val id = GidaivelId("test", "test")
  override val serviceType = 0x12L
}
*/


