package ch.inventsoft.gidaivel

import ch.inventsoft.scalabase.binary._
import ch.inventsoft.gidaivel.avieul._


trait AvieulDevice extends GidaivelDevice {
  protected[this] val service: AvieulService
  protected[this] def request[A](requestType: Short, data: Seq[Byte])(fun: Function[Seq[Byte],Either[A,GidaivelError]]) = {
    service.request(requestType, data).map(_ match {
      case Left(responseData) =>
        fun(responseData)
      case Right(error) => Right(mapError(error))
    })
  }
  protected[this] def call(callType: Short, data: Seq[Byte]) = {
    service.call(callType, data).map(_ match {
      case Left(()) => Left(())
      case Right(error) => Right(mapError(error))
    })
  }
  protected[this] def mapError(error: AvieulError): GidaivelError = error match {
    case TransmitFailed => DeviceUnavailable
    case UnknownAvieulService => UnknownRequest
    case UnknownAvieulServiceRequest => UnknownRequest
    case UnknownAvieulServiceSubscription => UnknownRequest
  }
}

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
    case 0x00 :: Nil => Left(true)
    case 0x01 :: Nil => Left(false)
    case other => Right(UnknownReply(other))
  }
}

class DirectOnOffLight(override protected[this] val service: AvieulService) extends SimpleLamp with AvieulOnOffSwitchable {

}


