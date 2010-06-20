package ch.inventsoft.gidaivel

import ch.inventsoft.scalabase.binary._
import ch.inventsoft.gidaivel.avieul._


/**
 * Gidaivel-Device based on a AvieulService.
 */
trait AvieulDevice extends GidaivelDevice {
  val serviceType: Long
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
