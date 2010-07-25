package ch.inventsoft.gidaivel

import ch.inventsoft.scalabase.binary._
import ch.inventsoft.scalabase.oip._
import ch.inventsoft.scalabase.process._
import Messages._
import ch.inventsoft.gidaivel.avieul._


/*
class PassadiDevice(passadi: PassadiDAvieuls) extends GidaivelDevice {
  override val deviceType = PassadiDeviceType
//  override def dependsOn = Nil
  override val id = null //TODO
  override def isReady = null //TODO

  def doSome = "hello"
//  def findAvieuls: Answer[List[Avieul]]
//  def findAvieul: Answer[Option[Avieul]]
}
object PassadiDeviceType extends GidaivelDeviceType {
  override type D = PassadiDevice
  override def is(device: GidaivelDevice) = device match {
    case d: PassadiDevice => Some(d)
    case other => None
  }
}
*/

trait PassadiDevice extends GidaivelDevice {
  def avieuls: MessageSelector[List[Avieul]]
  def addAvieulListener(listener: AvieulListener): Function0[Unit]
}
trait Listener {
  val listening: Process
}
trait AvieulListener extends Listener {
  def addedAvieul(avieul: Avieul): Unit
  def removedAvieul(avieul: Avieul): Unit
}

/*
protected[this] trait AvieulDevice extends GidaivelDevice
    with DeviceLifecycle with StateServer {
  protected override type State = AvieulDeviceState

  protected[this] val avieulId: String
  protected[this] val serviceType: Long

    override def activate(environment: GidaivelEnvironment) = cast { state =>
      environment.addDeviceListener(deviceListener)
      state.withEnv(Some(environment))
    }
    override def passivate = call(passivate_) 
    def passivate = cast { s =>
      s.env.foreach(_.removeDeviceListener(deviceListener))
      s.withEnv(None)
    }
    private[this] val deviceListener = new DeviceListener {
      override def deviceAdded(device: GidaivelDevice) = {
      }
    }
    protected[this] def isDeviceRelevant(device: GidaivelDevice) = device match {
      case device: PassadiDevice => true
      case other => false
    }
//    protected[this] def handleDeviceAdded(device: GidaivelDevice) = cast


    //TODO how to find out if our dependency got added/removed
    
}

protected[this] case class AvieulDeviceState(
  env: Option[GidaivelEnvironment],
  passadi: Option[PassadiDevice],
  service: Option[AvieulService])
*/


//case class AvieulDeviceState(env: GidaivelEnvironment, passadi: PassadiDevice, service: AvieulService)



/**
 * Gidaivel-Device based on a AvieulService.
 */
/*
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
*/
