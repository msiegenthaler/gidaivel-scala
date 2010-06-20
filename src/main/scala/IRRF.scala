package ch.inventsoft.gidaivel

import ch.inventsoft.scalabase.binary._
import ch.inventsoft.gidaivel.avieul._


/**
 * Infrared command receiver.
 */

class IRReceiver(override protected[this] val service: AvieulService) extends AvieulDevice {
  override val serviceType = 0x10L
  
  def subscribe = {
    val handler = (data: Seq[Byte]) => println("## got a command: "+data);
    service.subscribe(0x0001, handler)
  }
  
  // def add() = {
  //   val data = 0x01 :: Nil map(_.toByte)
  //   request(0x01, data)(_ match {
  //     case 0x01 :: Nil => true
  //     case other => false
  //   }
  // }
  
  //TODO
}
