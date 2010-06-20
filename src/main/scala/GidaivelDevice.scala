package ch.inventsoft
import ch.inventsoft.scalabase.process._
import Messages._
import ch.inventsoft.scalabase.oip._


package object gidaivel {
  type Unregister = () => Unit
  type Answer[A] = MessageSelector[Either[A,GidaivelError]]
}
package gidaivel {


sealed trait GidaivelError
object DeviceUnavailable extends GidaivelError
object UnknownRequest extends GidaivelError
sealed trait ProtocolError extends GidaivelError
case class UnknownReply(data: Seq[Byte]) extends ProtocolError
case class IllegalReply(text: String) extends ProtocolError
trait ServiceProblem extends GidaivelError


trait GidaivelDevice {
}


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
