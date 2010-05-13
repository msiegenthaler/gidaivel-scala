package ch.inventsoft
package object gidaivel {
  type Unregister = () => Unit
}

import ch.inventsoft.scalabase.process._
import Messages._
import ch.inventsoft.scalabase.oip._



trait GidaivelDevice {
}


trait OnOffSwitch extends GidaivelDevice {
  def isOn: MessageSelector[Boolean]
  def listenForChange(fun: Boolean => Unit @processCps): () => Unit
}

trait OnOffSwitchable extends GidaivelDevice {
  def switchOn = switch(true)
  def switchOff = switch(false)
  def switch(on: Boolean): MessageSelector[Unit]
  def switch: MessageSelector[Boolean]
  def isOn: MessageSelector[Boolean]
}

trait SimpleLamp extends OnOffSwitchable



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
