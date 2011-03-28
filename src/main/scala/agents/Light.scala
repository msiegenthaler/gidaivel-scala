package ch.inventsoft
package gidaivel
package agents

import scala.collection.immutable.Set
import scalabase.time._
import scalabase.process._
import scalabase.log._
import scalabase.oip._
import scalaxmpp._
import scalaxmpp.component._
import net.liftweb.json._
import JsonDSL._
import JsonAST._
import Json._

/**
 * Light that can be switched on or off.
 * See the documententation under /docs/devices.
 */
trait OnOffLight extends AvieulBasedDevice with Log {
  protected case class State(friends: Seq[JID], isOn: Boolean) {
    def withFriends(friends: Seq[JID]) = copy(friends=friends)
    def persistent: JValue = seqOf(Jid).serialize(friends)
  }

  protected override def init(stored: JValue) = {
    val f = seqOf(Jid).parse(stored).getOrElse(Nil)
    val s = device_isOn
    log.debug("The light isOn={} now", s)
    ResourceManager[Unsubscribe](
      resource = device.subscribe(0x0001, p => onChange(p.head==0x01)),
      close = unsub => unsub()
    ).receive
    State(f, s)
  }
  override def shutdown = stopAndWait.receive
  protected override def doResync = concurrent { state =>
    val on = device_isOn
    atomic(_.copy(isOn = on))
  }

  protected override def iqGet = super.iqGet :+ isOn
  protected override def message = super.message :+ turnOnOff

  val namespace = "urn:gidaivel:lights:onOff"
  protected override def features = super.features :+ namespace

  protected val isOn = mkIqGet {
    case (get @ FirstElem(ElemName("is-on", namespace)),state) =>
      val res = <is-on xmlns={namespace}>{if (state.isOn) <on/> else <off/>}</is-on>
      get.resultOk(res)
  }
  protected val turnOnOff = mkMsg {
    case (FirstElem(ElemName("turn-on", namespace)),state) =>
      log.debug("Turn the light on")
      device_turnOnOff(true)
    case (FirstElem(ElemName("turn-off", namespace)),state) =>
      log.debug("Turn the light off")
      device_turnOnOff(false)
  }
  private def device_turnOnOff(on: Boolean) = {
    val status: Byte = if (on) 1 else 0
    device.call(0x0001, status :: Nil)
  }

  protected override def status(state: State) = {
    val s = if (state.isOn) {
      <status>on</status> ++ <is-on xmlns={namespace}><on/></is-on>
     } else {
      <status>off</status> ++ <is-on xmlns={namespace}><off/></is-on>
    }
    Status(<show>chat</show> ++ s)
  }

  /* called from the avieul subscription */
  private def onChange(newOn: Boolean) = cast { state =>
    log.debug("The light changed to isOn={}", newOn)
    if (state.isOn == newOn) state
    else {
      announce
      state.copy(isOn = newOn)
    }
  }
  /* ask the device whether it's turned on or off */
  protected def device_isOn = device.request(0x0001).head == 0x01

  override def toString = "Light("+avieulService.id+")"
}
