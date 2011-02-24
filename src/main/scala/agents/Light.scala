package ch.inventsoft
package gidaivel
package agents

import scalabase.time._
import scalabase.process._
import scalabase.log._
import scalaxmpp._
import scalaxmpp.component._
import net.liftweb.json._
import JsonAST._


/**
 * Light that can be switched on or off.
 * See the documententation under /docs/devices.
 */
trait OnOffLight extends AvieulBasedDevice with Log {
  protected case class State(friends: Seq[JID], isOn: Boolean, unsubscribe: () => Unit @process) {
    def withFriends(friends: Seq[JID]) = copy(friends=friends)
    def persistent = PersistentState(friends.toList)
  }
  protected case class PersistentState(friends: List[JID])

  protected override def init(stored: JValue) = {
    val ps = stored.extractOpt[PersistentState].getOrElse(PersistentState(Nil))
    val s = device_isOn.receiveWithin(timeout)
    log.debug("The light isOn={} now", s)
    val unsub = avieulService.subscribe(0x0001, p => onChange(p.head == 0x01)).receiveWithin(timeout)
    State(ps.friends, s, unsub)
  }

  protected override def iqGet(state: State) = super.iqGet(state) :+ isOn
  protected override def message(state: State) = super.message(state) :+ turnOnOff

  val namespace = "urn:gidaivel:lights:onOff"
  protected override def features = super.features :+ namespace

  protected val isOn = mkIqGet {
    case (get @ FirstElem(ElemName("is-on", namespace)),state) =>
      val res = <is-on xmlns={namespace}>{if (state.isOn) <on/> else <off/>}</is-on>
      (get.resultOk(res), state)
  }
  protected val turnOnOff = mkMsg {
    case (FirstElem(ElemName("turn-on", namespace)),state) =>
      log.debug("Turn the light on")
      device_turnOnOff(true)
      state
    case (FirstElem(ElemName("turn-off", namespace)),state) =>
      log.debug("Turn the light off")
      device_turnOnOff(false)
      state
  }
  private def device_turnOnOff(on: Boolean) = {
    val status: Byte = if (on) 1 else 0
    avieulService.call(0x0001, status :: Nil)
    noop
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
  protected def device_isOn = {
    val sel = avieulService.request(0x0001, Nil)
    sel.map_cps { _ match {
      case Left(status) if status.length>0 =>
        noop
        status.head == 0x01
      case Left(_) =>
        throw new RuntimeException("missing result")
      case Right(error) =>
        throw new RuntimeException(error.toString)
    }}
  }
}
