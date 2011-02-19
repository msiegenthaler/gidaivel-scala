package ch.inventsoft
package gidaivel
package agents

import scalabase.time._
import scalabase.process._
import scalaxmpp._
import scalaxmpp.component._
import net.liftweb.json._
import JsonAST._


/**
 * Light that can be switched on or off.
 */
trait OnOffLightAgent extends AvieulServiceAgent {
  protected case class State(friends: Seq[JID], isOn: Boolean, unsubscribe: () => Unit @process) {
    def withFriends(friends: Seq[JID]) = copy(friends=friends)
    def persistent = PersistentState(friends.toList)
  }
  protected case class PersistentState(friends: List[JID])

  protected override def init(stored: JValue) = {
    val ps = stored.extractOpt[PersistentState].getOrElse(PersistentState(Nil))
    val s = device_isOn.receiveWithin(timeout)
    val unsub = avieulService.subscribe(0x0001, p => onChange(p == 0x01)).receiveWithin(timeout)
    State(ps.friends, s, unsub)
  }

  protected override def iqGet(state: State) = super.iqGet(state) :+ queryOnOff
  protected override def message(state: State) = super.message(state)

  val namespace = "urn:gidaivel:lights:simple"
  val xml_on = <on xmlns={namespace} />
  val xml_off = <off xmlns={namespace} />

  protected override def status(state: State) = {
    val s = if (state.isOn) {
      <status>on</status> ++ xml_on
     } else {
      <status>off</status> ++ xml_off
    }
    Status(<show>chat</show> ++ s)
  }

  protected def queryOnOff = mkIqGet {
    case (get @ FirstElem(ElemName("query", namespace)),state) =>
      val res = if (state.isOn) xml_on else xml_off
      (get.resultOk(res), state)
  }

/*
  protected def chat = mkCommand {
    case ("on", _, state) =>
      device_turnOnOff(true)
      ("ok", state)
    case ("off", _, state) =>
      device_turnOnOff(false)
      ("ok", state)
    case ("isOn", _, state) =>
      val status = if (state.isOn) "on" else "off"
      (status, state)
    case ("resync", _, state) =>
      val on = device_isOn.receiveWithin(10 s)
      ("ok", state.copy(isOn = on))
  }
*/

  protected def onChange(newOn: Boolean) = cast { state =>
    if (state.isOn == newOn) state
    else {
      announce
      state.copy(isOn = newOn)
    }
  }

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
  protected def device_turnOnOff(on: Boolean) = {
    val status: Byte = if (on) 1 else 0
    avieulService.call(0x0001, status :: Nil)
    noop
  }
}
