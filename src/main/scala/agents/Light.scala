package ch.inventsoft
package gidaivel
package agents

import scala.collection.immutable.Set
import scalabase.time._
import scalabase.process._
import scalabase.log._
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
  protected case class State(friends: Seq[JID], isOn: Boolean, unsubscribe: () => Unit @process) {
    def withFriends(friends: Seq[JID]) = copy(friends=friends)
    def persistent: JValue = seqOf(Jid).serialize(friends)
  }

  protected override def init(stored: JValue) = {
    val f = seqOf(Jid).parse(stored).getOrElse(Nil)
    val s = device_isOn.receiveWithin(timeout)
    log.debug("The light isOn={} now", s)
    val unsub = avieulService.subscribe(0x0001, p => onChange(p.head == 0x01)).receiveWithin(timeout)
    State(f, s, unsub)
  }
  protected override def doResync = concurrent { state =>
    val on = device_isOn.receiveOption(1 minute).getOrElse(false)
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
