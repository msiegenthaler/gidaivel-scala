package ch.inventsoft
package gidaivel

import scala.xml._
import scalabase.process._
import scalabase.time._
import scalabase.oip._
import scalaxmpp._
import scalaxmpp.component._
import net.liftweb.json._
import JsonAST._



trait AuthorizedMembership {
  def isAllowed(jid: JID): Boolean
}


/**
 * A gidaivel agent.
 * Supports:
 *  - storage of state
 *  - management of "friends"
 */
trait GidaivelAgent extends StatefulAgent with PresenceManager with StateServer {
  protected val services: AgentServices
  def jid = services.jid
  def name = jid.node

  protected type PersistentState
  protected override type State <: {
    def friends: Seq[JID]
    def withFriends(friends: Seq[JID]): State
    def persistent: PersistentState
  }

  protected def storage: JsonStorage
  private[this] def storeToStorage(value: JValue) = {
    storage.store(jid.stringRepresentation, value)
  }
  private[this] def loadFromStorage = {
    storage.load(jid.stringRepresentation)
  }
  /** Tries to save the state asynchonously */
  protected def saveState = cast { state => 
    spawnChild(NotMonitored) {
      log.debug("Saving the state of {}", jid)
      val ser = serializeState(state)
      storeToStorage(ser)
    }
    state
  }

  protected override final def init = {
    val stored = loadFromStorage
    log.trace("Loaded state for {}: {}", jid, stored)
    val stored2 = stored match {
      case JNull => JArray(Nil) // Null parses to good (null values)
      case other => other
    }
    init(stored2)
  }

  /** Initialization (start of the agent) */
  protected def init(stateInStore: JValue): State @process
  protected implicit val formats = DefaultFormats
  /** Serialize the state to a Json */
  protected def serializeState(state: State): JValue @process = {
    val persistent = state.persistent
    Extraction.decompose(persistent)
  }

  protected override def termination(state: State) = {
    super.termination(state)
    storeToStorage(serializeState(state))
  }

  protected override def message(state: State) =
    super.message(state) :+ chatToIq
  protected override def iqGet(state: State) = super.iqGet(state) :+ discoInfo

  /**
   * Basically a debugging over chat.
   * Usage example: Send the message "iqGet <myiq xmlns="myns"/>" with a chat client to this agent.
   */
  protected val chatToIq = mkMsg {
    case (Chat(_, thread, ChatXmlCommand("iqget", content), from),state) =>
      val get = IQGet("X", from, jid, content)
      concurrent {
        val resp = handleIQ(get).receiveOption(10 s)
        sendChatXml(from, thread, resp.map(_.xml.child).getOrElse(<unknown/>))
      }
      state
    case (Chat(_, thread, ChatXmlCommand("iqset", content), from),state) =>
      val set = IQSet("X", from, jid, content)
      concurrent {
        val resp = handleIQ(set).receiveOption(10 s)
        sendChatXml(from, thread, resp.map(_.xml.child).getOrElse(<unknown/>))
      }
      state
    case (Chat(_, thread, ChatXmlCommand("message", content), from),state) =>
      val msg = MessageSend(None, "other", from, jid, content)
      handleMessage(msg)
      sendChatXml(from, thread, <forwarded />)
      state
    case (Chat(_, thread, ChatXmlCommand("probe", _), from),state) =>
      val xml = status(state).status
      sendChatXml(from, thread, xml)
      state
  }
  private def sendChatXml(to: JID, thread: Option[String], xml: NodeSeq): Unit @process = {
    val string = xml.toString
    services.send(Chat(None, thread, string, to, jid)).receiveOption(5 s)
    ()
  }

  protected val discoInfo = mkIqGet {
    case (get @ FirstElem(ElemName("query", "http://jabber.org/protocol/disco#info")),state) =>
      val fs = features.map(f => <feature var={f} />)
      val ids = identities.map { i =>
        if (i.name.isDefined) <identity category={i.category} type={i.typ} name={i.name.get} />
        else <identity category={i.category} type={i.typ} />                                
      }
      (get.resultOk(<query xmlns="http://jabber.org/protocol/disco#info">{fs ++ ids}</query>), state)
  }

  /** supported features */
  protected def features: Seq[String] = "http://jabber.org/protocol/disco#info" :: "urn:gidaivel:base" :: Nil
  protected def identities: Seq[XmppIdentity] = XmppIdentity("gidaivel", "device") :: Nil

  protected override def acceptSubscription(state: State)(from: JID, content: NodeSeq) = {
    log.trace("{} is asked to accept subscription from {}", jid, from)
    if (isAllowed(from)) {
      val f = state.friends :+ from
      saveState
      state.withFriends(f)
    } else state
  }
  protected override def removeSubscription(state: State)(from: JID) = {
    val f = state.friends.filterNot(_ == from)
    if (f.length < state.friends.length) {
      saveState
      state.withFriends(f)
    } else state
  }
  protected def isAllowed(jid: JID): Boolean

  override def toString = "GidaivelAgent "+name
}

private object ChatXmlCommand {
  def unapply(text: String) = {
    val (c, a) = text.span(_ != ' ')
    val cmd = c.toLowerCase
    val arg = a.trim
    if (arg.isEmpty) Some(cmd, NodeSeq.Empty)
    else {
      try {
        //Parser only accepts one root element, but our content might have more that one (or none)
        val root = XML.loadString("<a>"+arg+"</a>")
        Some(cmd, root.child)
      } catch {
        case e: SAXException => None
      }
    }
  }
}

case class XmppIdentity(category: String, typ: String, name: Option[String]=None)
