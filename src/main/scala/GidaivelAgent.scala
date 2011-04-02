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
import JsonDSL._



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

  protected override type State <: {
    def friends: Seq[JID]
    def withFriends(friends: Seq[JID]): State
    def persistent: JValue
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
      val ps = state.persistent
      log.trace("Saved state of {} is {}", jid, ps)
      storeToStorage(ps)
    }
    state
  }

  protected override final def init = {
    val stored = loadFromStorage
    log.trace("Loaded state for {}: {}", jid, stored)
    init(stored)
  }

  /** Initialization (start of the agent) */
  protected def init(loadedState: JValue): State @process

  protected override def termination(state: State) = {
    super.termination(state)
    storeToStorage(state.persistent)
  }

  protected override def message = super.message :+ chatToIq
  protected override def iqGet = super.iqGet :+ discoInfo

  /**
   * Basically a debugging over chat.
   * Usage example: Send the message "iqGet <myiq xmlns="myns"/>" with a chat client to this agent.
   */
  protected val chatToIq = mkMsg {
    case (Chat(_, thread, ChatXmlCommand("iqget", content), from),state) =>
      val get = IQGet("X", from, jid, content)
      val resp = handleIQ(get).receiveOption(10 s)
      sendChatXml(from, thread, resp.map(_.xml.child).getOrElse(<unknown/>))
    case (Chat(_, thread, ChatXmlCommand("iqset", content), from),state) =>
      val set = IQSet("X", from, jid, content)
      val resp = handleIQ(set).receiveOption(10 s)
      sendChatXml(from, thread, resp.map(_.xml.child).getOrElse(<unknown/>))
    case (Chat(_, thread, ChatXmlCommand("message", content), from),state) =>
      val msg = MessageSend(None, None, from, jid, content)
      handleMessage(msg)
      sendChatXml(from, thread, <forwarded />)
    case (Chat(_, thread, ChatXmlCommand("probe", _), from),state) =>
      val xml = status(state).status
      sendChatXml(from, thread, xml)
  }
  private def sendChatXml(to: JID, thread: Option[String], xml: NodeSeq): Unit @process = {
    val string = xml.toString
    services.send(Chat(None, thread, string, to, jid)).receive
  }

  protected val discoInfo = mkIqGet {
    case (get @ FirstElem(ElemName("query", "http://jabber.org/protocol/disco#info")),state) =>
      val fs = features.map(f => <feature var={f} />)
      val ids = identities.map { i =>
        if (i.name.isDefined) <identity category={i.category} type={i.typ} name={i.name.get} />
        else <identity category={i.category} type={i.typ} />                                
      }
      get.resultOk(<query xmlns="http://jabber.org/protocol/disco#info">{fs ++ ids}</query>)
  }

  /** supported features */
  protected def features: Seq[String] = "http://jabber.org/protocol/disco#info" :: "urn:gidaivel:base" :: Nil
  protected def identities: Seq[XmppIdentity] = XmppIdentity("gidaivel", "device") :: Nil

  protected override def acceptSubscription(from: JID, content: NodeSeq)(state: State) = {
    log.trace("{} is asked to accept subscription from {}", jid, from)
    if (isAllowed(from)) {
      val f = state.friends :+ from
      saveState
      state.withFriends(f)
    } else state
  }
  protected override def removeSubscription(from: JID)(state: State) = {
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


/** Utilities for Json serialization and parsing */
object Json {
  import JsonDSL._

  trait JsonMapper[T] {
    def parse(from: JValue): Option[T]
    def serialize(value: T): JValue
    def map[B](parseFun: T => Option[B], serializeFun: B => T) = {
      val outer = this
      new JsonMapper[B] {
        override def parse(from: JValue) = outer.parse(from).flatMap(parseFun)
        override def serialize(value: B) = outer.serialize(serializeFun(value))
      }
    }
    def apply(value: T) = serialize(value)
    def unapply(from: JValue) = parse(from)
  }

  def fromObject[T](field: String, mapper: JsonMapper[T]) = new JsonMapper[T] {
    override def parse(from: JValue) = {
      (for (JField(`field`, mapper(result)) <- from) yield result).headOption
    }
    override def serialize(value: T) = {
      JField(field, mapper(value))
    }
  }

  def seqOf[T](mapper: JsonMapper[T]): JsonMapper[Seq[T]] = new JsonMapper[Seq[T]] {
    override def parse(from: JValue) = from match {
      case JArray(list) => Some(list.flatMap(mapper.parse(_)))
      case other => None
    }
    override def serialize(values: Seq[T]) = JArray(values map(mapper.serialize(_)) toList)
  }

  object Jid extends JsonMapper[JID] {
    override def parse(from: JValue) = from match {
      case JString(value) => JID.parseOption(value)
      case _ => None
    }
    override def serialize(value: JID) = value.stringRepresentation
  }
  object XmlElem extends JsonMapper[Elem] {
    import scala.xml._
    override def parse(from: JValue) = from match {
      case JString(value) => try { Some(XML.loadString(value)) } catch { case e => None } 
      case _ => None
    }
    override def serialize(value: Elem) = value.toString
  }
  object Text extends JsonMapper[String] {
    override def parse(from: JValue) = from match {
      case JString(value) => Some(value)
      case _ => None
    }
    override def serialize(value: String) = value
  }
}
