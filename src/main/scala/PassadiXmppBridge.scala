package ch.inventsoft
package gidaivel

import passadi._
import agents._
import scalabase.process._
import scalabase.time._
import scalabase.oip._
import scalabase.log._
import scalaxmpp._
import scalaxmpp.component._
import net.liftweb.json._
import JsonAST._


/**
 * Bridges a passadi and its avieuls to XMPP.
 */
trait PassadiXmppBridge extends StateServer with Log {
  protected val agentManager: AgentManager
  protected val passadi: Passadi
  protected val storage: JsonStorage
  protected val authorized: AuthorizedMembership

  protected case class State(agents: Iterable[AgentSpecification])

  protected override def init = {
    agentManager.register("passadi", s => Spawner.start(new PassadiAgent(s, agentManager), SpawnAsRequiredChild))

    passadi.changeListener(onChange _)
    val avieuls = passadi.avieuls.receiveWithin(10 s)
    val agents = avieuls.flatMap_cps(registerAvieul(_))
    log.info("Passadi initialized with {} Avieul ({} AvieulServices)", avieuls.size, agents.size)
    State(agents.toList)
  }

  protected def onChange(change: PassadiChange): Unit @process = cast { state => 
    change match {
      case NewAvieul(avieul) =>
        log.info("New Avieul added: {}", avieul.id)
        val added = reregisterAvieul(avieul)
        state.copy(agents = state.agents.filterNot(_.avieul == avieul) ++ added)
      case ChangedAvieul(avieul) =>
        log.info("New Avieul updated: {}", avieul.id)
        val added = reregisterAvieul(avieul)
        state.copy(agents = state.agents.filterNot(_.avieul == avieul) ++ added)
      case RemovedAvieul(avieul) =>
        log.info("Avieul removed: {}", avieul.id)
        val agents = agentsForAvieul(avieul)
        agents.foreach_cps(_.unregister)
        state.copy(agents = state.agents.filterNot(_.avieul == avieul))
    }
  }
  private def reregisterAvieul(avieul: Avieul) = {
    val agents = agentsForAvieul(avieul)
    agents.foreach_cps(_.unregister)
    registerAvieul(avieul)
  }
  private def registerAvieul(avieul: Avieul) = {
    val services = avieul.services.receiveWithin(10 s)
    services.map_cps(registerService(avieul, _))
  }
  private def registerService(avieul: Avieul, service: AvieulService) = {
    val agentSpec = mkAgentForService(avieul, service)
    agentManager.register(agentSpec.name, agentSpec)
    agentSpec
  }
  private def agentsForAvieul(avieul: Avieul) = {
    val agents = agentManager.registeredComponents.receiveWithin(10 s)    
    agents.map(_._2).flatMap(_ match {
      case agent: AvieulAgent => if (agent.avieul == avieul) Some(agent) else None
      case _ => None
    })
  }

  protected trait AvieulAgent extends AvieulBasedDevice {
    private[PassadiXmppBridge] def unregister: Unit @process
  }
  protected trait AgentSpecification extends Function1[AgentServices,Agent @process] {
    val name: String
    val avieul: Avieul
    val avieulService: AvieulService
  }

  protected def mkAgentForService(avieul: Avieul, service: AvieulService): AgentSpecification = {
    val a = avieul
    new AgentSpecification {
      override val name = service.id
      override val avieul = a
      override val avieulService = service
      override def apply(s: AgentServices) = {
        val strg = storage
        trait AgentBase extends AvieulAgent {
          protected val storage = strg
          override val avieul = a
          override val avieulService = service
          protected val services = s
          protected override def isAllowed(jid: JID) = authorized.isAllowed(jid)
          override private[PassadiXmppBridge] def unregister = services.unregister
        }
        val agent = service match {
          case AvieulService(0x00000012, _) => 
            new OnOffLight with AgentBase
          case _ =>
            new UnknownAvieulBasedDevice with AgentBase
        }
        Spawner.start(agent, SpawnAsRequiredChild)
      }
    }
  }

  private[PassadiXmppBridge] def agents = get(_.agents) 

  /** Agent for the passadi */
  protected class PassadiAgent(override val services: AgentServices, val manager: AgentManager) extends GidaivelAgent {
    protected case class State(friends: Seq[JID]) {
      def withFriends(friends: Seq[JID]) = copy(friends=friends)
      def persistent = this
    }
    protected override type PersistentState = State

    protected override val storage = PassadiXmppBridge.this.storage
    protected override def isAllowed(jid: JID) = authorized.isAllowed(jid)
    protected override def init(stored: JValue) = {
      stored.extractOpt[PersistentState].getOrElse(State(Nil))
    }

    protected override val stateless = new ComponentInfoAgent {
      override val services = PassadiAgent.this.services
      override val manager = PassadiAgent.this.manager
    }
    
    protected override def message(state: State) = super.message(state) :+ refreshAvieuls
    protected override def iqGet(state: State) = super.iqGet(state) :+ listAvieuls
    protected val namespace = "urn:gidaivel:passadi"
    
    protected override def features = super.features :+ namespace
    protected override def identities = {
      val n = passadi.toString
      super.identities :+ XmppIdentity("gateway", "gidaivel-passadi") :+ XmppIdentity("gidaivel", "passadi", Some(n))
    }

    protected val refreshAvieuls = mkMsg {
      case (FirstElem(ElemName("refresh", namespace)),state) =>
        concurrent { 
          log.debug("Refreshing the passadi")
          passadi.refresh.receive
          log.info("Passadi refreshed Avieuls")
        }
        state
    }
    protected val listAvieuls = mkIqGet {
      case (get @ FirstElem(ElemName("query", "http://jabber.org/protocol/disco#items")),state) =>
        log.debug("Listing Avieuls..")
        val agents = PassadiXmppBridge.this.agents.receiveOption(4 s)
        val items = agents.getOrElse(Nil).map { a =>
          <item jid={JID(a.name, services.jid.domain).stringRepresentation} name={"Avieul "+a.name} />
        }
        (get.resultOk(<query xmlns="http://jabber.org/protocol/disco#items">{items}</query>), state)
    }
  }
}

object PassadiXmppBridge {
  def xmppComponent(domain: String, 
                    secret: Option[String],
                    passadi: => Passadi @process,
                    storage: JsonStorage,
                    authorization: AuthorizedMembership) = {
    val desc = "Gidaivel Passadi XMPP-component for Domain ("+domain+")"
    AgentComponent.specification("Gidaivel", desc, domain, secret) { am =>
      val p = passadi
      val strg = storage
      val bridge = new PassadiXmppBridge {
        override val agentManager = am
        override val passadi = p
        override val storage = strg
        override val authorized = authorization
      }
      Spawner.start(bridge, SpawnAsRequiredChild)
      ()
    }
  }
}

trait UnknownAvieulBasedDevice extends AvieulBasedDevice {
  protected case class State(friends: Seq[JID]) {
    def withFriends(friends: Seq[JID]) = copy(friends=friends)
    def persistent = this
  }
  protected type PersistentState = State
  protected override def init(stored: JValue) = {
    stored.extractOpt[PersistentState].getOrElse(State(Nil))
  }
}
