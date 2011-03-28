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
import JsonDSL._
import Json._


/**
 * Bridges a passadi and its avieuls to XMPP.
 */
trait PassadiXmppBridge extends StateServer with Log {
  protected val agentManager: AgentManager
  protected val passadi: Passadi
  protected val storage: JsonStorage
  protected val authorized: AuthorizedMembership

  protected case class State(agents: Iterable[AgentSpecification], passadiAgent: Option[PassadiAgent])

  protected override def init = {
    val passadiAgent = 
    agentManager.register("passadi", s => {
      val pa = new PassadiAgent(s, agentManager)
      spawn { cast(_.copy(passadiAgent=Some(pa))) }
      pa
    })

    val avieuls: Iterable[Avieul] = passadi.avieuls.receiveWithin(10 s)
    val agents = avieuls.flatMap_cps(registerAvieul(_))
    log.info("Passadi initialized with {} Avieul ({} AvieulServices)", avieuls.size, agents.size)
    passadi.changeListener(onChange _)
    State(agents.toList, None)
  }

  protected def onChange(change: PassadiChange): Unit @process = cast { state =>
    val s = change match {
      case NewAvieul(avieul) =>
        log.info("Avieul added: {}", avieul.id)
        val added = reregisterAvieul(avieul)
        state.copy(agents = state.agents.filterNot(_.avieul == avieul) ++ added)
      case ChangedAvieul(avieul) =>
        log.info("Avieul updated: {}", avieul.id)
        val added = reregisterAvieul(avieul)
        state.copy(agents = state.agents.filterNot(_.avieul == avieul) ++ added)
      case RemovedAvieul(avieul) =>
        log.info("Avieul removed: {}", avieul.id)
        val agents = agentsForAvieul(avieul)
        agents.foreach_cps(_.unregister)
        state.copy(agents = state.agents.filterNot(_.avieul == avieul))
    }
    s.passadiAgent.foreach_cps(_.agentsUpdated(s.agents))
    s
  }
  private def reregisterAvieul(avieul: Avieul) = {
    // the 'old' agents will be unregistered if they already exists by the agentcomponent
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
  protected trait AgentSpecification extends Function1[AgentServices,Agent] {
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
          override private[PassadiXmppBridge] def unregister = services.unregister.receiveWithin(5 minutes)
        }
        service match {
          case AvieulService(0x00000012, _) => 
            log.debug("Creating OnOffLight for {}", service.id)
            new OnOffLight with AgentBase
          case AvieulService(0x00000010, _) =>
            log.debug("Creating IRReceiver for {}", service.id)
            new IRReceiver with AgentBase
          case _ =>
            log.debug("Creating UnknownAvieulBasedDevice for {}", service.id)
            new UnknownAvieulBasedDevice with AgentBase
        }
      }
    }
  }

  private[PassadiXmppBridge] def agents = get(_.agents) 

  override def toString = "PassadiXmppBridge"

  /** Agent for the passadi */
  protected class PassadiAgent(override val services: AgentServices, val manager: AgentManager)
            extends GidaivelAgent with ComponentInfoAgent {
    protected case class State(friends: Seq[JID], agents: Iterable[AgentSpecification]) {
      def withFriends(friends: Seq[JID]) = copy(friends=friends)
      def persistent: JValue = seqOf(Jid).serialize(friends)
   }

    protected override val storage = PassadiXmppBridge.this.storage
    protected override def isAllowed(jid: JID) = authorized.isAllowed(jid)
    protected override def init(stored: JValue) = {
      val f = seqOf(Jid).parse(stored).getOrElse(Nil)
      val agents = PassadiXmppBridge.this.agents.receive
      State(f, agents)
    }

    protected override def message = super.message :+ refreshAvieuls
    protected override def iqGet = super.iqGet :+ listAvieuls
    protected val namespace = "urn:gidaivel:passadi"
    
    protected override def features = super.features :+ namespace
    protected override def identities = {
      val n = passadi.toString
      super.identities :+ XmppIdentity("gateway", "gidaivel-passadi") :+ XmppIdentity("gidaivel", "passadi", Some(n))
    }

    def agentsUpdated(agents: Iterable[AgentSpecification]) = cast { state =>
      announce
      state.copy(agents=agents)
    }

    protected override def status(state: State) = {
      val msg = "Passadi with " + state.agents.size + " avieuls"
      val s = <show>chat</show><status>{msg}</status>;
      Status(s)
    }
    protected val refreshAvieuls = mkMsg {
      case (FirstElem(ElemName("refresh", namespace)),state) =>
        log.debug("Refreshing the passadi")
        passadi.refresh.receive
        log.info("Passadi refreshed Avieuls")
    }
    protected val listAvieuls = mkIqGet {
      case (get @ FirstElem(ElemName("query", "http://jabber.org/protocol/disco#items")),state) =>
        log.debug("Listing Avieuls..")
        val items = state.agents.map { a =>
          <item jid={JID(a.name, services.jid.domain).stringRepresentation} name={"Avieul "+a.name} />
        }
        get.resultOk(<query xmlns="http://jabber.org/protocol/disco#items">{items}</query>)
    }
    override def toString = "PassadiAgent"
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
    def persistent: JValue = seqOf(Jid).serialize(friends)
  }
  protected type PersistentState = State
  protected override def init(stored: JValue) = {
    val f = seqOf(Jid).parse(stored).getOrElse(Nil)
    State(f)
  }
  override def toString = "UnknownAvieulBasedDevice("+avieulService.id+")"
}
