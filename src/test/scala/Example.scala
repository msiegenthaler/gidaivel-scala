package ch.inventsoft
package gidaivel

import scalabase.process._
import scalabase.time._
import scalabase.oip._
import scalaxmpp._
import scalaxmpp.component._
import serialcommunication._
import xbee._
import agents._
import passadi._
import net.liftweb.json.JsonAST._


object Example {
  def main(args: Array[String]): Unit = spawnAndBlock {
    val p = self
    println("## "+self)
    val domain = "gidaivel"
    val secret = Some("secret")
    val port = "/dev/cu.usbserial-A6003ThW"
    val portSpeed = 19200

    object AllowAll extends AuthorizedMembership {
      override def isAllowed(jid: JID) = {
        println("## allowed check for "+jid)
        true
      }
    }
    object Storage extends JavaUtilsPreferenceStorage(domain)


    val server = XMPPComponentServer.tcp("localhost", 5275, None)


/*
    class AboutAgent(override protected val services: AgentServices, manager: AgentManager) extends GidaivelAgent {
      protected case class State(friends: Seq[JID]) {
        def withFriends(friends: Seq[JID]) = copy(friends=friends)
        def persistent = this
      }
      protected override type PersistentState = State

      protected override def init(stored: JValue) = {
        val s = stored.extractOpt[PersistentState].getOrElse(State(Nil))
        s
      }
      protected override val storage = Storage
      protected override def isAllowed(jid: JID) = AllowAll.isAllowed(jid)
      protected override val stateless = new ComponentInfoAgent {
        override val services = AboutAgent.this.services
        override val manager = AboutAgent.this.manager
      }
    }
    val spec = AgentComponent.specification("Gidaivel", "Gidaivel component", domain, secret) { am =>
      am.register("about", s => Spawner.start(new AboutAgent(s, am), SpawnAsRequiredChild))
    }
    server.register(spec)
*/


    val psa = PassadiXBee({
      val portDesc = SerialPort.forName(port).get
      val serialPort = portDesc.open(portSpeed)(SpawnAsRequiredChild)
      val lowlevel =  LocalLowLevelXBeeInApiModePort(serialPort).receive
      LocalSeries1XBee(lowlevel)
    }, SpawnAsRequiredChild)

    val spec = PassadiXmppBridge.xmppComponent(
      domain = domain,
      secret = secret,
      passadi = psa,
      storage = Storage,
      authorization = AllowAll
    )

    server.register(spec)
  }
}



