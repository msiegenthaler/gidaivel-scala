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
    val portName = "/dev/cu.usbserial-A6003ThW"
    val portSpeed = 19200

    val port = SerialPort.forName(portName).get

    object AllowAll extends AuthorizedMembership {
      override def isAllowed(jid: JID) = {
        println("## allowed check for "+jid)
        true
      }
    }
    object Storage extends JavaUtilsPreferenceStorage(domain)


    val server = XMPPComponentServer.tcp("localhost", 5275, None)

    val psa = PassadiXBee({
      val portDesc = port
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
