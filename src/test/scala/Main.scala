package ch.inventsoft.gidaivel

import ch.inventsoft.scalabase.time._
import ch.inventsoft.scalabase.log._
import ch.inventsoft.scalabase.oip._
import ch.inventsoft.scalabase.process._
import ch.inventsoft.xbee._
import ch.inventsoft.serialcommunication._
import ch.inventsoft.gidaivel.avieul._
import ch.inventsoft.gidaivel.avieul.xbee._


object Main extends Application {
  def spawnApp(body: => Any @processCps): Unit = {
    val sync = new scala.concurrent.SyncVar[Unit]
    val monitor = spawn {
      noop
      val child = spawnChild(Monitored)(body)
      receive {
        case ProcessExit(child) => //normal termination
          println("Application terminated normally")
          sync.set(())
        case ProcessKill(child, by, reason) =>
          print("Application was terminate by process ")
          print(by)
          print(": ")
          println(reason)
          reason.printStackTrace
          sync.set(())
        case ProcessCrash(child, reason) =>
          print("Application crashed: ")
          println(reason)
          reason.printStackTrace
          sync.set(())
      }
    }
    //wait until monitor terminates
    sync.get
  }
  
spawnApp {
  println("Serial Ports (usbserial)")
  SerialPort.list.foreach(e => println("  "+e))
  val portDesc = SerialPort.forName("/dev/cu.usbserial-A6008hNp").getOrElse(throw new RuntimeException("XBee not attached"))
  println("Connecting to "+portDesc)
  val serialPort = portDesc.open(19200)(SpawnAsRequiredChild)
  println("Connected to "+portDesc)
  val lowlevel = CommunicationPortLowLevelLocalXBeeInApiMode(serialPort)(SpawnAsRequiredChild)  
  println("Connected lowlevel XBee")
  val xbee = LocalSeries1XBee(lowlevel)(SpawnAsRequiredChild)
  val localAddress = receive(xbee.address)
  println("The local xbee address is "+localAddress)
/*
  val peers = receive { xbee.discover() }
  println("Found peer xbees")
  peers.foreach(e => println("  "+e))
*/
  println("initializing..")
  val passadi = PassadiDAvieulsXBee(xbee, SpawnAsRequiredChild)
  println("waiting for initialization and publication of avieuls..")
  receiveWithin(3 s) { case Timeout => ()}
  println("initialized.")
  
  println("Looking for avieuls...")
  
  val avieuls = receive { passadi.findAvieuls }
  println("Found Avieuls:")
  avieuls.foreach { e =>
    println("  "+e)
    e.services.foreach(s => println("    "+s))
  }
  val services = receive { passadi.findServices }

  val onOff = new DirectOnOffLight(services.find(_.serviceType == 0x12L).get)
  println("Found a light")
  val isOn = receive { onOff.isOn }
  println("Current status is "+isOn+", switching")
  val newIsOn = receive { onOff.switch }
  println("New status is "+newIsOn)

  println("done")
  xbee.close
  lowlevel.close
  serialPort.close

  receiveWithin(1 s) { case Timeout => ()} // give the serial port time to shut itself down
}
}
