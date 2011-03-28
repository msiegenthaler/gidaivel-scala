package ch.inventsoft
package gidaivel
package passadi
package xbee

import scalabase.process._
import scalabase.log._
import ch.inventsoft.xbee._
import AvieulProtocol._


/** Message from an XBee */
protected case class XBeeMessage(from: XBeeAddress, data: Seq[Byte])


/**
 * Distributes the incoming ReceivedXBeeDataPacket's to the interested parties.
 * Manages its subscriber list itself by listening to ProcessEnd's.
 */
protected class MessageDistributor(processes: List[(XBeeAddress, Option[Byte], Process)] = Nil) extends Log {
  def add(forXBee: XBeeAddress, forServiceIndex: Byte)(process: Process) = {
    val newList = (forXBee, Some(forServiceIndex), process) :: processes
    new MessageDistributor(newList)
  }
  def add(forXBee: XBeeAddress)(process: Process) = {
    val newList = (forXBee, None, process) :: processes
    new MessageDistributor(newList)
  }

  type Element = (XBeeAddress, Option[Byte], Process)
  protected def forwardTo(filter: Element => Boolean, msg: => XBeeMessage) = {
    processes.view.filter(e => filter(e)).map(_._3).foreach_cps(_ ! msg)
    this
  }
  def all(item: Element) = true
  def service(xbee: XBeeAddress, serviceIndex: Byte)(item: Element) = {
    item._1 == xbee && item._2.filter(_ != serviceIndex).isEmpty
  }
  def xbee(xbee: XBeeAddress)(item: Element) = item._1 == xbee

  def handle(msg: Any): MessageDistributor @process = msg match {
    case ReceivedXBeeDataPacket(from, _, _, payload) =>
      payload match {
        case packet@AnnounceServices(_, _) =>
          log.trace("Passadi distributor got announce message from {}", from)
          forwardTo(xbee(from), XBeeMessage(from, packet))
        case packet@GenericAvieulMessage((msgType, data), _) if (msgType >= 0x10 && msgType <= 0x9F && !data.isEmpty) =>
          // call, request or subscription etc. (everything relating to service)
          log.trace("Passadi distributor got service message of type {}: {}", msgType, packet)
          val serviceIndex = data.head
          forwardTo(service(from, serviceIndex), XBeeMessage(from, packet))
        case other =>
          this //do not forward
      }
    case end: ProcessEnd =>
      end match {
        case ProcessExit(_) => ()
        case ProcessKill(_, _, _) => ()
        case ProcessCrash(_, cause) =>
          log.warn("A passadi d'avieuls child process crashed: {}", cause)
      }
      val newList = processes.filterNot(_._3 == end.process)
      new MessageDistributor(newList)
    case other => this
  }
}
