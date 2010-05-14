package ch.inventsoft.gidaivel.avieul

import ch.inventsoft.scalabase.binary.BytesParsing._


/**
 * Aviel protocol.
 */
object AvieulProtocol {
//TODO document
  val GenericAvieulMessage = <<( byte, list_to_end(byte) )>>
  
  val RequestInfo = <<( fix_byte(0x01) )
  val ServiceInfo = <<( byte, integer, byte )>>
  val AnnounceService = <<( fix_byte(0x02), list_to_end(ServiceInfo) ).drop1
  
  val ServiceCall = <<( fix_byte(0x10), byte, short, list_to_end(byte) ).drop1
  val ServiceRequest = <<( fix_byte(0x11), byte, short, list_to_end(byte) ).drop1
  val ServiceResponse = <<( fix_byte(0x12), byte, short, list_to_end(byte) ).drop1
  val ServiceRequestUnknown = <<( fix_byte(0x1F), byte, short ).drop1
  val ServiceSubscribe = <<( fix_byte(0x20), byte, short, list_to_end(byte) ).drop1
  val ServiceUnsubscribe = <<( fix_byte(0x21), byte, short ).drop1
  val ServiceSubscriptionMade = <<( fix_byte(0x22), byte, short ).drop1
  val ServicePublish = <<( fix_byte(0x22), byte, short, list_to_end(byte) ).drop1
  val ServiceSubscriptionUnknown = <<( fix_byte(0x2F), byte, short ).drop1
  val ServiceUnknown = <<( fix_byte(0x0F), byte ).drop1
}

