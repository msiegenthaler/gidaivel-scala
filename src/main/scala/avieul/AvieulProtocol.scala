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
  
  val ServiceUnknown = <<( fix_byte(0x10), byte ).drop1
  val ServiceCall = <<( fix_byte(0x20), byte, short, list_to_end(byte) ).drop1
  val ServiceRequest = <<( fix_byte(0x30), byte, short, list_to_end(byte) ).drop1
  val ServiceResponse = <<( fix_byte(0x31), byte, short, list_to_end(byte) ).drop1
  val ServiceRequestUnknown = <<( fix_byte(0x3F), byte, short ).drop1
  val ServiceSubscribe = <<( fix_byte(0x40), byte, short ).drop1
  val ServiceUnsubscribe = <<( fix_byte(0x41), byte, short ).drop1
  val ServiceSubscriptionConfirm = <<( fix_byte(0x42), byte, short ).drop1
  val ServicePublish = <<( fix_byte(0x49), byte, short, list_to_end(byte) ).drop1
  val ServiceSubscriptionUnknown = <<( fix_byte(0x4F), byte, short ).drop1
}

