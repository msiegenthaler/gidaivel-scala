package ch.inventsoft
package gidaivel
package passadi

import scalabase.binary.BytesParsing._


/**
 * Aviel protocol.
 */
object AvieulProtocol {
  /**
   * Basic structure of a message:
   * - type
   * - body (max. 99 bytes)
   * Checksum and length are known to receiver because the messages are part of
   * XBee-TX/RX packets that provide this informations. Also the sender is known.
   */
  val GenericAvieulMessage = <<( byte, list_to_end(byte) )>>
  
  /**
   * Request information about the device and it's services. The device responds
   * with an AnnounceServices message
   */
  val RequestInfo = <<( fix_byte(0x01) )

  /**
   * Information about a service:
   *  - Index of the service (used to address the service)
   *  - type
   *  - version
   */
  val ServiceInfo = <<( byte, integer, byte )>>
  /**
   * Information about the devices services.
   * Response to RequestInfo and may be sent/broadcasted by the device at any time
   * esp. when it starts up.
   */
  val AnnounceServices = <<( fix_byte(0x02), list_to_end(ServiceInfo) ).drop1
  
  /** The service requested is not known to the device (service-index) */
  val ServiceUnknown = <<( fix_byte(0x10), byte ).drop1
  
  /**
   * Call to a service. A call does not produce a result-message (in contrast
   * to a Service-Request).
   * - service index (see AnnounceService)
   * - request type (service-dependent)
   * - payload
   */
  val ServiceCall = <<( fix_byte(0x20), byte, short, list_to_end(byte) ).drop1

  /**
   * Request to a service. The request will be answered with a ServiceResponse
   * of the same type. Overlapping ServiceRequests from the same sender are not
   * explicity supported, the answers might be received out of order. See the
   * service's documentation for the behavior.
   * - service index (see AnnounceService)
   * - request type (service-dependent)
   * - payload
   */
  val ServiceRequest = <<( fix_byte(0x30), byte, short, list_to_end(byte) ).drop1
  /**
   * Response to a ServiceRequest.
   * - serviceIndex (see AnnounceService)
   * - request type (same as ServiceRequest)
   * - payload
   */
  val ServiceResponse = <<( fix_byte(0x31), byte, short, list_to_end(byte) ).drop1
  /**
   * Request is not known/supported.
   * - service index (see AnnounceService)
   * - request type (that is unsupported)
   */
  val ServiceRequestUnknown = <<( fix_byte(0x3F), byte, short ).drop1

  /**
   * Subscribe to messages of a service (see ServicePublish). Results in a
   * ServiceSubscriptionConfim or a ServiceSubscriptionUnknown.
   * Subscriptions are tracked per sender-address.
   * - service index (see AnnounceService)
   * - subscription type
   */
  val ServiceSubscribe = <<( fix_byte(0x40), byte, short ).drop1
  /**
   * Unsubscribe from a subscription (setup via ServiceSubscribe)
   * - service index (see AnnounceService)
   * - subscription type
   */
  val ServiceUnsubscribe = <<( fix_byte(0x41), byte, short ).drop1
  /**
   * Confirms that a subscription has been successfully set up.
   * - service index (see AnnounceService)
   * - subscription type
   */
  val ServiceSubscriptionConfirm = <<( fix_byte(0x42), byte, short ).drop1
  /**
   * Subscription type is not known / is unsupported
   * - service index (AnnounceService)
   * - subscription type
   */
  val ServiceSubscriptionUnknown = <<( fix_byte(0x4F), byte, short ).drop1
  /**
   * Message published by the service to all subscribers.
   * - service index (AnnounceService)
   * - subscription type
   * - payload
   */
  val ServicePublish = <<( fix_byte(0x49), byte, short, list_to_end(byte) ).drop1
}

