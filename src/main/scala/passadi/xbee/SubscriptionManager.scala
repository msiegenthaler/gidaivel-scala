package ch.inventsoft
package gidaivel
package passadi
package xbee

import scalabase.process._
import Messages._
import scalabase.log._
import scalabase.oip._
import scalabase.time._
import ch.inventsoft.xbee._
import AvieulProtocol._



/** Element that can be subscribed */
case class SubscriptionKey(xbee: XBeeAddress, serviceIndex: Byte, subscription: Short) {
  override def toString = "Subscription " + xbee + "-" + serviceIndex + "-" + subscription
}

/* A specific subscription */
case class Subscription(key: SubscriptionKey, handler: Seq[Byte] => Unit @process)

/** Manages a subscription */
protected trait SubscriptionManager {
  val key: SubscriptionKey
  def handlerProcess: Process
  def terminate: Unit @process
}


/**
 * Implementation of the subscription manager
 */
abstract class XBeeSubscriptionManager extends SubscriptionManager with Spawnable with Log {
  /** called when data to publish is received */
  protected def publish(data: Seq[Byte]): Unit @process
  protected val xbee: LocalXBee
  protected val sendTimeout: Duration = 8 seconds
  /** could not reach the XBee */
  protected def onNoAckReceived: Unit @process = noop

  protected override def body = {
    log.trace("Establishing subscription {}", key)
    def runLoop: Unit @process = {
      if (subscribeWithRetry) {
        log.debug("Subscription {} has been set up", key)
        if (run) {
          log.trace("Reestablishing subscription {}", key)
          runLoop
        } else noop
      } else noop
    }
    runLoop
    unsubscribe
  }

  protected val retryTimeout = 1 minute
  protected def subscribeWithRetry: Boolean @process = {
    emptyMsgs
    val r = subscribe
    r match {
      case Successful => noop; true
      case Failed =>
        log.trace("Could not setup {}. Retrying", key)
        receiveWithin(retryTimeout) {
          case XBeeMessage(key.xbee, AnnounceServices(_, _)) => subscribeWithRetry
          case TerminationRequest(from) =>
            from ! Terminate
            log.trace("Termination request for SubscriptionManager {} while retrying", key)
            false
          case Timeout => subscribeWithRetry
        }
      case UnknownSubOrService =>
        log.trace("Could not setup {} because service or subscriptionType is unknown. Retrying..", key)
        receive {
          case XBeeMessage(key.xbee, AnnounceServices(_, _)) => subscribeWithRetry
          case TerminationRequest(from) =>
            from ! Terminate
            log.trace("Termination request for SubscriptionManager {} while retrying", key)
            false
        }
      case TerminationRequested =>
        noop; false
    }
  }

  protected def subscribe: SubscriptionResult @process = {
    val sent = send(ServiceSubscribe(key.serviceIndex, key.subscription))
    if (sent.isSuccess) {
      receiveWithin(5 s) {
        case XBeeMessage(key.xbee, ServiceSubscriptionConfirm((key.serviceIndex, key.subscription), _)) =>
          Successful
        case XBeeMessage(key.xbee, ServiceSubscriptionUnknown((key.serviceIndex, key.subscription), _)) =>
          UnknownSubOrService
        case XBeeMessage(key.xbee, ServiceUnknown(key.serviceIndex, _)) =>
          UnknownSubOrService
        case TerminationRequest(from) =>
          from ! Terminate
          log.trace("Termination request for SubscriptionManager {} while trying to set up connection", key)
          TerminationRequested
        case Timeout =>
          Failed
      }
    } else {
      noop
      Failed
    }
  }

  protected sealed trait SubscriptionResult
  protected object Successful extends SubscriptionResult
  protected object Failed extends SubscriptionResult
  protected object UnknownSubOrService extends SubscriptionResult
  protected object TerminationRequested extends SubscriptionResult

  protected def unsubscribe = {
    log.debug("Subscription {} has been stopped (sending unsubscribe to xbee)", key)
    xbee.send(key.xbee, ServiceUnsubscribe(key.serviceIndex, key.subscription))
  }

  protected def run: Boolean @process = receive {
    case TerminationRequest(sender) =>
      sender ! Terminate
      log.trace("Termination request for SubscriptionManager {}", key)
      false
    case XBeeMessage(key.xbee, ServicePublish((key.serviceIndex, key.subscription, data), _)) =>
      publish(data)
      run
    case XBeeMessage(key.xbee, AnnounceServices(_, _)) =>
      //Device reannounces its services, it was probably restarted
      // renew subscription
      log.debug("Refreshing subscription since xbee has reannounced itself")
      true
    case other =>
      log.trace("Unknown message received: {}", other)
      run
  }

  protected def send(data: Seq[Byte]): TransmitStatus @process = {
    log.debug("Sending subscribe for {} to xbee", key)
    val selector = xbee.sendTracked(key.xbee, data)
    val res = receiveWithin(sendTimeout)(selector.option).getOrElse(TransmitStatusNoAckReceived)
    if (res == TransmitStatusNoAckReceived) onNoAckReceived
    res
  }
  protected def emptyMsgs: Unit @process = receiveNoWait {
    case Timeout => ()
    case something => emptyMsgs
  }

  override def terminate = {
    val caller = self
    val notification = new Object
    spawnChild(Required) {
      watch(process)
      process ! TerminationRequest(self)
      receiveWithin(2 s) {
        case Terminate => //wait until terminated
          receiveWithin(2 minutes) {
            case ProcessEnd(this.process) =>
              ()
            case Timeout =>
              log.info("Gave up waiting for termination of subscription manager {}..", key)
          }
          caller ! notification
        case Timeout => // probably already terminated
          log.debug("Could not correctly shut down subscription manager {}, it did not respond", key)
          caller ! notification
      } 
    }
    receive {
      case `notification` => //ok, terminated
    }
  }
  protected case class TerminationRequest(from: Process)

  override def handlerProcess = process
}
