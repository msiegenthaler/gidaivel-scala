package ch.inventsoft
package gidaivel
package agents

import scalabase.time._
import org.scalatest._
import matchers._


class IRProtocolSpec extends ProcessSpec with ShouldMatchers {
  describe("SingleBitFixedLengthIRProtocol") {
    it("should serialize and reparse Sony") {
      val xml = IRProtocol.toXml(SonyIRProtocol)
      IRProtocol.fromXml(xml) match {
        case Some(protocol) =>
          protocol should be(SonyIRProtocol)
        case None => fail
      }
    }
    it("should serialize and reparse Accuphase") {
      val xml = IRProtocol.toXml(AccuphaseIRProtocol)
      IRProtocol.fromXml(xml) match {
        case Some(protocol) =>
          protocol should be(AccuphaseIRProtocol)
        case None => fail
      }
    }
  }

}
