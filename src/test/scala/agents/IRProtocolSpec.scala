package ch.inventsoft
package gidaivel
package agents

import scala.xml._
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
    it("should serialize sony to a given xml") {
      val xml = IRProtocol.toXml(SonyIRProtocol)
      val expected = <single-bit-fixed-length xmlns="urn:gidaivel:irrfgateway" name="sony"><repeats>2</repeats><bit-count>12</bit-count><preample-ns>0</preample-ns><preample-ns>2450000</preample-ns><bit-zero-ns>500000</bit-zero-ns><bit-zero-ns>650000</bit-zero-ns><bit-one-ns>500000</bit-one-ns><bit-one-ns>1250000</bit-one-ns></single-bit-fixed-length>
      
      def p(e: Node) = new scala.xml.PrettyPrinter(1000, 2).format(e)
      p(xml) should be(p(expected))
    }
    it("should parse an xml") {
      val xml = <single-bit-fixed-length name="sony" xmlns="urn:gidaivel:irrfgateway">
                  <repeats>2</repeats>
                  <bit-count>12</bit-count>
                  <preample-ns>0</preample-ns>
                  <preample-ns>2450000</preample-ns>
                  <bit-zero-ns>500000</bit-zero-ns>
                  <bit-zero-ns>650000</bit-zero-ns>
                  <bit-one-ns>500000</bit-one-ns>
                  <bit-one-ns>1250000</bit-one-ns>
                </single-bit-fixed-length>
      IRProtocol.fromXml(xml) should be(Some(SonyIRProtocol))
    }
  }

}
