package ch.inventsoft
package gidaivel
package agents

import scalabase.time._
import org.scalatest._
import matchers._


class IRProtocolUtilsSpec extends ProcessSpec with ShouldMatchers {
  describe("durationConvert") {
    import IRProtocolUtils.durationConvert
    it("should have a unit of 1 if the only value is 1") {
      val (u,l) = durationConvert(Milliseconds)(1 :: 1  :: 1 :: Nil map(t => Duration(t, Milliseconds)))
      u should be(1)
      l should be(1 :: 1 :: 1 :: Nil)
    }
    it("should have a unit of 0 if the values are 0") {
      val (u,l) = durationConvert(Milliseconds)(0 :: 0  :: 0 :: Nil map(t => Duration(t, Milliseconds)))
      u should be(0)
      l should be(0 :: 0 :: 0 :: Nil)
    }
    it("should have a unit of 1 if the values are 0 and 1") {
      val (u,l) = durationConvert(Milliseconds)(1 :: 0  :: 1 :: Nil map(t => Duration(t, Milliseconds)))
      u should be(1)
      l should be(1 :: 0 :: 1 :: Nil)
    }
    it("should have a unit of 10 if the values are 0 and 10") {
      val (u,l) = durationConvert(Milliseconds)(10 :: 0  :: 10 :: Nil map(t => Duration(t, Milliseconds)))
      u should be(10)
      l should be(1 :: 0 :: 1 :: Nil)
    }
    it("should have a unit of 5 if the values are 0, 5 and 10") {
      val (u,l) = durationConvert(Milliseconds)(5 :: 0  :: 10 :: Nil map(t => Duration(t, Milliseconds)))
      u should be(5)
      l should be(1 :: 0 :: 2 :: Nil)
    }
    it("should have a unit of 5 if the values are 0, 5 and 100") {
      val (u,l) = durationConvert(Milliseconds)(5 :: 0  :: 100 :: Nil map(t => Duration(t, Milliseconds)))
      u should be(5)
      l should be(1 :: 0 :: 20 :: Nil)
    }
    it("should have a unit of 30 if the values are 600, 0, 1200, 7200") {
      val (u,l) = durationConvert(Milliseconds)(600 :: 0  :: 600 :: 1200 :: 7200 :: Nil map(t => Duration(t, Milliseconds)))
      u should be(40)
      l should be(15 :: 0 :: 15 :: 30 :: 180.toByte :: Nil)
    }
    it("should have a unit of 5 if the values are 612, 0, 1300, 8400, 721") {
      val (u,l) = durationConvert(Milliseconds)(612 :: 0  :: 720 :: 1300 :: 721 :: 8400 :: Nil map(t => Duration(t, Milliseconds)))
      u should be(48)
      l should be(13 :: 0 :: 15 :: 27 :: 15 :: 175.toByte :: Nil)
    }
    it("should have a unit of 22 if the values are 0, 2450, 500, 650 and 1250") {
      val list = List(0 μsec, 2450 μsec, 500 μsec, 650 μsec, 1250 μsec)
      val (u, d) = durationConvert(Microseconds)(list)
      u should be(25)
      d should be(List(0, 98, 20, 26, 50))
    }
  }
}
