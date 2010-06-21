package ch.inventsoft.gidaivel

import org.scalatest._
import matchers._
import ch.inventsoft.scalabase.process._
import Messages._
import ch.inventsoft.scalabase.oip._
import ch.inventsoft.scalabase.time._
import ch.inventsoft.xbee._
import ch.inventsoft.gidaivel.avieul._
import AvieulProtocol._
import scala.concurrent.SyncVar


class IRReceiverSpec extends ProcessSpec with ShouldMatchers {

  describe("convertDurations") {
    import IRReceiver._
    it_("should have a unit of 1 if the only value is 1") {
      val (u,l) = durationConvert(Milliseconds)(1 :: 1  :: 1 :: Nil map(t => Duration(t, Milliseconds)))
      u should be(1)
      l should be(1 :: 1 :: 1 :: Nil)
    }
    it_("should have a unit of 0 if the values are 0") {
      val (u,l) = durationConvert(Milliseconds)(0 :: 0  :: 0 :: Nil map(t => Duration(t, Milliseconds)))
      u should be(0)
      l should be(0 :: 0 :: 0 :: Nil)
    }
    it_("should have a unit of 1 if the values are 0 and 1") {
      val (u,l) = durationConvert(Milliseconds)(1 :: 0  :: 1 :: Nil map(t => Duration(t, Milliseconds)))
      u should be(1)
      l should be(1 :: 0 :: 1 :: Nil)
    }
    it_("should have a unit of 10 if the values are 0 and 10") {
      val (u,l) = durationConvert(Milliseconds)(10 :: 0  :: 10 :: Nil map(t => Duration(t, Milliseconds)))
      u should be(10)
      l should be(1 :: 0 :: 1 :: Nil)
    }
    it_("should have a unit of 5 if the values are 0, 5 and 10") {
      val (u,l) = durationConvert(Milliseconds)(5 :: 0  :: 10 :: Nil map(t => Duration(t, Milliseconds)))
      u should be(5)
      l should be(1 :: 0 :: 2 :: Nil)
    }
    it_("should have a unit of 5 if the values are 0, 5 and 100") {
      val (u,l) = durationConvert(Milliseconds)(5 :: 0  :: 100 :: Nil map(t => Duration(t, Milliseconds)))
      u should be(5)
      l should be(1 :: 0 :: 20 :: Nil)
    }
    it_("should have a unit of 30 if the values are 600, 0, 1200, 7200") {
      val (u,l) = durationConvert(Milliseconds)(600 :: 0  :: 600 :: 1200 :: 7200 :: Nil map(t => Duration(t, Milliseconds)))
      u should be(40)
      l should be(15 :: 0 :: 15 :: 30 :: 180.toByte :: Nil)
    }
    it_("should have a unit of 5 if the values are 612, 0, 1300, 8400, 721") {
      val (u,l) = durationConvert(Milliseconds)(612 :: 0  :: 720 :: 1300 :: 721 :: 8400 :: Nil map(t => Duration(t, Milliseconds)))
      u should be(48)
      l should be(13 :: 0 :: 15 :: 27 :: 15 :: 175.toByte :: Nil)
    }
  }
}