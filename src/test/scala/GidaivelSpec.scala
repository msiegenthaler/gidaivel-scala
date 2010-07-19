package ch.inventsoft.gidaivel

import org.scalatest._
import matchers._


class GidaivelIdSpec extends Spec with ShouldMatchers {
  describe("GidaivelId") {
    it("should parse a, b.com") {
      val id = GidaivelId("a", "b.com")
      id.local should be("a")
      id.domain should be("b.com")
    }
    it("should parse a, b") {
      val id = GidaivelId("a", "b")
      id.local should be("a")
      id.domain should be("b")
    }
    it("should parse a@b.com") {
      val id = GidaivelId("a@b.com")
      id.local should be("a")
      id.domain should be("b.com")
    }
    it("should parse A, B.com") {
      val id = GidaivelId("A", "B.com")
      id.local should be("A")
      id.domain should be("B.com")
    }
    it("should parse hans-mueller.home+a, my-domain.house.com") {
      val id = GidaivelId("hans-mueller.home+a", "my-domain.house.com")
      id.local should be("hans-mueller.home+a")
      id.domain should be("my-domain.house.com")
    }
    def notParse(local: String, domain: String) = {
      try {
        GidaivelId(local, domain)
        fail
      } catch {
        case e: IllegalArgumentException => ()
      }
    }
    def notParseAll(all: String) = {
      try {
        GidaivelId(all)
        fail
      } catch {
        case e: IllegalArgumentException => ()
      }
    }
    it("should not parse hans mueller, b.com") {
      notParse("hans mueller", "b.com")
    }
    it("should not parse a, b ub.com") {
      notParse("a", "b ub.com")
    }
    it("should not parse a, a+1.com") {
      notParse("a", "a+1.com")
    }
    it("should not parse ad@aa@a.com") {
      notParseAll("ad@aa@a.com")
    }
    it("should not parse ad aa@a.com") {
      notParse("ad", "aa@a.com")
    }
    it("should not parse adaasd") {
      notParseAll("adaasd")
    }
    describe("parse") {
      it("a@my.com should parse to Some()") {
        GidaivelId.parse("a@my.com") should be(Some(GidaivelId("a", "my.com")))
      }
      it("a, my.com should parse to Some()") {
        GidaivelId.parse("a", "my.com") should be(Some(GidaivelId("a", "my.com")))
      }
      it("bla should parse to None") {
        GidaivelId.parse("bla") should be(None)
      }
      it("bla#a mydomain should parse to None") {
        GidaivelId.parse("bla#a", "mydomain") should be(None)
      }
      it("aa@bb@cc should parse to None") {
        GidaivelId.parse("aa@bb@cc") should be(None)
      }
    }

    describe("dev1@mydomain.ch") {
      val id = GidaivelId("dev1", "mydomain.ch")
      def assertEquals(a: GidaivelId, b: GidaivelId) = {
        assert(a == b)
        assert(b == a)
        assert(a.hashCode == b.hashCode)
      }
      it(".local should be dev1") {
        id.local should be("dev1")
      }
      it(".domain should be mydomain.ch") {
        id.domain should be("mydomain.ch")
      }
      it("should equal dev1@mydomain.ch") {
        val o = GidaivelId("dev1", "mydomain.ch")
        assertEquals(id, o)
      }
      it("should equal dev1@mydomain.ch (all)") {
        val o = GidaivelId("dev1@mydomain.ch")
        assertEquals(id, o)
      }
      it("should equal DEV1@mydomain.ch") {
        val o = GidaivelId("DEV1", "mydomain.ch")
        assertEquals(id, o)
      }
      it("should equal dev1@Mydomain.ch") {
        val o = GidaivelId("dev1", "Mydomain.ch")
        assertEquals(id, o)
      }
      it("should equal Dev1@Mydomain.ch") {
        val o = GidaivelId("Dev1", "Mydomain.ch")
        assertEquals(id, o)
      }
      it("should not equal dev2@mydomain.ch") {
        val o = GidaivelId("dev2", "mydomain.ch")
        assert(id != o)
        assert(o != id)
      }
      it("should not equal dev1@yourdomain.ch") {
        val o = GidaivelId("dev1", "yourdomain.ch")
        assert(id != o)
        assert(o != id)
      }
      it("should not equal dev2@yourdomain.ch") {
        val o = GidaivelId("dev2", "yourdomain.ch")
        assert(id != o)
        assert(o != id)
      }
      it("should unapply to (dev1,mydomain.ch") {
        id match {
          case GidaivelId(local, domain) =>
            local should be("dev1")
            domain should be("mydomain.ch")
          case other => fail
        }
      }
      describe("toString") {
        it("should be dev1@mydomain.ch") {
          id.toString should be("dev1@mydomain.ch")
        }
      }
    }
    describe("Dev1@Mydomain.com") {
      val id = GidaivelId("Dev1", "Mydomain.com")
      it(".local should be Dev1") {
        id.local should be("Dev1")
      }
      it(".domain should be Mydomain.com") {
        id.domain should be("Mydomain.com")
      }
      describe("toString") {
        it("should be Dev1@Mydomain.com") {
          id.toString should be("Dev1@Mydomain.com")
        }
      }
    }
  }
}
