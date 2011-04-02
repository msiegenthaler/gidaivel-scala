package ch.inventsoft
package gidaivel

import Json._
import net.liftweb.json._
import JsonAST._
import JsonDSL._
import JsonParser._
import scalaxmpp._
import org.scalatest._
import matchers._


class JsonSpec extends ProcessSpec with ShouldMatchers {
  describe("JsonMapper") {
    describe("seqOf") {
      it("should serialize List('a', 'b') to ['a', 'b']") {
        val json = seqOf(Text).serialize(List("a", "b"))
        compact(render(json)) should be("""["a","b"]""")
      }
      it("should parse ['a','b'] to List('a','b')") {
        val json = parse("""["a", "b"]""")
        seqOf(Text).parse(json) should be(Some(List("a", "b")))
      }
      it("should serialize List() to []") {
        val json = seqOf(Text).serialize(List())
        compact(render(json)) should be("[]")
      }
      it("should parse [] to List()") {
        val json = parse("[]")
        seqOf(Text).parse(json) should be(Some(Nil))
      }
      it("should parse {} to None") {
        val json = parse("{}")
        seqOf(Text).parse(json) should be(None)
      } 
      it("should parse 'Hi' to None") {
        val json = JString("Hi")
        seqOf(Text).parse(json) should be(None)
      }
    }
    describe("XmlElem") {
      it("should serialize <a/> to \"<a/>\"") {
        val json = XmlElem.serialize(<a/>)
        compact(render(json)) should be("\"<a></a>\"")
      }
      it("should parse \"<a/>\" to <a/>") {
        val json = JString("<a/>")
        XmlElem.parse(json) should be(Some(<a/>))
      }
      it("should parse \"Hi\" to None") {
        val json = JString("Hi")
        XmlElem.parse(json) should be(None)
      }
      it("should parse [\"<a/>\"] to None") {
        val json = parse("""["<a/>"]""")
        XmlElem.parse(json) should be(None)
      }
    }
    describe("JID") {
      it("should serialize a@b.com to \"a@b.com\"") {
        val json = Jid.serialize(JID("a", "b.com"))
        compact(render(json)) should be("\"a@b.com\"")
      }
      it("should parse \"a@b.com\" to a@b.com") {
        val json = JString("a@b.com")
        Jid.parse(json) should be(Some(JID("a", "b.com")))
      }
      it("should parse \"a@b.com/c\" to a@b.com/c") {
        val json = JString("a@b.com/c")
        Jid.parse(json) should be(Some(JID("a", "b.com", "c")))
      }
    }
    describe("fromObject") {
      val no = fromObject("name", Text)
      it("""should parse field name from {"name": "Hans", "age": 31} to "Hans"""") {
        val json = parse("""{"name": "Hans", "age": 31}""")
        no.parse(json) should be(Some("Hans"))
      }
      it("""should parse field name from {"age": 31} to None""") {
        val json = parse("""{"age": 31}""")
        no.parse(json) should be(None)
      }
      it("""should serialize "Mario" to {"name":"Mario"}""") {
        val a = JObject(no.serialize("Mario") :: Nil)
        compact(render(a)) should be("""{"name":"Mario"}""")
      }
    }
    describe("composing") {
      it("""should parse field jid of {"name": "Mario","jid":"a@b.com/c"} to JID(a@b.com/c)""") {
        val json = parse("""{"name": "Mario","jid":"a@b.com/c"}""")
        fromObject("jid", Jid).parse(json) should be(Some(JID("a", "b.com", "c")))
      }
    }
  }
}
