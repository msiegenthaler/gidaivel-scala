package ch.inventsoft
package gidaivel
package agents

import scala.xml._
import scalabase.time._
import scalaxmpp.component._

sealed trait IRProtocol {
  val name: String
}

object IRProtocol {
  val xmlNamespace = "urn:gidaivel:irrfgateway"
  def toXml(protocol: IRProtocol): Elem = protocol match {
    case protocol: SingleBitFixedLengthIRProtocol =>
      <single-bit-fixed-length xmlns={xmlNamespace}>
        <name>{protocol.name}</name>
        <repeats>{protocol.repeats}</repeats>
        <bit-count>{protocol.bitCount}</bit-count>
        {protocol.preample.map(e => <preample-ns>{e.amountAs(Nanoseconds)}</preample-ns>)}
        {protocol.bitZero.map(e => <bit-zero-ns>{e.amountAs(Nanoseconds)}</bit-zero-ns>)}
        {protocol.bitOne.map(e => <bit-one-ns>{e.amountAs(Nanoseconds)}</bit-one-ns>)}
        {protocol.suffix.map(e => <suffix-ns>{e.amountAs(Nanoseconds)}</suffix-ns>)}
      </single-bit-fixed-length>
  }
  private def text(n: NodeSeq) = {
    val t = n.text
    if (t.isEmpty) None else Some(t)
  }
  private def int(n: NodeSeq) = text(n).flatMap { s =>
    try(Some(s.toInt)) catch { case e: NumberFormatException => None }
  }
  private def nss(n: NodeSeq) = Some(n.flatMap(int(_).map(_ ns)))
  def fromXml(elem: Elem): Option[IRProtocol] = elem match {
    case ElemName("single-bit-fixed-length", `xmlNamespace`) =>
      for {
        name <- text(elem \ "name")
        repeats <- int(elem \ "repeats")
        bitCount <- int(elem \ "bit-count")
        preample <- nss(elem \ "preample-ns")
        bitOne <- nss(elem \ "bit-one-ns")
        bitZero <- nss(elem \ "bit-zero-ns")
        suffix <- nss(elem \ "suffix-ns")
      } yield {
        SingleBitFixedLengthIRProtocol(name, repeats, preample, bitCount, bitOne, bitZero, suffix)
      }
  }
}

/**
 * Protocol for IR-commands with a fixed length (in bits) and "single-bit" IR sequences.
 */
case class SingleBitFixedLengthIRProtocol(
  override val name: String,
  /** number of times a command is repeated */
  repeats: Int,
  /** pulses sent before each command */
  preample: Seq[Duration],
  /** number of bits per command */
  bitCount: Int,
  /** pulses representing a binary 1 */
  bitOne: Seq[Duration],
  /** pulses representing a binary 0 */
  bitZero: Seq[Duration],
  /** pulses sent after each command */
  suffix: Seq[Duration]
) extends IRProtocol

