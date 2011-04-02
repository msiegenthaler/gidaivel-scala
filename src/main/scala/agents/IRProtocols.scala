package ch.inventsoft
package gidaivel
package agents

import scalabase.time._


/** IR Protocol used by Sony */
object SonyIRProtocol extends SingleBitFixedLengthIRProtocol(
  name = "sony",
  repeats = 2,
  bitCount = 12,
  preample = List(0 μsec, 2450 μsec),
  bitZero = List(500 μsec, 650 μsec),
  bitOne = List(500 μsec, 1250 μsec),
  suffix = Nil
)


/** IR Protocol used by Accuphase */
object AccuphaseIRProtocol extends SingleBitFixedLengthIRProtocol(
  name = "accuphase",
  repeats = 1,
  bitCount = 32,
  preample = List(0 μsec, 8500 μsec, 4250 μsec, 600 μsec),
  bitZero = List(500 μsec, 600 μsec),
  bitOne = List(1550 μsec, 600 μsec),
  suffix = Nil
)
