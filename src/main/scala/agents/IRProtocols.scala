package ch.inventsoft
package gidaivel
package agents

import scalabase.time._


/** IR Protocol used by Sony */
object SonyIRProtocol extends SingleBitFixedLengthIRProtocol {
  override val repeats = 2
  override val bitCount = 12
  override val preample = List(0 μsec, 2450 μsec)
  override val bitZero = List(500 μsec, 650 μsec)
  override val bitOne = List(500 μsec, 1250 μsec)
  override val suffix = Nil
  override def toString = "Sony"
}


/** IR Protocol used by Accuphase */
object AccuphaseIRProtocol extends SingleBitFixedLengthIRProtocol {
  override val repeats = 1
  override val bitCount = 32
  override val preample = List(0 μsec, 8500 μsec, 4250 μsec, 600 μsec)
  override val bitZero = List(500 μsec, 600 μsec)
  override val bitOne = List(1550 μsec, 600 μsec)
  override val suffix = Nil
  override def toString = "Accuphase"
}
