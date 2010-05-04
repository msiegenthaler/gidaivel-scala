package ch.inventsoft.gidaivel.avieul

import ch.inventsoft.scalabase.process._
import Messages._


/**
 * Gateway to a bunch of avieuls.
 */
trait PassadiDAvieuls {
  /**
   * Find all available avieuls.
   */
  def findAvieuls: MessageSelector[List[Avieul]]
}
