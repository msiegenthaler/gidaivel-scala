package ch.inventsoft.gidaivel.avieul

import ch.inventsoft.scalabase.process._
import Messages._


/**
 * Gateway to a bunch of avieuls.
 */
trait PassadiDAvieuls {
  /**
   * Unique identifier of the passadi.
   */
//  val id: String
//TODO
  
  /**
   * Find all available avieuls.
   */
  def findAvieuls: MessageSelector[List[Avieul]]

  /**
   * Find all services.
   */
  def findServices: MessageSelector[List[AvieulService]]

  /**
   * Stop the passadi.
   */
  def close: Unit
}
