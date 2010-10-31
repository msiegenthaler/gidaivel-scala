package ch.inventsoft
package gidaivel
package passadi

import scalabase.process._


/**
 * Gateway to avieuls.
 * The avieuls are not activly polled, so avieuls/service might report outdated
 * lists. Use refresh if a current view is needed.
 */
trait Passadi {

  /** All avieuls currently active and known */
  def avieuls: Selector[Iterable[Avieul]] @process

  /** All services currently known and active */
  def services: Selector[Iterable[AvieulService]] @process

  /**
   * Forces a refresh of known avieuls, all avieuls are told to report
   * to the passadi. Avieuls not reporting in within some amount of time
   * are considered gone.
   * Attention: This uses power at the avieuls and forces them to exit their
   * sleep state, so don't call to often.
   */
  def refresh: Selector[Iterable[Avieul]] @process

  /**
   * Set the listener for changes in the avieul configuration (new or changed or
   * removed avieuls).
   * @return unregister function
   */
  def changeListener(listener: Function1[PassadiChange, Unit @process]): Unit @process

  def close: Selector[Unit] @process
}

sealed trait PassadiChange {
  val avieul: Avieul
}
case class NewAvieul(avieul: Avieul) extends PassadiChange
case class ChangedAvieul(avieul: Avieul) extends PassadiChange
case class RemovedAvieul(avieul: Avieul) extends PassadiChange
