package ch.inventsoft
package gidaivel

import scalabase.process._
import scalabase.time._
import scalabase.oip._
import scalabase.log._
import scalaxmpp._
import scalaxmpp.component._
import net.liftweb.json._
import JsonAST._
import JsonDSL._


/**
 * Storage for JSON data
 */
trait JsonStorage {
  def store(key: String, value: JValue): Unit @process
  def load(key: String): JValue @process
}

/**
 * Saves the JSONs into the java.util.pref.Preference for the user running the JVM.
 */
class JavaUtilsPreferenceStorage(node: String) extends JsonStorage with Log {
  import java.util.prefs._
  import net.liftweb.json._
  import JsonAST._
  private val prefs = Preferences.userRoot.node(node)

  override def store(key: String, value: JValue) = {
    val json = compact(render(value))
    prefs.put(key, json)
    prefs.sync
    log.trace("Saved value for '{}': {}", key, json)
  }

  override def load(key: String) = {
    prefs.sync
    val string = prefs.get(key, "")
    log.trace("Loaded value for '{}': {}", key, string)
    if (string.isEmpty) JNull
    else try {
      JsonParser.parse(string)
    } catch {
      case e: net.liftweb.json.JsonParser.ParseException =>
        log.info("Could not parse the value for {}: {}", key, e.getMessage)
        JNull
    }
  }
}
