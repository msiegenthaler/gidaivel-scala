package ch.inventsoft
package gidaivel

import scalabase.process._
import scalabase.time._
import scalabase.oip._
import scalaxmpp._
import scalaxmpp.component._
import net.liftweb.json._
import JsonAST._


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
class JavaUtilsPreferenceStorage(node: String) extends JsonStorage {
  import java.util.prefs._
  import net.liftweb.json._
  import JsonAST._
  protected val prefs = Preferences.userRoot.node(node)
  override def store(key: String, value: JValue) = {
    prefs.put(key, value.toString)
  }
  override def load(key: String) = {
    prefs.sync
    val string = prefs.get(key, "")
    if (string.isEmpty) JNull
    else try {
      JsonParser.parse(string)
    } catch {
      case e: java.text.ParseException => JNull
    }
  }
}
