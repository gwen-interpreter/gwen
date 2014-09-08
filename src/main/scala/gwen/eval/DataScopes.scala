package gwen.eval

import gwen.Predefs.Kestrel
import play.api.libs.json.JsObject
import play.api.libs.json.Json

/**
 * Maintains a map of named data scopes.
 */
class DataScopes(val dataScopes: Map[String, ScopedDataStack]) {

  private var scopes = dataScopes
  
  /**
   * Auxiliary default constructor.
   */
  def this() = this(Map[String, ScopedDataStack]())
  
  /**
   * Resets the current data scopes to the initial value.
   */
  def reset() {
    scopes = dataScopes
  }
  
  /**
   * Gets a named data scope (creates it if it does not exist)
   * 
   * @param name
   * 			the name of the data scope to get (or create and get)
   */
  def scope(name: String) = 
    if (scopes.contains(name)) {
      scopes(name)
    } else {
      new ScopedDataStack(name) tap { scope =>
        scopes += (name -> scope)
      }
    }
  
  /**
   * Returns the current state of all scoped data as a Json object.
   */  
  def toJson: JsObject = Json.obj("data" -> scopes.map { case (name, scope) => scope.toJson })
  
}