package org.sufrin.glyph
package styled

import org.sufrin.logging.Loggable

/**
 * A control panel supporting individual assigment of logging
 * levels to all objects declared as extensions of `Loggable`
 * and `SourceLoggable`.
 *
 *@param className maps a loggable to the name it should appear as on the control.
 * By default this is `loggable.className`, but it could be `loggable.simpleClassName`,
 * or a textually transformed version of the class name -- perhaps to elide package names.
 *
 * @param filter  (normally always true) Determines whether the given `Loggable` will be included
 *                  on the control panel
 *
 * @param sheet  the style
 */
class LoggingPanel(className: Loggable => String = loggable => loggable.className, filter: Loggable => Boolean = loggable => true)(implicit sheet: StyleSheet) {
  import org.sufrin.logging._
  Logging.declareLoggables(loggables.filter(filter))
  Logging.className = className
  val GUI = Logging.GUI

}
