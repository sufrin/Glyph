package org.sufrin.glyph
package styled

import org.sufrin.glyph.unstyled.static.FilledRect
import org.sufrin.logging.{Loggable, UNSET}

/**
 * A control panel supporting individual assigment of logging
 * levels to all objects declared as extensions of `Loggable`
 * and `SourceLoggable`.
 *
 *`className` maps a loggable to the name it should appear as on the control.
 * By default this is `loggable.className`, but it could be `loggable.simpleClassName`,
 * or a textually transformed version of the class name -- perhaps to elide package names.
 *
 * `filter`  (normally always true) Determines whether the given `Loggable` will be included
 *                  on the control panel
 *
 * TODO: fold panels if there are too many loggables for comfort
 * @param loggables normally `org.sufrin.logging.loggables` -- all registered `Loggable` companion objects
 * @param sheet  the style
 */


class LoggingPanel(loggables: Seq[Loggable]=org.sufrin.logging.loggables)(implicit sheet: StyleSheet) {
  import org.sufrin.logging.{Loggable, WARN, levelName}

  def className(loggable: Loggable): String  = loggable.className
  def filter(loggable: Loggable):    Boolean = true

  /**
   * Make a (new) gui suitable for controlling the logging levels of the currently-declared
   * collection of `Loggable`s.
   */
  def GUI(implicit sheet: StyleSheet): Glyph = {
    val names = "OFF/UNSET/FATAL/ERROR/WARN/INFO/FINE/FINER/FINEST/ALL".split('/').toSeq
    for { loggable <- loggables } if (loggable.level==UNSET) loggable.level = WARN

    def CONTROL(loggable: Loggable): Glyph = {
      lazy val buttons: RadioCheckBoxes =
        RadioCheckBoxes(names, prefer = "WARN") {
          case Some(i) => loggable.level = org.sufrin.logging.toLogLevel(names(i))
          case None    => buttons.select(4)
        }

      val name = levelName(loggable.level)
      locally { buttons.select(names.indexOf(name)) }

      NaturalSize.Col(align=Left)(
        Label(className(loggable)),
        NaturalSize.Row(align=Mid)(buttons.glyphRows) // TODO: why did buttons.arrangedHorizontally stop working?
      ).enlarged(5).framed()
    }

    NaturalSize.Col(align = Left)(loggables map CONTROL).enlarged(5)
  }

}

