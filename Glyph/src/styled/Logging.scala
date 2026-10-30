package org.sufrin.glyph
package styled

import unstyled.static.FilledRect

/**
 *  Provides a facility for dynamic user-control (by GUI) of the logging
 *  levels of a collection of `Loggable` objects.
 *
 *  @see LoggingPanel
 */

object Logging {
  import org.sufrin.logging.{Loggable, WARN}

  private var _loggables: Seq[Loggable] = Seq.empty
  var className: Loggable => String = { case loggable: Loggable => loggable.simpleClassName }

  /**
   * Make a (new) gui suitable for controlling the logging levels of the currently-declared
   * collection of `Loggable`s.
   */
  def GUI(implicit style: StyleSheet): Glyph = {
    val names = "OFF/ UNSET/ FATAL/ ERROR/ WARN/ INFO/ FINE/ FINER/ FINEST/ ALL".split('/').toSeq
    for { loggable <- _loggables } loggable.level = WARN

    def CONTROL(loggable: Loggable): Glyph = {
       lazy val buttons: RadioCheckBoxes =
        RadioCheckBoxes(names, prefer = "WARN") {
          case Some(i) => loggable.level = org.sufrin.logging.toLogLevel(names(i).trim)
          case None    => buttons.select(4)
        }

      NaturalSize.Col(align=Left)(
        Label(className(loggable)),
        NaturalSize.Row(align=Mid)(buttons.glyphRows) // TODO: why did buttons.arrangedHorizontally stop working?
      ).enlarged(5).framed()
    }

    NaturalSize.Col(align = Left)(_loggables map CONTROL).enlarged(5)
  }

  /**
   * Decorate `hostGUI` with a thin, dark-grey, button that pops up a `GUI`. The location of
   * the `hostGUI` relative to the button is defined by `align`: `Top` and `Bottom` do the
   * obvious thing, whilst `Mid` sandwiches the host between two copies of the button.
   */
  def Decorate(hostGUI: Glyph, align: VAlignment)(implicit style: StyleSheet): Glyph = {
    lazy val loggingDialogue   = windowdialogues.Dialogue.FLASH(Logging.GUI, title = "Logging settings")

    lazy val loggingButtonBottom: Glyph =
      MenuGlyphButton(FilledRect(hostGUI.w, 5, Brushes.darkGrey), hint=Hint(0.75, "Logging controls", preferredLocation = Hint.North)) {
       _ => if (loggingDialogue.running.isDefined) {} else loggingDialogue.South(loggingButtonBottom).start(floating = false)
     }
    lazy val loggingButtonTop: Glyph =
      MenuGlyphButton(FilledRect(hostGUI.w, 5, Brushes.darkGrey), hint=Hint(0.75, "Logging controls")) {
       _ => if (loggingDialogue.running.isDefined) {} else loggingDialogue.South(loggingButtonTop).start(floating = false)
     }
    align match {
      case Bottom     =>  NaturalSize.Col(loggingButtonTop, hostGUI)
      case Top        =>  NaturalSize.Col(hostGUI, loggingButtonBottom)
      case _          =>  NaturalSize.Col(loggingButtonTop, hostGUI, loggingButtonBottom)
    }
  }

  /**
   * Declare the collection of loggables to be controlled; set their logging levels at `WARN`
   */
  def apply(loggables: Loggable*): Unit = {
    _loggables = loggables
    for { loggable <- loggables } loggable.level = WARN
  }

  def declareLoggables(loggables: Seq[Loggable]): Unit = _loggables=loggables

  /**
   * Declare the collection of loggables to be controlled; set their logging levels at `level`
   */
  def apply(level: Int, loggables: Iterable[Loggable]): Unit = {
    _loggables = loggables.toSeq
    for { loggable <- loggables } loggable.level = level
  }
}
