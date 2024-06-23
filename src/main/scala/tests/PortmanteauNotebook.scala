package org.sufrin.glyph
package tests

import GlyphTypes._


object PortmanteauNotebook extends Application  {

  trait Local extends Styles.DefaultSheet {
    override lazy val buttonFontSize: Scalar = 40
  }

  object LocalStyle extends Local

  /**
   * Style derived from the `Local` stylesheet: with
   * blurred white-on-blue buttons.
   *
   * TODO: rethink the way in which concrete style sheet objects are
   *       built from scratch, so that differential/incremental
   *       specifications feels more straightforward.
   */
  implicit val blurred: StyleSheet = new LocalStyle.Derived {
    import Styles._
    import DefaultBrushes._
    override lazy val buttonStyle: Styles.ButtonStyle  =
      delegate.buttonStyle.copy(frame = Decoration.Blurred(blue, nothing, 15f, 5f),
        up = GlyphStyle(font = buttonFont, fg = white, bg = nothing))
  }

  val interface = new PortmanteauInterface()

  lazy val GUI: Glyph = interface.asRNotebook

  def title = s"""PortmanteauNotebook -scale=$scaleFactor ${extraArgs.mkString(", ")}"""

  override
  val defaultIconPath: Option[String] = Some ("./flag.png")

  override
  def onClose(window: Window): Unit = interface.confirmCloseOn(GUI)(window)
}
