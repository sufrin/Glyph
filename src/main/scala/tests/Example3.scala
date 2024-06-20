package org.sufrin.glyph
package tests
import Glyphs._
import NaturalSize._

/**
 * Style to be used throughout the interface
 */

/**
 * Interface using styled glyphs
 */

trait Example3Interface {
  object LocalStyle extends Styles.Basic {
    import Styles.Decoration.Framed

    implicit val localButtons: Styles.ButtonStyle =
      buttonStyle.copy(frame = Framed(fg = blue(width = 8, cap=ROUND), bg=white, radiusFactor = 0.5f))

    implicit val localLabels: Styles.GlyphStyle =
      labelStyle.copy(font=GlyphTypes.Font(face, 40))
  }
  import styled.TextButton
  import styled.TextLayout.TextLabel

  import LocalStyle._
  import Spaces._


  val labelColor: Brush = green()

  val GUI: Glyph = Col(bg=lightGrey).centered(
    TextLabel("A simple label") enlarged(20, bg=labelColor), ex,
    Row(TextButton("make it yellow") { _ => labelColor color yellow.color },
        TextButton("make it red")    { _ => labelColor color red.color })
  ).enlarged(40f).enlarged(20f, bg=yellow)

}

object Example3 extends Application  with Example3Interface {
  override def title: String = "Example 3"
}
