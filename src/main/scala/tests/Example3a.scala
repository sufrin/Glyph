
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

trait Example3aInterface {
  implicit object LocalStyle extends Styles.Sheet {
    object Super extends Styles.Sheet
    import Styles.Decoration.Blurred

    override implicit lazy val buttonStyle: Styles.ButtonStyle =
      Super.buttonStyle.copy(frame = Blurred(blur=10f, spread=10f, fg = yellow(width = 8, cap=SQUARE)))

    override implicit lazy val labelStyle: Styles.GlyphStyle =
      Super.labelStyle.copy(font=GlyphTypes.Font(face, 40))
  }

  import styled.TextButton
  import styled.TextLayout.TextLabel

  import LocalStyle.Spaces._


  val labelColor: Brush = green()

  val GUI: Glyph = Col(bg=lightGrey).centered(
    TextLabel("A simple label") enlarged(20, bg=labelColor), ex,
    Row(TextButton("make it yellow") { _ => labelColor color yellow.color },
        TextButton("make it red")    { _ => labelColor color red.color })
  ).enlarged(40f).enlarged(20f, bg=yellow)

}

object Example3a extends Application  with Example3aInterface {
  override def title: String = "Example 3a"
}
