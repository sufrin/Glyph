
package org.sufrin.glyph.tests
import org.sufrin.glyph.Glyphs._
import org.sufrin.glyph.NaturalSize._
import org.sufrin.glyph._

/**
 * Style to be used throughout the interface
 */

/**
 * Interface using styled glyphs
 */

trait Example3aInterface {
  object LocalStyle extends Styles.Basic {
    import Styles.Decoration.{Blurred}

    implicit val localButtons: Styles.ButtonStyle =
      buttonStyle.copy(frame = Blurred(blur=10f, spread=10f, fg = yellow(width = 8, cap=SQUARE)))

    implicit val localLabels: Styles.GlyphStyle =
      labelStyle.copy(font=GlyphTypes.Font(face, 40))
  }
  import LocalStyle._
  import Spaces._
  import styled.TextButton
  import styled.TextLayout.TextLabel


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
