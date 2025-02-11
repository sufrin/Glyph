
package org.sufrin.glyph
package tests

import Glyphs._
import NaturalSize._
import DefaultBrushes._

import org.sufrin.glyph.Styles.Decoration


/**
 * Interface using styled glyphs
 */

trait Example3aInterface {
  val variableColor: Brush = green()

  implicit val LocalStyle: Sheet =
    Sheet(buttonFrame=Decoration.Blurred(blur=10f, spread=10f, fg = yellow(width = 8, cap=SQUARE)),
          labelBackgroundBrush = variableColor
    )

  import sheeted.TextButton
  import sheeted.Label

  val labelColor: Brush = green()

  val GUI: Glyph = Col(bg=lightGrey).centered(
    Label("A simple label") enlarged(20),
    Row(TextButton("make it yellow") { _ => variableColor color yellow.color },
        TextButton("make it red")    { _ => variableColor color red.color })
  ).enlarged(40f).enlarged(20f, bg=yellow)

}

object Example3a extends Application  with Example3aInterface {
  override def title: String = "Example 3a"
}
