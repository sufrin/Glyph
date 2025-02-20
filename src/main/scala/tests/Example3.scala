package org.sufrin.glyph
package tests
import Glyphs._
import NaturalSize._
import DefaultBrushes._

import org.sufrin.glyph.Styles.Decoration.Framed


/**
 * Style to be used throughout the interface
 */

/**
 * Interface using styled glyphs
 */

trait Example3Interface {
  val variableColor: Brush = green()

  implicit val LocalStyle: Sheet =
    Sheet(buttonFrame=Framed(fg = blue(width = 8, cap=ROUND), bg=white, radiusFactor = 0.3f),
          labelBackgroundBrush = variableColor
    )

  import sheeted.TextButton
  import sheeted.Label


  val GUI: Glyph = Col(bg=lightGrey).centered(
    Label("A simple label") enlarged(20),
    Row(TextButton("make it yellow") { _ => variableColor color yellowHuge.color },
        TextButton("make it red")    { _ => variableColor color red.color })
  ).enlarged(40f).enlarged(20f, bg=yellowHuge)

}

object Example3 extends Application  with Example3Interface {
  override def title: String = "Example 3"
}
