
package org.sufrin.glyph
package tests

import Glyphs._
import NaturalSize._
import DefaultBrushes._

import org.sufrin.glyph.styles.decoration


/**
 * Interface using styled glyphs
 */

trait Example3aInterface {
  val variableColor: Brush = green()

  implicit val LocalStyle: StyleSheet =
    StyleSheet(buttonDecoration=decoration.Blurred(blur=10f, spread=10f, fg = yellowHuge(width = 8, cap=SQUARE)),
          labelBackgroundBrush = variableColor
    )

  import styled.TextButton
  import styled.Label

  val labelColor: Brush = green()

  val GUI: Glyph = Col(bg=lightGrey, align=Center)(
    Label("A simple label") enlarged(20),
    Row(TextButton("make it yellow") { _ => variableColor color yellowHuge.color },
        TextButton("make it red")    { _ => variableColor color red.color })
  ).enlarged(40f).enlarged(20f, bg=yellowHuge)

}

object Example3a extends Application  with Example3aInterface {
  override def title: String = "Example 3a"
}
