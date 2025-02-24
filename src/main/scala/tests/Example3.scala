package org.sufrin.glyph
package tests
import Glyphs._
import NaturalSize._
import DefaultBrushes._

import org.sufrin.glyph.styles.decoration.Framed


/**
 * Style to be used throughout the interface
 */

/**
 * Interface using styled glyphs
 */

trait Example3Interface {
  val variableColor: Brush = green()

  implicit val LocalStyle: StyleSheet =
    StyleSheet(buttonDecoration=Framed(fg = blue(width = 8, cap=ROUND), bg=white, radiusFactor = 0.3f),
          labelBackgroundBrush = variableColor
    )

  import styled.TextButton
  import styled.Label


  val GUI: Glyph = Col(bg=lightGrey).centered(
    Label("A simple label") enlarged(20),
    Row(TextButton("make it yellow") { _ => variableColor color yellowHuge.color },
        TextButton("make it red")    { _ => variableColor color red.color })
  ).enlarged(40f).enlarged(20f, bg=yellowHuge)

}

object Example3 extends Application  with Example3Interface {
  override def title: String = "Example 3"
}
