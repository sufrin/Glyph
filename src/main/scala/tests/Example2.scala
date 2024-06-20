package org.sufrin.glyph
package tests

import Glyphs._
import NaturalSize.{Col, Row}
import ReactiveGlyphs.TextButton

/**
 * Interface using non-styled glyphs
 */

trait Example2Interface {
  val blueish: Brush = blue(cap=ROUND, width=8)
  val labelColor: Brush = green()
  // val courier = Font(FontManager.default.matchFamilyStyle("Courier", FontStyle.NORMAL), 30f)

  val GUI: Glyph = Col(bg=lightGrey).centered(
    Label("A simple label", fg=white) enlarged(20, bg=labelColor),
    Row(TextButton("make it blue") { _ => labelColor color blue.color }.framed(fg=blueish, bg=blueish),
        TextButton("make it red")  { _ => labelColor color red.color }.framed(fg=blueish, bg=blueish))
  ).enlarged(40f).enlarged(20f, bg=yellow)

}

object Example2 extends Application  with Example2Interface {
  override def title: String = "Example 2"
}
