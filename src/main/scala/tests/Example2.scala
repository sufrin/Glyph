package org.sufrin.glyph
package tests

import unstyled.static._
import NaturalSize.{Col, Row}
import unstyled.reactive.TextButton
import Brushes._
import GlyphTypes.Font

/**
 * Interface using non-styled glyphs
 */

trait Example2Interface {
  val buttonFrame:     Brush = blue(cap=ROUND, width=18)
  val labelBackground: Brush = green().rounded(18)
  val font:            Font  = FontFamily("Courier").makeFont(size=32)
  val spacer = Rect(0, 20, fg=transparent)

  val GUI: Glyph = Col(align=Center, bg=lightGrey) (
    Label("A simple label", font, fg=white, bg=labelBackground) enlarged(20),
    spacer,
    Row(skip=10)(
        TextButton("make it blue", bg=invisible) { _ => labelBackground.color(blue.color) }.framed(buttonFrame),
        TextButton("make it red", bg=invisible)  { _ => labelBackground.color(red.color) }.framed(buttonFrame)
    )
  ).enlarged(20)
}

object Example2 extends Application  with Example2Interface {
  override def title: String = s"Example 2"
}
