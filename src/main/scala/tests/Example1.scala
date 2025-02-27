package org.sufrin.glyph
package tests

import Glyphs.Label

object Example1 extends Application  {
  val font = FontFamily("Courier").makeFont(size=32)
  val GUI: Glyph = Label("A simple label", font)
  override def title: String = "Example 1"
}
