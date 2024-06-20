package org.sufrin.glyph
package tests

import Glyphs.Label

object Example1 extends Application  {
  val GUI: Glyph = Label("A simple label") enlarged 20
  override def title: String = "Example 1"
}
