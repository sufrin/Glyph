package org.sufrin.glyph
package tests

import NaturalSize.{Col,Row,Grid}
import Brushes._
import unstyled.Label


/**
 * Ad-hoc little tests
 */
object Haddock extends Application {

  def glyphs = "Bb Cc DdDdDd\ndddddddd Ee Ff Gg Hh".split(' ').toList.map(Label(_))
  def nums = "1 2 3 4 5 6 7 8 9 10 11 12 13 14".split(' ').toList.map(Label(_))

  def GUI: Glyph = Col(skip=8, align=Centre)(
      Grid(width=2,  fg=blackLine, padx=10, pady=10)(Label("Glyph\nw=2")::glyphs),
      Grid(width=4, bg=yellow, fg=redLine, padx=10, pady=10)(Label("Glyph\nw=4")::glyphs),
      Grid(height=2, fg=greenLine, padx=10, pady=10).table(Label("Glyph\nh=2\ntable")::glyphs),
      Grid(width=4, bg=yellow, fg=brownLine, padx=10, pady=10).table(Label("Glyph\nw=4\ntable")::glyphs),
  ).enlarged(10).framed(brownFrame).enlarged(20) beside
    Col(skip=8, align=Centre)(
      Grid(width=5, fg=greenLine, padx=10, pady=10)(Label("Glyph\nw=5")::nums),
      Grid(height=3, fg=greenLine, padx=10, pady=10)(Label("Glyph\nh=3")::nums),
      Grid(width=5, fg=greenLine, padx=10, pady=10).rows(Label("Glyph\nw=5\nrows")::nums),
      Grid(height=3, fg=greenLine, padx=10, pady=10).cols(Label("Glyph\nh=3\ncols")::nums),
      Grid(height=3, fg=greenLine, padx=10, pady=10).table(Label("Glyph\nh=3\ntable")::nums),
      ).enlarged(10).framed(brownFrame).enlarged(20)

  def title: String = "Haddock"
}
