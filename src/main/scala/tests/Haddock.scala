package org.sufrin.glyph
package tests

import NaturalSize.{Col,Row}
import Brushes._


/**
 * Ad-hoc little tests
 */
object Haddock extends Application {

  def glyphs = "Aa Bb Cc DdDdDd\ndddddddd Ee Ff Gg Hh".split(' ').toList.map(unstyled.Label(_))
  def nums = "0 1 2 3 4 5 6 7 8 9 10 11 12 13 14".split(' ').toList.map(unstyled.Label(_))

  val rowcol =  Row(uniform=false, skip=20, frame = Brush("red.1"))(
    Col(frame=Brush("blue"),align=Centre, skip=4, uniform=false)(glyphs) :: glyphs ++ List(Col(frame=Brush("blue.1"), skip=4, uniform=false)(glyphs))
    )

  def GUI: Glyph = Col(skip=8)(
      NaturalSize.Grid(width=2, fg=blackLine, padx=10, pady=10)(glyphs),
      NaturalSize.Grid(height=2, bg=yellow, fg=greenFrame, padx=10, pady=10).Table(glyphs),
      NaturalSize.Grid(width=4, bg=yellow, fg=redFrame(width=2), padx=10, pady=10)(glyphs),
      NaturalSize.Grid(height=2, bg=yellow, fg=redLine, padx=10, pady=10).Table(glyphs),
      NaturalSize.Grid(height=2, fg=brownFrame, padx=10, pady=10).grid(width=2)(glyphs),
      NaturalSize.Grid(width=5, fg=greenFrame, padx=10, pady=10)(nums),
      NaturalSize.Grid(height=1, fg=pinkLine, padx=10, pady=10)(nums),

  ).enlarged(20) beside NaturalSize.Grid(width=1, fg=pinkLine, padx=10, pady=20)(nums) enlarged(20)


  def title: String = "Haddock"
}
