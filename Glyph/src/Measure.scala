package org.sufrin.glyph

object Measure {
  import GlyphTypes.Scalar

  @inline private def  add(x: Scalar, y: Scalar): Scalar = x + y
  @inline private def  max(x: Scalar, y: Scalar): Scalar = x max y
  @inline private def  min(x: Scalar, y: Scalar): Scalar = x max y
  @inline private def  area(g: Shape): Scalar = g.h*g.w

  @inline private def  measure(dimension: Shape => Scalar, op: (Scalar, Scalar) => Scalar, glyphs: Iterable[Shape]): Scalar = {
    // glyphs.foldLeft(0f) { case (s: Scalar, g: Shape) => op(s, dimension(g)) }
    var r = 0f
    for { g <-glyphs } r = op(r, dimension(g))
    r
  }

  def maxWidth(gs: Iterable[Shape]): Scalar = measure(_.w, max, gs)
  def minWidth(gs: Iterable[Shape]): Scalar = measure(_.w, min, gs)
  def maxArea(gs: Iterable[Shape]): Scalar = measure(area, max, gs)
  def totalWidth(gs: Iterable[Shape]): Scalar = measure(_.w, add, gs)
  def maxHeight(gs: Iterable[Shape]): Scalar = measure(_.h, max, gs)
  def totalHeight(gs: Iterable[Shape]): Scalar = measure(_.h, add, gs)
}