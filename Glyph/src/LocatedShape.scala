package org.sufrin.glyph

import GlyphTypes.Scalar

class LocatedShape(var x: Scalar, var y: Scalar, val shape: Shape) {
  def w: Scalar = shape.w
  def h: Scalar = shape.h

  @inline def topLeft: Vec                        = Vec(x, y)
  @inline private def centred(g: Shape): Vec =
    Vec(x, y) + ((diagonal - g.diagonal) * 0.5f)
  @inline def centre: Vec = topLeft + (diagonal * 0.5f)

  def withForeground(brush: Brush): LocatedShape =
    new LocatedShape(x, y, shape.withBrushes(brush))

  /** the glyph's bounding box contains the mouse pointer */
  def isBeneath(point: Vec): Boolean = shape.encloses((point - (x, y)))

  /** Draw the shape and its auxiliary shapes */
  def draw(surface: Surface): Unit = {
    surface.withOrigin(x, y) {
      shape.draw(surface)
    }
  }

  def copyState: LocatedShape = LocatedShape(x, y, shape)

  def diagonal: Vec = shape.diagonal

  override def toString: String = s"$shape.variable${(x, y)}"

  def isIn(set: Seq[LocatedShape]): Boolean = set contains this

  def notIn(set: Seq[LocatedShape]): Boolean = !(set contains this)

  def moveTo(x: Scalar, y: Scalar): Unit = {
    this.x = x; this.y = y
  }

  def placeAt(x: Scalar, y: Scalar): Unit = {
    this.x = x; this.y = y
  }

  def placeAt(loc: Vec): Unit = {
    this.x = loc.x; this.y = loc.y
  }

  def centerAt(loc: Vec): Unit = {
    this.x = loc.x - (w * 0.5f); this.y = loc.y - (w * 0.5f)
  }

  def moveBy(x: Scalar, y: Scalar): Unit = {
    this.x += x; this.y += y
  }

  def center: Vec = (diagonal * 0.5f) + (x, y)

  def withBrushes(fg: Brush = shape.fg, bg: Brush = shape.bg): LocatedShape =
    LocatedShape(x, y, shape.withBrushes(fg, bg))

}

object LocatedShape {
  def apply(x: Scalar, y: Scalar, shape: Shape): LocatedShape =
    new LocatedShape(x, y, shape)
  def apply(pos: Vec, shape: Shape): LocatedShape =
    new LocatedShape(pos.x, pos.y, shape)
  def apply(pair: (Scalar, Scalar), shape: Shape): LocatedShape =
    new LocatedShape(pair._1, pair._2, shape)
}
