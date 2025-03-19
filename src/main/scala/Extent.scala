package org.sufrin.glyph

import GlyphTypes.Scalar

/**
 * An extent represents a rectangular region of the screen. Extents are
 * used to limit the scope of certain mouse gestures to particular regions.
 *
 * @see unstyled.dynamic.SplitScreen
 */
trait Extent {
  val isEmpty: Boolean
  /**  */
  def contains(point: Vec): Boolean
}

object EmptyExtent extends Extent {
  override def toString: String = "EmptyExtent"
  val isEmpty: Boolean = true
  def contains(point: Vec): Boolean = true
}

case class ScaledExtent(scale: Scalar, origin: Vec, extent: Vec) extends Extent {
  val isEmpty: Boolean  = (extent eq Vec.Zero) || extent.x==0 && extent.y==0
  private val extentX = origin.x+extent.x*scale
  private val extentY = origin.y+extent.y*scale
  def contains(point: Vec): Boolean = {
    val scaledPoint = point scaled scale
    isEmpty || (
      (origin.x <= scaledPoint.x) && (scaledPoint.x < extentX) &&
        (origin.y <= scaledPoint.y) && (scaledPoint.y < extentY))
  }
}
