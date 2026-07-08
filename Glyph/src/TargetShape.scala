package org.sufrin.glyph

import Brush.ROUND
import Brushes.red
import Shape.{circle, rect}
import GlyphTypes.Scalar
import GlyphTypes.PaintMode._

/** A variably-located and rotated shape currently located at `(x,y)`. Shown
  * with a "handle" whose appearance can be changed when hovering or selected.
  */
case class TargetShape(x$ : Scalar, y$ : Scalar, shape$ : Shape)
    extends LocatedShape(x$, y$, shape$) {

  val handleBrush0 =
    Brushes.yellow(width = 1f, mode = STROKE, cap = ROUND, alpha = 0.6f)
  val handleBrush  = handleBrush0.copy()
  val centreBrush0 =
    Brushes.red(width = 1, mode = STROKE, cap = ROUND, alpha = 0.7f)
  val centreBrush = centreBrush0.copy()
  val radius      = ((w max h) / 12) min 5

  private val handle = rect(radius, radius)(handleBrush)
  val deltaHandle    = handle.diagonal * 0.5f
  val handles        = Handles(
    this,
    for { loc <- Shape.cardinalPoints(this.shape)} yield Handle(
      this,
      handle,
      loc - deltaHandle
    )
  )

  val centreHandle = circle(radius)(centreBrush)

  @inline private def centeredAbout(g: Shape): Vec =
    Vec(x, y) + ((diagonal - g.diagonal) * 0.5f)

  override def withForeground(brush: Brush): TargetShape =
    new TargetShape(x, y, shape$.withBrushes(brush))

  def setHovering(state: Boolean): Unit = {
    if (state) {
      handleBrush.alpha(1f)
      handleBrush.strokeWidth(2 * handleBrush0.strokeWidth)
    } else {
      handleBrush.alpha(handleBrush0.alpha)
      handleBrush.strokeWidth(handleBrush0.strokeWidth)
      handleBrush.color(handleBrush0.color)
    }
  }

  def setSelected(state: Boolean): Unit = {
    if (state) {
      handleBrush.mode(FILL)
      handleBrush.alpha(1.0)
      handleBrush.color(red.color)
      centreBrush.mode(FILL)
    } else {
      handleBrush.mode(STROKE)
      handleBrush.alpha(handleBrush0.alpha)
      handleBrush.color(handleBrush0.color)
      centreBrush.mode(centreBrush0.mode)
    }
  }

  /** one of the glyph's handle(s) contains the mouse pointer */
  def canHandle(point: Vec): Boolean =
    handles.canHandle(point - (x, y)).nonEmpty

  /** Draw the shape and its auxiliary shapes */
  override def draw(surface: Surface): Unit = {
    surface.withOrigin(x, y) {
      shape.draw(surface)
      handles.draw(surface)
    }
    surface.withOrigin(centeredAbout(centreHandle)) {
      centreHandle.draw(surface)
    }
  }

  override def copyState: TargetShape = TargetShape(x, y, shape)

  override def toString: String = s"$shape.variable${(x, y)}"

}
