package org.sufrin.glyph

import io.github.humbleui.jwm.EventMouseMove

/**
 * A mouse move that finds no natural glyph to hover over may be
 * made outside a window. The window's root glyph is informed when it
 * looks like the mouse is entering or leaving the window itself.
 *
 * The criterion for entering is that the motion is "just outside", but towards
 * the window. That for leaving the window is that the motion is "somewhat more than just
 * outside" the window and away from it.
 *
 * When an object of this class is used to follow the mouse it triggers
 * `enteringWindow` and `leavingWindow` methods in `root: RootGlyph` when
 * motion events a arising in `root.rootWindow` are delivered to it
 * by invocations of:
 *
 *  {{{
 *    accept(event: MouseMotionEvent)
 *  }}}
 *
 */
class MouseStateTracker(val root: RootGlyph) {
  val window: GlyphTypes.Window = root.rootWindow

  val enterMargin = 16
  val leaveMargin = 32

  /** Is the mouse inside the window */
  var inside: Option[Boolean] = None

  @inline def enteringWindow(): Unit =
    inside match {
      case Some(true) =>
      case None | Some(false) =>
        inside = Some(true)
        root.acceptRootGlyphEvent(RootEnterEvent(window))
    }

  @inline def leavingWindow(): Unit =
    inside match {
      case Some(false) =>
      case None | Some(true) =>
        inside = Some(false)
        root.acceptRootGlyphEvent(RootLeaveEvent(window))
    }

  trait Direction
  case class L(dist: Int) extends Direction
  case class R(dist: Int) extends Direction
  case object N extends Direction
  case class U(dist: Int) extends Direction
  case class D(dist: Int) extends Direction
  case object LeftWards extends Direction
  case object RightWards extends Direction
  case object UpWards extends Direction
  case object DownWards extends Direction


  def accept(event: EventMouseMove): Unit = {
    val x = event._x.toInt
    val y = event._y.toInt
    //org.sufrin.logging.Default.info(s"($x,$y) -> (${event._movementX}, ${event._movementY})")
    val dx = if (event._movementX > 0) RightWards else if (event._movementX < 0) LeftWards else N
    val dy = if (event._movementY > 0) DownWards else if (event._movementY < 0) UpWards else N
    val w = window.getWindowRect.getWidth
    val h = window.getWindowRect.getHeight
    val xOutside = if (x <= 0) L(-x) else if (x >= w) R(x - w) else N
    val yOutside = if (y <= 0) U(-y) else if (y >= h) D(y - h) else N

    (xOutside, dx) match {
      case (L(dist), RightWards) if dist <= enterMargin => enteringWindow()
      case (R(dist), LeftWards) if dist <= enterMargin  => enteringWindow()
      case (L(dist), LeftWards) if dist <= leaveMargin  => leavingWindow()
      case (R(dist), RightWards) if dist <= leaveMargin => leavingWindow()
      case _ =>
        (yOutside, dy) match {
          case (U(dist), DownWards) if dist <= enterMargin => enteringWindow()
          case (D(dist), UpWards) if dist <= enterMargin   => enteringWindow()
          case (U(dist), UpWards) if dist <= leaveMargin   => leavingWindow()
          case (D(dist), DownWards) if dist <= leaveMargin => leavingWindow()
          case _ =>
        }
    }
  }
}
