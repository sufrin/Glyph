package org.sufrin.glyph

/**
 * The classes/traits herein embody a way of constructing reactive glyphs (glyphs that respond to
 * user gestures) uniformly, using a uniform encoding of UI events.
 */

package gesture


import GlyphTypes.{EventKey, Window}
import Modifiers.{Bitmap, Command, Control, Pressed, Primary, Secondary, Shift}

import io.github.humbleui.jwm.{EventMouseButton, EventMouseMove, EventMouseScroll, Key}

/**
 * Encoding of a user's gesture at a glyph that  will (in all cases but `GlyphEnter`) have
 * focus.
 */
trait Gesture {
  val modifiers: Bitmap
  val PRESSED       = modifiers.includeAll(Pressed)
  val CONTROL       = modifiers.includeSome(Command  | Control)
  val PRIMARY       = modifiers.includeAll(Primary   | Pressed)
  val SECONDARY     = modifiers.includeAll(Secondary | Pressed) || (PRIMARY && CONTROL)
  val COMPLEMENT    = modifiers.includeSome(Shift)
  val SHIFT         = modifiers.includeSome(Shift)
}

case class Keystroke(key: Key, modifiers: Bitmap) extends Gesture
case class MouseClick(modifiers: Bitmap) extends Gesture
case class MouseMove(modifiers: Bitmap) extends Gesture
case class MouseEnters(modifiers: Bitmap, leaving: Option[ReactiveGlyph], entering: Option[ReactiveGlyph]) extends Gesture
case class MouseLeaves(modifiers: Bitmap, leaving: Option[ReactiveGlyph], entering: Option[ReactiveGlyph]) extends Gesture
case class MouseScroll(modifiers: Bitmap) extends Gesture

/**
 *
 */
trait GestureHandler {
  @inline def handle(event: EventKey, location: Vec, window: Window): Unit =
    handle(Keystroke(event.getKey, Modifiers(event)), location, Vec.Zero)

  @inline def handle(event: EventMouseMove, location: Vec, window: Window): Unit = {
    val (dx, dy) = (event.getMovementX.toFloat, event.getMovementY.toFloat)
    handle(MouseMove(Modifiers(event)), location, Vec(dx, dy))
  }

  @inline def handle(event: EventMouseButton, location: Vec, window: Window): Unit = {
    handle(MouseClick(Modifiers(event)), location, Vec.Zero)
  }

  @inline def handle(event: EventMouseScroll, location: Vec, window: Window): Unit = {
    handle(MouseScroll(Modifiers(event)), location, Vec(event.getDeltaX, event.getDeltaY))
  }

  @inline def handle(event: GlyphEvent, location: Vec, window: Window): Unit = {
    event match {
      case GlyphEnter(leaving, entering, modifiers) => handle(MouseEnters(modifiers, leaving, entering), location, Vec.Zero)
      case GlyphLeave(leaving, entering, modifiers) => handle(MouseLeaves(modifiers, leaving, entering), location, Vec.Zero)
    }
  }

  /**
   * Implements the reaction to the given gesture made at the given `location`, when the mouse has moved by
   * `delta` (which is `Vec.Zero` except for a `MouseMove` or a
   */
  def handle(gesture: Gesture, location: Vec, delta: Vec): Unit
}


/**
 * A `ReactiveGlyph` all of whose reactions are implemented by the single method:
 * {{{
 *    def handle(gesture: Gesture, loc: Vec, delta: Vec): Unit
 * }}}
 *
 */
abstract class GestureBasedReactiveGlyph extends ReactiveGlyph with GestureHandler {
  override def accept(event: EventKey, location: Vec, window: Window): Unit = handle(event, location, window)
  override def accept(event: EventMouseMove, location: Vec, window: Window): Unit = handle(event, location, window)
  override def accept(event: EventMouseButton, location: Vec, window: Window): Unit = handle(event, location, window)
  override def accept(event: GlyphEvent, location: Vec, window: Window): Unit = handle(event, location, window)
  override def accept(event: EventMouseScroll, location: Vec, window: Window): Unit = handle(event, location, window)
}
