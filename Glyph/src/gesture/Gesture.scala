package org.sufrin.glyph

/**
 * The classes/traits herein embody a way of constructing reactive glyphs (glyphs that respond to
 * user gestures) uniformly, using a uniform encoding of UI events.
 */

package gesture


import io.github.humbleui.jwm._
import org.sufrin.glyph.GlyphTypes.EventKey
import org.sufrin.glyph.Modifiers.{Bitmap, Command, Control, Pressed, Primary, Secondary, Shift}

/**
 * Encoding of a user's gesture at a glyph that  will (in all cases but `GlyphEnter`) have
 * focus.
 */
sealed trait Gesture {
  val modifiers: Bitmap
  val PRESSED       = modifiers.includeAll(Pressed)
  val CONTROL       = modifiers.includeSome(Command  | Control)
  val PRIMARY       = modifiers.includeAll(Primary   | Pressed)
  val SECONDARY     = modifiers.includeAll(Secondary | Pressed) || (PRIMARY && CONTROL)
  val COMPLEMENT    = modifiers.includeSome(Shift)
  val SHIFT         = modifiers.includeSome(Shift)
}

case class TextInputMarked(text: String, modifiers: Bitmap, start: Int, end: Int) extends Gesture
case class TextInput(text: String, modifiers: Bitmap) extends Gesture
case class Keystroke(key: Key, modifiers: Bitmap) extends Gesture {
  override def toString: String = s"${modifiers}-$key"
}
case class MouseClick(modifiers: Bitmap) extends Gesture
case class MouseMove(modifiers: Bitmap) extends Gesture
case class MouseEnters(modifiers: Bitmap, leaving: Option[ReactiveGlyph], entering: Option[ReactiveGlyph]) extends Gesture
case class MouseLeaves(modifiers: Bitmap, leaving: Option[ReactiveGlyph], entering: Option[ReactiveGlyph]) extends Gesture
case class MouseScroll(modifiers: Bitmap) extends Gesture

/**
 * A mixin for `ReactiveGlyph`s that deals with the mapping of the various kinds of `Event` to
 * their corresponding `Gesture` and the passing on of those gestures by the to-be-defined
 * method `handle(gesture: Gesture, location: Vec, delta: Vec): Unit` to (one or more) clients.
  */
trait GestureHandler {

  @inline def handle(key: EventTextInputMarked, location: Vec, window: Window): Unit = {
    val start       = key.getReplacementStart
    val end         = key.getReplacementEnd
    //var deleteLater = 1+end-start
    //val text        = key.getText
    // TextModel.insToReplace(text, deleteLater) // pending characters to delete
    handle(TextInputMarked(key.getText, Modifiers(key), start, end), location, Vec.Origin)
  }

  @inline def handle(key: EventTextInput, location: Vec, window: Window): Unit = {
    handle(TextInput(key.getText, Modifiers(key)), location, Vec.Origin)
  }

  @inline def handle(event: EventKey, location: Vec, window: Window): Unit =
    handle(Keystroke(event.getKey, Modifiers(event)), location, Vec.Zero)

  @inline def handle(event: EventMouseMove, location: Vec, window: Window): Unit = {
    val (dx, dy) = (event.getMovementX.toFloat, event.getMovementY.toFloat)
    handle(MouseMove(Modifiers(event)), location, Vec(dx, dy))
  }

  @inline def handle(event: EventMouseButton, location: Vec, window: Window): Unit = {
    handle(MouseClick(Modifiers(event)), location, Vec.Zero)
  }

  /**
   * On a normal scrollwheel scroll is reported as a y delta
   * TODO: Investigate trackpad behaviour
   */
  @inline def handle(event: EventMouseScroll, location: Vec, window: Window): Unit = {
    handle(MouseScroll(Modifiers(event)), location, Vec(event._deltaX, event._deltaY))
  }

  @inline def handle(event: GlyphEvent, location: Vec, window: Window): Unit = {
    event match {
      case GlyphEnter(leaving, entering, modifiers) => handle(MouseEnters(modifiers, leaving, entering), location, Vec.Zero)
      case GlyphLeave(leaving, entering, modifiers) => handle(MouseLeaves(modifiers, leaving, entering), location, Vec.Zero)
    }
  }

  /**
   * Implements the reaction to the given gesture made at the given `location`, when the mouse has moved by
   * `delta` (which is `Vec.Zero` except for a `MouseMove` or a `MouseScroll`)
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
  override def accept(event: EventTextInputMarked, location: Vec, window: Window): Unit = handle(event, location, window)
  override def accept(event: EventTextInput, location: Vec, window: Window): Unit = handle(event, location, window)

  override def accept(event: EventKey, location: Vec, window: Window): Unit = handle(event, location, window)
  override def accept(event: EventMouseMove, location: Vec, window: Window): Unit = handle(event, location, window)
  override def accept(event: EventMouseButton, location: Vec, window: Window): Unit = handle(event, location, window)
  override def accept(event: GlyphEvent, location: Vec, window: Window): Unit = handle(event, location, window)
  override def accept(event: EventMouseScroll, location: Vec, window: Window): Unit = handle(event, location, window)
}
