package org.sufrin.glyph

/**
 *    A `ReactiveGlyph` can react to mouse, scrolling, and keyboard events. It
 *    discriminates on the detailed kind of event.
 *
 *    It is essential for the correct implementation of
 *    `contains`, THAT EVERY REACTIVE GLYPH IMPLEMENTATION invokes
 *    `surface.safeAffine(this)` if it is drawn with
 *    a transform in place.
 *
 *    @see RawButton
 *
 */
abstract class ReactiveGlyph extends Glyph {
  import io.github.humbleui.jwm._
  import GlyphTypes.Scalar

  override def isReactive: Boolean = true

  /**
   *
   *  This maps the screen locations of mouse events to their
   *  locations relative to the location where this glyph was
   *  last drawn.
   *
   *  INVARIANT: where transform is the last `declareTransform(argument)`
   *  {{{
   *    reverseTransform = AffineTransform.reverse(transform)
   *  }}}
   */
  var reverseTransform: Vec    => Vec = { v => v }


  def declareCurrentTransform(transform: AffineTransform.Transform, ambientScaling: Scalar): Unit = {
      reverseTransform = AffineTransform.reverse(transform, ambientScaling)
  }

  /**
   * Returns whether this reactive glyph contains the given screen location.
   * It takes account of the fact that this reactive glyph
   * (or one of its ancestors) may have been scaled rotated
   * or translated.
   *
   * The drawing method of each reactive glyph MUST save the
   * affine transform in place when it is drawn.
   *
   */
  override def contains(screenLocation: Vec): Boolean = {
    //import AffineTransform.{inverse, reverse, transform}
    //val inv = inverse(affineTransform)
    //val rel = transform(inv)(rootDistance)
    val relativeLocation = reverseTransform(screenLocation)
    super.contains(rootDistance+relativeLocation)
  }

  def glyphContains(glyph: Glyph, screenLocation: Vec): Boolean = {
    //import AffineTransform.{inverse, reverse, transform}
    //val inv = inverse(affineTransform)
    //val rel = transform(inv)(rootDistance)
    val relativeLocation = reverseTransform(screenLocation)
    glyph.contains(rootDistance + relativeLocation)
  }


  var _changedState: Boolean = false

  /** Ask whether the state has changed, and reset the state-changed flag */
  def stateChanged: Boolean = {
    val result = _changedState
    _changedState = false
    result
  }

  def markStateChanged: Unit = { _changedState = true }

  /** If the glyph wants to react to a mouse click */
  def accept(event: EventMouseButton, location: Vec, window: Window): Unit = {}

  /** If the glyph wants to react to a mouse movement */
  def accept(event: EventMouseMove, location: Vec, window: Window): Unit = {}

  /** If the glyph wants to react to the mousewheel */
  def accept(event: EventMouseScroll, location: Vec, window: Window): Unit = {}

  /** If the glyph wants to react to a keystroke */
  def accept(key: EventKey, location: Vec, window: Window): Unit = {}
  def accept(key: EventTextInput, location: Vec, window: Window): Unit = {}
  def accept(key: EventTextInputMarked, location: Vec, window: Window): Unit = {}
  // ... etc

  // Synthetic events delivered by the standard event handler
  def accept(event: GlyphEvent, location: Vec, window: Window): Unit = {}

  // Synthetic events delivered by the standard event handler on focusin/focusout
  def accept(event: RootGlyphEvent, location: Vec, window: Window): Unit = {}

}







