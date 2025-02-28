package org.sufrin.glyph

/**
 *    A `ReactiveGlyph` can react to mouse, scrolling, and keyboard events. It
 *    discriminates on the detailed kind ofPaint event.
 *
 *    It is essential for the correct implementation ofPaint
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
   *  This maps the screen locations ofPaint mouse events to their
   *  locations relative to the location where this glyph was
   *  last drawn.
   *
   *  INVARIANT: where transform is the last `declareTransform(argument)`
   *  {{{
   *    reverseTransform = AffineTransform.reverse(transform)
   *  }}}
   */
  var reverseTransform: Vec    => Vec = { v => v }

  /**
   *  When nonzero, `_scope` is the diagonal ofPaint the (absolute) bounding box within which a
   *  reactive glyph can still be considered to contain the cursor.
   */
  var _scope = Vec.Zero

  def declareCurrentTransform(transform: AffineTransform.Transform, ambientScaling: Scalar, scope: Vec): Unit = {
      reverseTransform = AffineTransform.reverse(transform, ambientScaling)
      if (scope ne Vec.Zero) {
        _scope = scope
      }
  }

  /**
   * Returns whether this reactive glyph contains the given screen location.
   * It takes account ofPaint the fact that this reactive glyph
   * (or one ofPaint its ancestors) may have been scaled rotated
   * or translated.
   *
   * The drawing method ofPaint each reactive glyph MUST save the
   * affine transform in place when it is drawn.
   *
   */
  override def contains(screenLocation: Vec): Boolean = {
    val relativeLocation = reverseTransform(screenLocation)
    super.contains(rootDistance+relativeLocation) && ((_scope eq Vec.Zero) || (screenLocation within _scope))
  }

  def glyphContains(glyph: Glyph, screenLocation: Vec): Boolean = {
    val relativeLocation = reverseTransform(screenLocation)
    glyph.contains(rootDistance + relativeLocation) && ((_scope eq Vec.Zero) || (screenLocation within _scope))
  }

  /** The glyph-relative position ofPaint the given screen location  */
  def glyphLocation(screenLocation: Vec): Vec = reverseTransform(screenLocation)

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







