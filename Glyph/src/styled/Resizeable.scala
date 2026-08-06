package org.sufrin
package glyph
package styled


/**
 *   A glyph, whose appearance is denoted by the Glyph
 *   delivered by `glyph`. It rebuilds its appearance at a given size on request.
 *   Its initial appearance is determined by the context `initialStyle`; and if the context hasn't
 *   set a positive `boundingBox`, this will be the "natural" (bottom-up)
 *   appearance determined by the rest of the context. Subsequent invocations of
 *   `atSize(box: Vec)` regenerate the appearance using `currentStyle.copy(boundingBox=box)`
 *
 *   It is intended for use as the top-level `Glyph` of a GUI whose
 *   window may be resized, and whose layout may need to be adapted to
 *   the current size.
 *
 */



class Resizeable(glyph: StyleSheet=>Glyph, initialStyle: StyleSheet) extends Glyph with GlyphTransforms {
  override def resizeable: Boolean = true

  override def toString: String = s"Resizeable($glyph)[${currentStyle.containerDiagonal} ${currentStyle.windowDiagonal}"

  var currentStyle: StyleSheet = initialStyle

  def forceResize(): Unit = {
    if (hasGuiRoot) guiRoot.setContentSize(currentStyle.containerDiagonal)
  }

  var delegate: Glyph = _

  locally {
    val initial = glyph(currentStyle)
    initial.parent = this
    delegate = initial
  }

  override def atSize(boundingBox: Vec): Glyph = {
    val screenDiagonal: Vec = if (hasGuiRoot) {
      val wa = guiRoot.currentScreen.getWorkArea
      Vec(wa.getWidth.toFloat, wa.getHeight.toFloat)
    } else Vec.Zero
    val windowDiagonal: Vec = if (hasGuiRoot) { guiRoot.diagonal } else Vec.Zero
    Resizeable.fine(s"atSize$boundingBox with windowDiagonal$windowDiagonal; screenDiagonal$screenDiagonal")
    currentStyle = currentStyle.copy(containerDiagonal = boundingBox, windowDiagonal = windowDiagonal, screenDiagonal = screenDiagonal)
    delegate = glyph(currentStyle)
    delegate.parent = this
    delegate
  }

  def draw(surface: Surface): Unit = delegate.draw(surface)
  def diagonal: Vec = delegate.diagonal
  def copy(fg: Brush=delegate.fg, bg: Brush=delegate.bg): Glyph = delegate.copy(fg, bg)
  override val fg: Brush = delegate.fg
  override val bg: Brush = delegate.bg
  override def glyphContaining(p: Vec): Option[Hit] = delegate.glyphContaining(p)
  override def reactiveContaining(p: Vec): Option[ReactiveGlyph] = delegate.reactiveContaining(p)

}

/**
 *  A glyph, whose initial appearance is denoted by the Glyph
 *  delivered by `generate(style)`.
 *  It rebuilds its appearance at a given size on request.
*/
object Resizeable extends logging.Loggable {
  def apply(generate: StyleSheet=>Glyph)(implicit style: StyleSheet): Resizeable = new Resizeable(generate, style)
}
