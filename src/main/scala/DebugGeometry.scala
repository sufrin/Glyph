package org.sufrin.glyph


/**
 *  A visual geometry-checking glyph that is effectively `glyph`. Intended only for debugging.
 *
 *  Iff `enable` is true it is drawn decorated, using `fg`, with a rectangular frame
 *  and (when appropriate) its `baseLine`. These glyphs are not treated as
 *  part of the semantic glyph tree. The methods where this matters are marked with [**] below.
 *  `fg` should have a very small stroke width, otherwise the frame might obliterate parts of the glyph
 *  and its surroundings.
 */
class DebugGeometry(glyph: Glyph, enable: Variable[Boolean], val fg: Brush, withBaseLine: Boolean) extends Glyph {
  val diagonal: Vec = glyph.diagonal
  val bg: Brush = Brush().color(0)

  override def toString: String = s"DebugGeometry($glyph)${if (withBaseLine) "B" else ""}$baseLineText"

  /** [**] forwarded to `glyph.parent_=` */
  override def parent_= (parent: Glyph): Unit = glyph.parent=parent

  /** [**] forwarded to `glyph.glyphContaining` */
  override def glyphContaining(p: Vec): Option[Hit] = glyph.glyphContaining(p)
  /** [**] forwarded to `glyph.glyphContaining` */
  override def reactiveContaining(p: Vec): Option[ReactiveGlyph] = glyph.reactiveContaining(p)

  override def draw(surface: Surface): Unit = {
    if (withBaseLine && glyph.baseLine > 0.0f) {
      surface.withOrigin(0, glyph.baseLine) {
        glyph.draw(surface)
      }
    } else {
      glyph.draw(surface)
    }
    if (enable.value) {
      surface.drawRect(fg, Vec.Origin, diagonal)
      surface.drawLines$(fg, 0f, glyph.baseLine, diagonal.x, glyph.baseLine)
    }
  }

  def copy(fg: Brush=fg, bg: Brush=bg): Glyph = new DebugGeometry(glyph.copy(), enable, fg, withBaseLine)
}

object DebugGeometry {
  /** Debugging frames are drawn in this colour by default. */
  val frameColor: Brush = Brush("DebugGeometry.frameColor") color 0xFF000000 strokeWidth 1.0f
  val enableFrame: Variable[Boolean] = new Variable(true)
}
