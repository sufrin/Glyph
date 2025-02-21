package org.sufrin.glyph
import GlyphTypes.Scalar


/**
 *  A visual geometry-checking glyph that is effectively `glyph`. Intended only for debugging.
 *
 *  Iff `enable` is true glyph is drawn decorated with a rectangular frame
 *  and (when appropriate) its `baseLine` using the `fg` brush. The original glyph is treated as
 *  part of the semantic glyph tree; but the the framing glyph is not. The methods where this matters are marked with [**] below.
 *  `fg` should have a very small stroke width, otherwise the frame might obliterate parts of the glyph
 *  and its surroundings.
 */
class ShowingBaseline(glyph: Glyph, enable: Variable[Boolean], val fg: Brush) extends Glyph {
  val diagonal: Vec = glyph.diagonal
  val bg: Brush = Brush().color(0)

  override def toString: String = s"DebugGeometry($glyph)$baseLineText"

  override def baseLine: Scalar = glyph.baseLine

  /** [**] forwarded to `glyph.parent_=` */
  override def parent_= (parent: Glyph): Unit = glyph.parent=parent

  /** [**] forwarded to `glyph.glyphContaining` */
  override def glyphContaining(p: Vec): Option[Hit] = glyph.glyphContaining(p)
  /** [**] forwarded to `glyph.glyphContaining` */
  override def reactiveContaining(p: Vec): Option[ReactiveGlyph] = glyph.reactiveContaining(p)

  override def draw(surface: Surface): Unit = {
    glyph.draw(surface)
    if (enable.value) {
      surface.drawRect(fg, Vec.Origin, diagonal)
      surface.drawLines$(fg, 0f, glyph.baseLine, diagonal.x, glyph.baseLine)
    }
  }

  def copy(fg: Brush=fg, bg: Brush=bg): Glyph = new ShowingBaseline(glyph.copy(), enable, fg)
}

object ShowingBaseline {
  /** Default colour */
  var fg: Brush = Brush("Showingbaseline.fg") color 0xFF000000 strokeWidth 1.0f
  /** Boolean Variable controlling visibility of ShowingBaseline frames */
  val enabled: Variable[Boolean] = new Variable(true)
  def apply(fg: Brush=fg, g: Glyph): Glyph = new ShowingBaseline(g, enabled, fg)
}
