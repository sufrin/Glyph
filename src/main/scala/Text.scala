package org.sufrin.glyph

import GlyphTypes.{Font, Scalar}

/**
 * The given string in the given font. Used to derive textlayout glyphs.
 */
object Text extends DefaultPaints {
  def apply(string: String, font: Font, fg: Brush = defaultFG, bg: Brush = defaultBG) = new Text(string, font, fg, bg)
}

  trait TextGlyph extends Glyph {
    val implementation: io.github.humbleui.skija.TextLine

    /**
     * The index of the codepoint whose glyph is laterally offset by `x` from the
     * start of the displayed text.
     */
    def charIndexOf(x: Scalar): Int = implementation.getOffsetAtCoord(x)

    /**
     * The lateral offset from the start of the displayed text of the `index`th glyph.
     *
     * {{{
     *   charIndexOf(lateralOffsetOf(n)) = n (for 0<=n<string.length)
     * }}}
     */
    def lateralOffsetOf(index: Int): Scalar = implementation.getCoordAtOffset(index)

    def glyphs: List[Short] = implementation.getGlyphs.toList
  }

  class Text(val string: String, val font: Font, val fg: Brush, val bg: Brush) extends TextGlyph {
    theText: Text =>
    import io.github.humbleui.skija.TextLine
    val implementation: TextLine = io.github.humbleui.skija.TextLine.make(string, font)
    val width     = implementation.getWidth
    val ascent    = implementation.getAscent
    val height    = implementation.getHeight
    val descent   = implementation.getDescent
    val leading   = implementation.getLeading
    val spacing   = descent + ascent + leading
    val diagonal  = Vec(width, height + descent)
    val drop      = height - spacing
    def copy(fg: Brush=fg, bg: Brush=bg): Glyph = if ((fg eq this.fg) && (bg eq this.bg)) this else new Text(string, font, fg, bg)
    def draw(surface: Surface): Unit = {
      drawBackground(surface)
      surface.drawTextLine(fg, implementation, 0, height)
      surface.drawLines$(fg, 0f,height, width,height)
    }

    override def baseLine: Scalar = height



    /**
     * A glyph with the diagonal of this text that
     * will be drawn with origin adjusted by the baseline of the text.
     */
    def atBaseline(fg: Brush = this.fg, bg: Brush = this.bg): Glyph = this.copy(fg, bg)
    /**
     * A self-contained glyph whose diagonal is the diagonal of
     * this text.
     */
    def asGlyph(fg: Brush = this.fg, bg: Brush = this.bg): Glyph = this.copy(fg, bg)

    def asLabel(fg: Brush = this.fg, bg: Brush = this.fg): Glyph = this.copy(fg, bg)
  }