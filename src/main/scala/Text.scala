package org.sufrin.glyph

import GlyphTypes.{Font, Scalar}

import scala.collection.mutable


object Text extends DefaultPaints {
  /**
   * A glyph representing the given string in the given font.
   * @param string
   * @param font
   * @param fg
   * @param bg
   * @param transient true if there is no point in sharing this `(string,font)`'s IMPLEMENTATION with those of
   *                  others. The space saving afforded by sharing can be considerable.
   * @return
   */
  def apply(string: String, font: Font, fg: Brush = defaultFG, bg: Brush = defaultBG, transient: Boolean=false) = new Text(string, font, fg, bg, transient)

  val fontCache = new mutable.HashMap[(String,Font), io.github.humbleui.skija.TextLine](1024, 2.0)

}

  trait TextGlyph extends Glyph {

    val implementation: io.github.humbleui.skija.TextLine

    /**
     * The index of the character (Unicode codepoint) whose visual representation is laterally offset by `distance` from the
     * start of the displayed text.
     */
    def charIndexOf(distance: Scalar): Int = implementation.getOffsetAtCoord(distance)

    /**
     * The lateral offset from the start of the displayed text of the visual representation of the `index`th character
     * of the string.
     *
     * {{{
     *   charIndexOf(lateralOffsetOf(n)) = n (for 0<=n<string.length)
     * }}}
     */
    def lateralOffsetOf(index: Int): Scalar = implementation.getCoordAtOffset(index)

    /**
     * Font-specific identifiers of the individual characters of the text. "These IDs
     * help in rendering text efficiently".
     */
    def glyphIDs: Seq[Short] = implementation.getGlyphs.toSeq
  }

  class Text(val string: String, val font: Font, val fg: Brush, val bg: Brush, transient: Boolean) extends TextGlyph {
    theText: Text =>
    import io.github.humbleui.skija.TextLine

    val implementation: TextLine =
        if (transient)
          io.github.humbleui.skija.TextLine.make(string, font)
        else
          Text.fontCache.getOrElseUpdate((string, font), io.github.humbleui.skija.TextLine.make(string, font))

    val width     = implementation.getWidth
    val ascent    = implementation.getAscent
    val height    = implementation.getHeight
    val descent   = implementation.getDescent
    val leading   = implementation.getLeading
    val spacing   = descent + ascent + leading
    val diagonal  = Vec(width, height + descent)
    val drop      = height - spacing

    def copy(fg: Brush=fg, bg: Brush=bg): Glyph = new Text(string, font, fg, bg, transient)

    def draw(surface: Surface): Unit = {
      drawBackground(surface)
      surface.drawTextLine(fg, implementation, 0, height)
    }

    /**
     * Distance from the top of the glyph to the baseline of the font in which it will be drawn.
     * To construct a horizontal glyph of glyphs ... aligned at their baselines:
     * {{{
     * Row(align=Baseline)(...)
     * }}}
     */
    override def baseLine: Scalar = height

    /**
     * OBSOLETE: same as this.copy(fg, bg)
     */
    def atBaseline(fg: Brush = this.fg, bg: Brush = this.bg): Glyph = this.copy(fg, bg)
    /**
     * OBSOLETE: same as this.copy(fg, bg)
     */
    def asGlyph(fg: Brush = this.fg, bg: Brush = this.bg): Glyph = this.copy(fg, bg)
    /**
     * OBSOLETE: same as this.copy(fg, bg)
     */
    def asLabel(fg: Brush = this.fg, bg: Brush = this.fg): Glyph = this.copy(fg, bg)
  }