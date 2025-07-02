package org.sufrin.glyph
package unstyled

import GlyphTypes.{Font, Scalar}

import scala.collection.mutable


object Text  {
  /**
   * A glyph representing the given string in the given font.
   * @param string
   * @param font
   * @param fg
   * @param bg
   * @param transient true if there is no point in sharing this `(string,font)`'s IMPLEMENTATION with those of
   *                  others. The space saving afforded by sharing can be considerable when using `glyphXML` constructs or
   *                  `SimpleParagraphs` constructs, because each "word" is represented by an individual `Text`, whose
   *                  implementation is a `skija.TextLine`.
   * @return
   */
  def apply(string: String, font: Font=fallback.textFont, fg: Brush = fallback.textForeground, bg: Brush = fallback.textBackground, transient: Boolean=false) =
      new Text(string, font, fg, bg, transient)

  val fontCache = new mutable.HashMap[(String,Font), io.github.humbleui.skija.TextLine](1024, 2.0)

}



class Text(val string: String, val font: Font, override val fg: Brush, override val bg: Brush, transient: Boolean) extends TextInterface {
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
    @deprecated("Obsolete") def atBaseline(fg: Brush = this.fg, bg: Brush = this.bg): Glyph = this.copy(fg, bg)
    /**
     * OBSOLETE: same as this.copy(fg, bg)
     */
    @deprecated("Obsolete") def asGlyph(fg: Brush = this.fg, bg: Brush = this.bg): Glyph = this.copy(fg, bg)
    /**
     * OBSOLETE: same as this.copy(fg, bg)
     */
    @deprecated("Obsolete") def asLabel(fg: Brush = this.fg, bg: Brush = this.fg): Glyph = this.copy(fg, bg)

  lazy val positions = implementation.getPositions()

  /*
  def codePointIndexToCharIndex(codePointIndex: Int): Int = string.offsetByCodePoints(0, codePointIndex)
  def codePointIndexToPosition(codePointIndex: Int): Scalar = {
      val offset = string.offsetByCodePoints(0, codePointIndex)
      println(s"$offset ${positions.toList.mkString("(",",", ")")}")
      positions(2*offset)
  }
  */

}