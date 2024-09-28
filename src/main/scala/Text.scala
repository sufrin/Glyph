package org.sufrin.glyph

import GlyphTypes.{Font}

/**
 * The given string in the given font. Used to derive textlayout glyphs.
 */
object Text extends DefaultPaints {
  def apply(string: String, font: Font, fg: Brush = defaultFG, bg: Brush = defaultBG) = new Text(string, font, fg, bg)
}

  class Text(val string: String, val font: Font, fg: Brush, bg: Brush) {
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

    def glyphs: List[Short] = implementation.getGlyphs.toList


    /**
     * A textlayout glyph with the diagonal of this textlayout, that
     * will be drawn with origin adjusted by the baseline of the textlayout.
     */
    def atBaseline(fg: Brush = fg, bg: Brush = bg): Glyph = {
      val _fg = fg
      val _bg = bg

      new Glyph {
      override val kind="Text.atBaseline"
      override def toString: String = s"Text(${string}).atBaseline(${this.fg}, ${this.bg}))"
      val diagonal = theText.diagonal
      override val baseLine = height
      val fg = _fg
      val bg = _bg

      def draw(surface: Surface): Unit = {
        drawBackground(surface)
        surface.drawTextLine(fg, implementation, 0, 0)
      }

      def copy(fg: Brush=fg, bg: Brush=bg): Glyph = atBaseline(fg, bg)
    }
   }

    /**
     * A self-contained glyph whose diagonal is the diagonal of
     * this textlayout.
     */
    def asGlyph(fg: Brush = Text.defaultFG, bg: Brush = Text.defaultBG): Glyph = {
      val _fg = fg
      val _bg = bg
      new Glyph {
        override val kind="Text.asGlyph"
        override def toString: String = s"""Text("${string}").asGlyph(${fg}, $bg))"""
        val diagonal = theText.diagonal
        override val baseLine = 0f
        val fg = _fg
        val bg = _bg

        def draw(surface: Surface): Unit = {
          drawBackground(surface)
          surface.drawTextLine(paint, implementation, 0, height)
        }

        def copy(fg: Brush=fg, bg: Brush=bg): Glyph = asGlyph(fg, bg)
      }
    }

    def asLabel(fg: Brush = Text.defaultFG, bg: Brush = Text.defaultBG): Glyph = {
      val _fg = fg
      val _bg = bg
      new Glyph {
        override val kind="Text.asLabel"
        override def toString: String = s"Label(${string}, fg=$fg, bg=$bg)"
        val diagonal = theText.diagonal
        override val baseLine = 0f
        val fg = _fg
        val bg = _bg

        def draw(surface: Surface): Unit = {
          drawBackground(surface)
          surface.drawTextLine(paint, implementation, 0, height)
        }

        def copy(fg: Brush=fg, bg: Brush=bg): Glyph = asLabel(fg, bg)
      }
    }
  }