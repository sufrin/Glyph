package org.sufrin.glyph

import GlyphTypes.{Scalar}

/**
 * `FixedSize` glyphs are Rows / Columns whose widths/heights are preset, and that contain
 * `Space` elements that are cellFit to fill them.
 *
 * When a row (col) is to be set in a given width (height), its natural size is
 * first established by summing the widths (heights) of its glyphs. If this is
 * smaller than the given width (height), then the excess is distributed among its
 * stretchable `Space` glyphs, in proportion to their stretchability in the appropriate
 * dimension. The aggregate width (height) will now be no less than the given width (height),
 * providing that there is at least one `Space` among the glyphs. If there are none then the
 * aggregate width (height) is taken to be the natural width (height).
 *
 */
object FixedSize extends DefaultPaints {

  /** A space whose dimension(s) can be changed before layout */
  class Space(var _w: Scalar, var _h: Scalar, xS: Scalar, yS: Scalar) extends Glyph {
    override val xStretch: Scalar = xS
    override val yStretch: Scalar = yS

    def draw(surface: Surface): Unit = {
      drawBackground(surface)
      if (fg.color!=0) {
        println(s"$fg $diagonal (${fg.filter})")
        surface.fillRect(fg, diagonal)
      }
    }

    def w_=(x: Scalar): Unit = _w = x

    def h_=(x: Scalar): Unit = _h = x

    def diagonal: Vec = Vec(_w, _h)

    val fg: Brush = nothing
    val bg: Brush = nothing

    /** A copy of this glyph; perhaps with different foreground/background */
    def copy(fg: Brush, bg: Brush): Glyph = {
      val ffg = fg
      val bbg = bg
      new Space(w, h, xS, yS) {
        override val fg: Brush = ffg
        override val bg: Brush = bbg
      }
    }

    val thisGlyph: Glyph = this
  }

  object Space {
    def apply(wh: Scalar, stretch: Scalar): Space = new Space(wh, wh, stretch, stretch)

    def apply(w: Scalar, h: Scalar, stretch: Scalar): Space = new Space(w, h, stretch, stretch)

    def tab: Space = new Space(10f, 10f, 1.0f, 1.0f)
  }

  /** Inflate stretchable spaces so that the total width is `width` */
  def HInflate(width: Scalar, glyphs: Seq[Glyph]): Unit = {
    val totalw = glyphs.map(_.w).sum
    val extraw = width - totalw
    val totalStretch = glyphs.map(_.xStretch).sum
    if (totalStretch>0 && extraw>0) {
      val extraPerStretch = extraw / totalStretch
        for {glyph <- glyphs}
          glyph match {
            case sg: Space =>
              sg.w += extraPerStretch * glyph.xStretch
            case _ =>
          }
    }
  }

  /** Inflate stretchable spaces so that the total height is `height` */
  def VInflate(height: Scalar, glyphs: Seq[Glyph]): Unit = {
    val totalh = glyphs.map(_.h).sum
    val extrah = height - totalh
    val totalStretch = glyphs.map(_.yStretch).sum
    if (totalStretch > 0 && extrah>0) {
      val extraPerStretch = extrah / totalStretch
      for {glyph <- glyphs}
        glyph match {
          case sg: Space =>
            sg.h += extraPerStretch * glyph.yStretch
          case _ =>
        }
    }
  }

  trait RowGenerators { theseGenerators =>
    val fg: Brush
    val bg: Brush
    val width: Scalar
    /** The glyphs are drawn so their centre lines are at the centre line of the row. */
    def centered(theGlyphs: Glyph*): Composite = aligned(width, 0.5f, theGlyphs)

    /** The glyphs are drawn so their top is at the top of the row. */
    def atTop(theGlyphs: Glyph*): Composite = aligned(width, 0.0f, theGlyphs)

    /** The glyphs are drawn so their bottom is at the bottom of the row. */
    def atBottom(theGlyphs: Glyph*): Composite = aligned(width, 1.0f, theGlyphs)

    def centered$(theGlyphs: Seq[Glyph]): Composite = aligned(width, 0.5f, theGlyphs)

    /** The glyphs are drawn so their top is at the top of the row. */
    def atTop$(theGlyphs: Seq[Glyph]): Composite = aligned(width, 0.0f, theGlyphs)

    /** The glyphs are drawn so their bottom is at the bottom of the row. */
    def atBottom$(theGlyphs: Seq[Glyph]): Composite = aligned(width, 1.0f, theGlyphs)

    def apply(theGlyphs: Glyph*): Composite = aligned(width, 0.0f, theGlyphs)

    /** Construct `RowGenerator`s for a row of the given width, with the given foreground and background. */
    def apply(width: Scalar, fg: Brush=nothing, bg: Brush = nothing): RowGenerators = {
      val (_width, _fg, _bg) = (width, fg, bg)
      new RowGenerators {
        val fg: Brush = _fg
        val bg: Brush = _bg
        val width: Scalar = _width
      }
    }

    def aligned(theWidth: Scalar, proportion: Float, theGlyphs: Seq[Glyph]): Composite = {
      require(theGlyphs.nonEmpty)
      require(theWidth>0f)
      HInflate(theWidth, theGlyphs)
      val height = theGlyphs.map(_.h).max
      val width = theWidth // theGlyphs.map(_.w).sum
      var x, y = 0f
      for {glyph <- theGlyphs} {
        val extra = (height - glyph.h) * proportion
        glyph @@ Vec(x, extra + y)
        x += glyph.w
      }
      new Composite(theGlyphs) {
        override val kind: String = "Row"
        val glyphs = theGlyphs
        val diagonal = Vec(width, height)
        override
        val baseLine = glyphs.map(_.baseLine).max

        val fg = theseGenerators.fg //if (glyphs.isEmpty) theseGenerators.fg else if (theseGenerators.fg.color == 0x00000000) glyphs.head.fg else theseGenerators.fg
        val bg = theseGenerators.bg //if (glyphs.isEmpty) theseGenerators.bg else if (theseGenerators.bg.color == 0x00000000) glyphs.head.bg else theseGenerators.bg

        /** A geometry-debugging glyph surrounding this glyph */
        override
        def $$$$(enable: Variable[Boolean] = DebugGeometry.enableFrame, framePaint: Brush = DebugGeometry.frameColor): Glyph =
          new DebugGeometry(this, enable, fg = framePaint, false)

        locally {
          setParents()
        }

        def copy(fg: Brush = this.fg, bg: Brush = this.bg): Composite = aligned(theWidth, proportion, theGlyphs.map(_.copy()))
      }
    }
  }

  trait ColumnGenerators { theseGenerators =>
    val fg: Brush
    val bg: Brush
    val height: Scalar

     /** Glyphs drawn with their centres at the centre line of the column. */
    def centered(theGlyphs: Glyph*): Composite = aligned(height, 0.5f, theGlyphs)

    /** Glyphs drawn with left edges at the left edge of the column. */
    def atLeft(theGlyphs: Glyph*): Composite = aligned(height, 0.0f, theGlyphs)

    /** Glyphs drawn with right edges at the right edge of the column. */
    def atRight(theGlyphs: Glyph*): Composite = aligned(height, 1.0f, theGlyphs)

    /** Glyphs drawn with their centres at the centre line of the column. */
    def centered$(theGlyphs: Seq[Glyph]): Composite = aligned(height, 0.5f, theGlyphs)

    /** Glyphs drawn with left edges at the left edge of the column. */
    def atLeft$(theGlyphs: Seq[Glyph]): Composite = aligned(height, 0.0f, theGlyphs)

    /** Glyphs drawn with right edges at the right edge of the column. */
    def atRight$(theGlyphs: Seq[Glyph]): Composite = aligned(height, 1.0f, theGlyphs)

    /** Same as `atLeft`. */
    def apply(theGlyphs: Glyph*): Composite = aligned(height, 0.0f, theGlyphs)

    /** Construct `ColumnGenerator`s for a column of the given height, with the given foreground and background. */
    def apply(height: Scalar, fg: Brush = nothing, bg: Brush = nothing): ColumnGenerators = {
      val (_height, _fg, _bg) = (height, fg, bg)
      new ColumnGenerators {
        val fg: Brush = _fg
        val bg: Brush = _bg
        val height: Scalar = _height
      }
    }


    /**
     * A column-alignment of the glyphs, with each glyph placed laterally at
     * `proportion` of the difference between the column width and its own width.
     *
     *
     * {{{ 0.0 <= proportion <= 1.0 && theGlyphs.nonEmpty }}}
     */

    def aligned(theHeight: Scalar, proportion: Float, theGlyphs: Seq[Glyph]): Composite = {
      require(theGlyphs.nonEmpty)
      require(theHeight>0f)
      VInflate(theHeight, theGlyphs)
      val width = theGlyphs.map(_.w).max
      val height = theHeight // theGlyphs.map(_.h).sum
      var x, y = 0f
      for {glyph <- theGlyphs} {
        val extra = (width - glyph.w) * proportion
        glyph @@ Vec(x + extra, y)
        y += glyph.h
      }
      new Composite(theGlyphs) {
        override val kind: String = "Col"
        val glyphs = theGlyphs
        val diagonal = Vec(width, height)

        val fg = theseGenerators.fg //if (glyphs.isEmpty) theseGenerators.fg else if (theseGenerators.fg.color == 0x00000000) glyphs.head.fg else theseGenerators.fg
        val bg = theseGenerators.bg //if (glyphs.isEmpty) theseGenerators.bg else if (theseGenerators.bg.color == 0x00000000) glyphs.head.bg else theseGenerators.bg

        locally {
          setParents()
        }

        def copy(fg: Brush = fg, bg: Brush = bg): Composite = aligned(theHeight, proportion, theGlyphs.map(_.copy()))
      }
    }
  }

  val nothing = Brush() color 0

  object Row extends RowGenerators {
    val bg: Brush = nothing
    val fg: Brush = nothing
    val width: Scalar = 0f
  }

  object Col extends ColumnGenerators {
    val fg: Brush = nothing
    val bg: Brush = nothing
    val height:Scalar = 0f
  }


}