package org.sufrin.glyph

import scala.collection.mutable.ArrayBuffer

object NaturalSize {

  val nothing: Brush = Brush("nothing") color 0

  /**
   * Provides various generators for a row of glyphs, with `baseLine` equivalent to the largest of the `baseLine`s.
   * The `width` is the sum of the glyph widths, and the height is the maximum glyph height.
   *
   * `RowGenerators` with a specified foreground and background can be used in a `Row` expression; for example:
   * {{{
   *   Row(fg=..., bg=...).atTop(glyph1, glyph2, ...)
   *   Row(fg=..., bg=...).centered(glyph1, glyph2, ...)
   * }}}
   */
  trait RowGenerators {
    theseGenerators =>

    val fg: Brush
    val bg: Brush

    /** The glyphs are drawn so their centre lines are at the centre line of the row. */
    def centered(theGlyphs: Glyph*): Composite = aligned(0.5f, theGlyphs)

    /** The glyphs are drawn so their top is at the top of the row. */
    def atTop(theGlyphs: Glyph*): Composite = aligned(0.0f, theGlyphs)

    /** The glyphs are drawn so their bottom is at the bottom of the row. */
    def atBottom(theGlyphs: Glyph*): Composite = aligned(1.0f, theGlyphs)

    /** The glyphs are drawn so their centre lines are at the centre line of the row. */
    def centered$(theGlyphs: Seq[Glyph]): Composite = aligned(0.5f, theGlyphs)

    /** The glyphs are drawn so their top is at the top of the row. */
    def atTop$(theGlyphs: Seq[Glyph]): Composite = aligned(0.0f, theGlyphs)

    /** The glyphs are drawn so their bottom is at the bottom of the row. */
    def atBottom$(theGlyphs: Seq[Glyph]): Composite = aligned(1.0f, theGlyphs)


    /** Same as `atTop` */
    def apply(first: Glyph, theGlyphs: Glyph*): Composite = aligned(0.0f, first :: theGlyphs.toList)

    def apply(fg: Brush = nothing, bg: Brush = nothing): RowGenerators = {
      val (_fg, _bg) = (fg, bg)
      new RowGenerators {
        val fg: Brush = _fg
        val bg: Brush = _bg
      }
    }

    /**
     * A row-alignment of the glyphs, with each glyph placed vertically at
     * `proportion` of the difference between the row height and its own height.
     *
     * {{{ 0.0 <= proportion <= 1.0 && theGlyphs.nonEmpty }}}
     */
    def aligned(proportion: Float, theGlyphs: Seq[Glyph]): Composite = {
      // require(theGlyphs.nonEmpty)
      val height = Measure.maxHeight(theGlyphs)
      val width = Measure.totalWidth(theGlyphs)
      var x, y = 0f
      for {glyph <- theGlyphs} {
        val extra = glyph.vStretch(height, proportion, glyph.h)
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

        def copy(fg: Brush = fg, bg: Brush = bg): Composite = aligned(proportion, theGlyphs.map(_.copy()))
      }
    }
  }


  /**
   * Provides generators for a column of glyphs (drawn top to bottom).
   * The `width` is the maximum glyph width, and the height is the sum of the glyph heights.
   * `ColumnGenerators` with specified foreground and background can be used in a `Col` expression; for example:
   * {{{
   *   Col(fg=..., bg=...).atLeft(glyph1, glyph2, ...)
   *   Col(fg=..., bg=...).centered(glyph1, glyph2, ...)
   * }}} */
  trait ColumnGenerators {
    theseGenerators =>
    val fg: Brush
    val bg: Brush

    /** Glyphs drawn with their centres at the centre line of the column. */
    def centered(theGlyphs: Glyph*): Composite = aligned(0.5f, theGlyphs)

    /** Glyphs drawn with left edges at the left edge of the column. */
    def atLeft(theGlyphs: Glyph*): Composite = aligned(0.0f, theGlyphs)

    /** Glyphs drawn with right edges at the right edge of the column. */
    def atRight(theGlyphs: Glyph*): Composite = aligned(1.0f, theGlyphs)

    /** Glyphs drawn with their centres at the centre line of the column. */
    def centered$(theGlyphs: Seq[Glyph]): Composite = aligned(0.5f, theGlyphs)

    /** Glyphs drawn with left edges at the left edge of the column. */
    def atLeft$(theGlyphs: Seq[Glyph]): Composite = aligned(0.0f, theGlyphs)

    /** Glyphs drawn with right edges at the right edge of the column. */
    def atRight$(theGlyphs: Seq[Glyph]): Composite = aligned(1.0f, theGlyphs)


    /** Same as `atLeft`. */
    def apply(first: Glyph, theGlyphs: Glyph*): Composite = aligned(0.0f, first :: theGlyphs.toList)

    def apply(fg: Brush = nothing, bg: Brush = nothing): ColumnGenerators = {
      val (_fg, _bg) = (fg, bg)
      new ColumnGenerators {
        val fg: Brush = _fg
        val bg: Brush = _bg
      }
    }

    /*
    def apply(height: Scalar)(fg: Brush = nothing, bg: Brush = nothing): FixedSize.ColumnGenerators = {
      val (_height, _fg, _bg) = (height, fg, bg)
      new FixedSize.ColumnGenerators {
        val fg: Brush = _fg
        val bg: Brush = _bg
        val height: Scalar = _height
      }
    }*/

    /**
     * A column-alignment of the glyphs, with each glyph placed laterally at
     * `proportion` of the difference between the column width and its own width.
     *
     *
     * {{{ 0.0 <= proportion <= 1.0 && theGlyphs.nonEmpty }}}
     */

    def aligned(proportion: Float, theGlyphs: Seq[Glyph]): Composite = {
      //require(theGlyphs.nonEmpty)
      val width = Measure.maxWidth(theGlyphs)
      val height = Measure.totalHeight(theGlyphs)
      var x, y = 0f
      for {glyph <- theGlyphs} {
        val extra = glyph.hStretch(width, proportion, glyph.w)
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

        def copy(fg: Brush = fg, bg: Brush = bg): Composite = aligned(proportion, theGlyphs.map(_.copy()))
      }
    }
  }

  object Row extends RowGenerators {
    val bg: Brush = nothing
    val fg: Brush = nothing
  }

  object Col extends ColumnGenerators {
    val fg: Brush = nothing
    val bg: Brush = nothing
  }

  trait GridGenerators {
    theseGenerators =>
    import GlyphTypes.Scalar
    val fg: Brush
    val bg: Brush


    def Columns$(width: Int, padx: Scalar = 0f, pady: Scalar = 0f)(glyphs: Seq[Glyph]): Composite = {
      val maxh = pady + Measure.maxHeight(glyphs)
      val maxw = padx + Measure.maxWidth(glyphs)
      var x, y = 0f
      var i    = 0
      val it = glyphs.iterator
      val locatedGlyphs: ArrayBuffer[Glyph] = ArrayBuffer[Glyph]()
      while (it.hasNext) {
        val w = it.next().enlargedTo(maxw, maxh, fg=theseGenerators.fg, bg=theseGenerators.bg)
        val f = if (fg.color != 0) w.edged(fg) else w
        locatedGlyphs.append(f@@(x, y))
        i += 1
        if (i==width) {
          x = 0f
          y += maxh
          i = 0
        }
        else {
          x += maxw
        }
      }
      val theGlyphs = locatedGlyphs.toSeq
      new Composite(theGlyphs) {
        override val kind: String = "Grid.asRows"
        val glyphs   = theGlyphs
        val diagonal = Vec(width*maxw, y+(if (i==0) 0 else maxh))
        val fg = theseGenerators.fg
        val bg = theseGenerators.bg
        locally {
          setParents()
        }
        def copy(fg: Brush = fg, bg: Brush = bg): Composite = Columns$(width)(theGlyphs.map(_.copy()))
      }
    }

    def Columns(width: Int, padx: Scalar = 0f, pady: Scalar = 0f)(theGlyphs: Glyph*): Composite = Columns$(width, padx, pady)(theGlyphs)

    def Rows$(height: Int, padx: Scalar = 0f, pady: Scalar = 0f)(glyphs: Seq[Glyph]): Composite = {
      val maxh = pady + Measure.maxHeight(glyphs)
      val maxw = padx + Measure.maxWidth(glyphs)
      var x, y = 0f
      var i    = 0
      val it = glyphs.iterator
      val locatedGlyphs: ArrayBuffer[Glyph] = ArrayBuffer[Glyph]()
      while (it.hasNext) {
        val w = it.next().enlargedTo(maxw, maxh, fg=theseGenerators.fg, bg=theseGenerators.bg)
        val f = if (fg.color != 0) w.edged(fg) else w
        locatedGlyphs.append(f@@(x, y))
        i += 1
        if (i==height) {
          y = 0f
          x += maxw
          i = 0
        }
        else {
          y += maxh
        }
      }
      val theGlyphs = locatedGlyphs.toSeq
      new Composite(theGlyphs) {
        override val kind: String = "Grid.asCols"
        val glyphs   = theGlyphs
        val diagonal = Vec(x+(if (i==0) 0 else maxw), height*maxh)
        val fg = theseGenerators.fg
        val bg = theseGenerators.bg
        locally {
          setParents()
        }
        def copy(fg: Brush = fg, bg: Brush = bg): Composite = Rows$(height)(theGlyphs.map(_.copy()))
      }
    }

    def Rows(height: Int, padx: Scalar = 0f, pady: Scalar = 0f)(theGlyphs: Glyph*): Composite = Rows$(height, padx, pady)(theGlyphs)

  }

  object Grid extends GridGenerators {
    val fg: Brush = nothing
    val bg: Brush = nothing
    def apply(fg: Brush=fg, bg: Brush=bg):GridGenerators = {
      val (_fg, _bg) = (fg, bg)
      new GridGenerators {
        val fg: Brush = _fg
        val bg: Brush = _bg
      }
    }
  }
}
