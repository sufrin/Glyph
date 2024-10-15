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

  import GlyphTypes.Scalar

    /*
     *  All `GridGenerators` methods with `width`, and/or `height`, and 'glyphs` formal parameters interpret
     *  glyphs as follows:
     *  if there's a `width` parameter then glyphs is interpreted as a catenation of rows of the given `width`;
     *  if there's a `height` parameter then glyphs is interpreted as a catenation of columns of the given `height`.
     *  if there are both `width` and `height` parameters, then if one argument is omitted,
     *  `glyphs` is interpreted as if the method had only one formal paramater; if
     *  both arguments are omitted then `width` is taken to be `ceiling(sqrt(glyphs.length))`.
     *
     */
  trait GridGenerators {
    theseGenerators =>
    val fg:   Brush
    val bg:   Brush
    val padx: Scalar
    val pady: Scalar


      /**
       * Glyphs are arranged as a grid of uniformly-sized cells, dimensioned to (just) fit
       * them all.
       */
    def grid(width: Int=0, height: Int=0)(glyphs: Seq[Glyph]): Composite = {
      (width>0, height>0) match {
        case (true, false) => uniformlyByRows(width)(glyphs)
        case (false, true) => uniformlyByCols(height)(glyphs)
        case (_, _) =>
          val width: Int = Math.sqrt(glyphs.length).ceil.toInt
          uniformlyByRows(width)(glyphs)
      }
    }

      /**
       * Glyphs are arranged as a grid of uniformly-sized cells, dimensioned to (just) fit
       * them all.
       */
    def Grid(width: Int=0, height: Int=0)(glyphs: Glyph*): Composite = grid(width, height)(glyphs)

      /**
       * Glyphs are arranged as a grid of constant-height rows, dimensioned to (just) fit
       * all cells on each row.
       */
    def Rows(width: Int=0)(glyphs: Seq[Glyph]): Composite  = rows(width)(glyphs)

      /**
       * Glyphs are arranged as a grid of constant-width columns, dimensioned to (just) fit
       * all cells on each column.
       */
    def Cols(height: Int=0)(glyphs: Seq[Glyph]): Composite = cols(height)(glyphs)


    /**
     *  Each col will be as wide as its widest element;
     *  each row will be as high as its highest element.
     */
    def Table(width: Int=0, height: Int=0)(glyphs: Glyph*): Composite = table(width, height)(glyphs)

    /**
     *  Each col will be as wide as its widest element;
     *  each row will be as high as its highest element.
     */
    def table(width: Int=0, height: Int=0)(glyphs: Seq[Glyph]): Composite = {
      val (rowLength, transpose)   =
          if (width>0) (width, false)
          else
          if (height>0) (height, true)
          else
            (Math.sqrt(glyphs.length).ceil.toInt, false)

      val pad: Glyph   = FixedSize.Space(0f, 0f, 0f)
      val (rs, cs)     = GridUtils.asRows[Glyph](pad)(rowLength, glyphs)
      val (rows, cols) = if (transpose) (cs, rs) else (rs, cs)
      val colWidths    = cols.map(col=>padx+Measure.maxWidth(col))
      val rowHeights   = rows.map(row=>pady+Measure.maxHeight(row))
      val locatedGlyphs: ArrayBuffer[Glyph] = ArrayBuffer[Glyph]()
      var y = 0f
      var maxw = 0f
      for { row <- 0 until rows.length } {
          var x=0f
          for { col <- 0 until rows(row).length} {
              val cell   = rows(row)(col)
              val padded = cell.fitToCell(colWidths(col), rowHeights(row), fg=theseGenerators.fg, bg=theseGenerators.bg)
              val framed = if (fg.color != 0 && (cell ne pad)) padded.edged(fg) else padded
              locatedGlyphs.append(framed@@(x, y))
              x += colWidths(col)
          }
          maxw = maxw max x
          y += rowHeights(row)
      }
      val theGlyphs = locatedGlyphs.toSeq
      new Composite(theGlyphs) {
        override val kind: String = "TabulateRows"
        val glyphs   = theGlyphs
        val diagonal = Vec(maxw, y)
        val fg = theseGenerators.fg
        val bg = theseGenerators.bg
        locally {
          setParents()
        }
        def copy(fg: Brush = fg, bg: Brush = bg): Composite = table(height)(theGlyphs.map(_.copy(fg, bg)))
      }
    }

      /** @see Rows */
    def rows(width: Int)(glyphs: Seq[Glyph]): Composite = table(width=width, height=0)(glyphs)

      /** @see Cols */
    def cols(height: Int)(glyphs: Seq[Glyph]): Composite = table(width=0, height=height)(glyphs)

    def uniformlyByRows(width: Int)(glyphs: Seq[Glyph]): Composite = {
      val maxh = pady + Measure.maxHeight(glyphs)
      val maxw = padx + Measure.maxWidth(glyphs)
      var x, y = 0f
      var i    = 0
      val it = glyphs.iterator
      val locatedGlyphs: ArrayBuffer[Glyph] = ArrayBuffer[Glyph]()
      while (it.hasNext) {
        val w = it.next().fitToCell(maxw, maxh, fg=theseGenerators.fg, bg=theseGenerators.bg)
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
        def copy(fg: Brush = fg, bg: Brush = bg): Composite = uniformlyByRows(width)(theGlyphs.map(_.copy()))
      }
    }



      def uniformlyByCols(height: Int)(glyphs: Seq[Glyph]): Composite = {
      val maxh = pady + Measure.maxHeight(glyphs)
      val maxw = padx + Measure.maxWidth(glyphs)
      var x, y = 0f
      var i    = 0
      val it = glyphs.iterator
      val locatedGlyphs: ArrayBuffer[Glyph] = ArrayBuffer[Glyph]()
      while (it.hasNext) {
        val w = it.next().fitToCell(maxw, maxh, fg=theseGenerators.fg, bg=theseGenerators.bg)
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
        def copy(fg: Brush = fg, bg: Brush = bg): Composite = uniformlyByCols(height)(theGlyphs.map(_.copy()))
      }
    }

    def Width(width: Int)(theGlyphs: Glyph*): Composite =
        uniformlyByRows(width)(theGlyphs)

    def Height(height: Int)(theGlyphs: Glyph*): Composite =
        uniformlyByCols(height)(theGlyphs)

  }

  object Grid extends GridGenerators {
    val fg: Brush = nothing
    val bg: Brush = nothing
    val padx: Scalar = 0
    val pady: Scalar = 0

    def apply(fg: Brush=fg, bg: Brush=bg, padx: Scalar = 0, pady: Scalar = 0): GridGenerators = {
      val (_fg, _bg, _padx, _pady) = (fg, bg, padx, pady)
      new GridGenerators {
        val fg: Brush = _fg
        val bg: Brush = _bg
        val padx: Scalar = _padx
        val pady: Scalar = _pady
      }
    }
  }
}
