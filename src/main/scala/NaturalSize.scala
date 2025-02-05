package org.sufrin.glyph

import scala.collection.mutable.ArrayBuffer
import GlyphTypes.Scalar


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
    val uniform: Boolean
    val frame: Brush
    val skip: Scalar
    val valign: VAlignment

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



    def apply(first: Glyph, theGlyphs: Glyph*): Composite = aligned(valign.proportion, first :: theGlyphs.toList)
    def apply(theGlyphs: Seq[Glyph]): Composite = aligned(valign.proportion, theGlyphs)

    def apply(align: VAlignment=Top, fg: Brush = nothing, bg: Brush = nothing, uniform: Boolean=false, frame: Brush = nothing, skip: Scalar=0f): RowGenerators = {
      val (_valign, _fg, _bg, _un, _fr, _sk) = (align, fg, bg, uniform, frame, skip)
      new RowGenerators {
        val valign = _valign
        val fg: Brush = _fg
        val bg: Brush = _bg
        val uniform: Boolean = _un
        val frame: Brush = _fr
        val skip = _sk
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
      val maxWidth = skip+Measure.maxWidth(theGlyphs)
      val theUniformGlyphs = if (uniform) theGlyphs.map(_.enlargedTo(maxWidth, height, bg=bg)) else theGlyphs
      var x = 0f
      var y = 0f
      for {glyph <- theUniformGlyphs} {
        val extra = glyph.vStretch(height, proportion, glyph.h)
        glyph @@ Vec(x, extra + y)
        x += glyph.w
      }
      new Composite(theUniformGlyphs) {
        override val kind: String = "Row"
        val glyphs = theUniformGlyphs
        val diagonal = Vec(x, height)
        override
        val baseLine = glyphs.map(_.baseLine).max

        lazy val locs: Seq[Scalar] =
          if (frame.getAlpha==0) Seq.empty else
            for { glyph <- glyphs.tail } yield glyph.location.x

        def drawLines(surface: Surface): Unit =
          for { loc<-locs } surface.drawLines$(frame, loc,0f, loc,height)

        override def draw(surface: Surface): Unit = {
          super.draw(surface)
          drawLines(surface)
        }

        val fg = theseGenerators.fg //if (glyphs.isEmpty) theseGenerators.fg else if (theseGenerators.fg.color == 0x00000000) glyphs.head.fg else theseGenerators.fg
        val bg = theseGenerators.bg //if (glyphs.isEmpty) theseGenerators.bg else if (theseGenerators.bg.color == 0x00000000) glyphs.head.bg else theseGenerators.bg

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
    val uniform: Boolean
    val frame: Brush
    val skip: Scalar
    val halign: Alignment

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

    def apply(first: Glyph, theGlyphs: Glyph*): Composite = aligned(halign.proportion, first :: theGlyphs.toList)
    def apply(theGlyphs: Seq[Glyph]): Composite = aligned(halign.proportion, theGlyphs)


    def apply(fg: Brush = nothing, bg: Brush = nothing, align: Alignment = Left, uniform: Boolean=false, frame: Brush=nothing, skip: Scalar=0f): ColumnGenerators = {
      val (_fg, _bg, _un, _fr, _sk) = (fg, bg, uniform, frame, skip)
      new ColumnGenerators {
        val fg: Brush = _fg
        val bg: Brush = _bg
        val uniform: Boolean = _un
        val frame: Brush = _fr
        val skip: Scalar = _sk
        val halign = align
      }
    }


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
      val maxHeight = skip+Measure.maxHeight(theGlyphs)
      val theUniformGlyphs = if (uniform) theGlyphs.map(_.enlargedTo(width, maxHeight, bg=bg)) else theGlyphs
      var x, y = 0f
      for {glyph <- theUniformGlyphs} {
        val extra = glyph.hStretch(width, proportion, glyph.w)
        glyph @@ Vec(x + extra, y)
        y += glyph.h
      }
      new Composite(theUniformGlyphs) {
        override val kind: String = "Col"

        val glyphs = theUniformGlyphs
        val diagonal = Vec(width, y)

        lazy val locs: Seq[Scalar] =
          if (frame.getAlpha==0) Seq.empty else
            for { glyph <- glyphs.tail } yield glyph.location.y

        def drawLines(surface: Surface): Unit =
          for { loc<-locs } surface.drawLines$(frame, 0f,loc, width,loc)

        override def draw(surface: Surface): Unit = {
          super.draw(surface)
          drawLines(surface)
        }

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
    val uniform: Boolean = false
    val frame: Brush = nothing
    val skip: Scalar = 0f
    val valign: VAlignment = Top
  }

  object Col extends ColumnGenerators {
    val fg: Brush = nothing
    val bg: Brush = nothing
    val uniform: Boolean = false
    val frame: Brush = nothing
    val skip: Scalar = 0f
    val halign: Alignment = Left
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
    def grid(width: Int=0, height: Int=0)(glyphs: Seq[Glyph]): Glyph = {
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
    def Grid(width: Int=0, height: Int=0)(glyphs: Glyph*): Glyph = grid(width, height)(glyphs)

      /**
       * Glyphs are arranged as a grid of constant-height rows, dimensioned to (just) fit
       * all cells on each row.
       */
    def Rows(width: Int=0)(glyphs: Seq[Glyph]): Glyph  = rows(width)(glyphs)

      /**
       * Glyphs are arranged as a grid of constant-width columns, dimensioned to (just) fit
       * all cells on each column.
       */
    def Cols(height: Int=0)(glyphs: Seq[Glyph]): Glyph = cols(height)(glyphs)


    /**
     *  Each col will be as wide as its widest element;
     *  each row will be as high as its highest element.
     */
    def Table(width: Int=0, height: Int=0)(glyphs: Glyph*): Glyph = table(width, height)(glyphs)

    /**
     *  Each col will be as wide as its widest element;
     *  each row will be as high as its highest element.
     */
    def table(width: Int=0, height: Int=0)(glyphs: Seq[Glyph]): Glyph = {
      val (rowLength, transpose)   =
          if (width>0) (width, false)
          else
          if (height>0) (height, true)
          else
            (Math.sqrt(glyphs.length).ceil.toInt, false)

      val pad: Glyph   = Glyphs.INVISIBLE()
      val (rs, cs)     = GridUtils.asRows[Glyph](pad)(rowLength, glyphs)
      val (rows, cols) = if (transpose) (cs, rs) else (rs, cs)
      val colWidths    = cols.map(col=>padx+Measure.maxWidth(col))
      val rowHeights   = rows.map(row=>pady+Measure.maxHeight(row))
      val locatedGlyphs: ArrayBuffer[Glyph] = ArrayBuffer[Glyph]()
      var y = 0f
      var dy = 0f
      var maxw = 0f
      for { row <- 0 until rows.length } {
          var x=0f
          for { col <- 0 until rows(row).length} {
              val cell   = rows(row)(col)
              val padded = cell.fitToCell(colWidths(col), rowHeights(row), fg=theseGenerators.fg, bg=theseGenerators.bg)
              val framed = if (fg.getAlpha!=0) padded.edged(fg) else padded
              locatedGlyphs.append(framed@@(x, y))
              x += framed.w//colWidths(col)
              dy = framed.h
          }
          maxw = maxw max x
          y += dy//rowHeights(row)
      }
      val theGlyphs = locatedGlyphs.toSeq
      val core = new Composite(theGlyphs) {
        override val kind: String = "TabulateRows"
        val glyphs   = theGlyphs
        val diagonal = Vec(maxw, y)
        val fg = theseGenerators.fg
        val bg = theseGenerators.bg
        locally {
          setParents()
        }
        def copy(fg: Brush = fg, bg: Brush = bg): Glyph = table(height)(theGlyphs.map(_.copy(fg, bg)))
      }
      if (fg.getAlpha!=0) core.edged(fg) else core
    }

      /** @see Rows */
    def rows(width: Int)(glyphs: Seq[Glyph]): Glyph = table(width=width, height=0)(glyphs)

      /** @see Cols */
    def cols(height: Int)(glyphs: Seq[Glyph]): Glyph = table(width=0, height=height)(glyphs)

    def uniformlyByRows(width: Int)(glyphs: Seq[Glyph]): Glyph = {
      val maxh = pady + Measure.maxHeight(glyphs)
      val maxw = padx + Measure.maxWidth(glyphs)
      val edgeX, edgeY = fg.strokeWidth*2
      var x, y = 0f
      var i    = 0
      val it = glyphs.iterator
      val locatedGlyphs: ArrayBuffer[Glyph] = ArrayBuffer[Glyph]()
      while (it.hasNext) {
        val padded = it.next().fitToCell(maxw, maxh, fg=theseGenerators.fg, bg=theseGenerators.bg)
        val edged  = if (fg.color != 0) padded.edged(fg) else padded
        locatedGlyphs.append(edged@@(x, y))
        i += 1
        if (i==width) {
          x = 0f
          y += maxh + edgeY
          i = 0
        }
        else {
          x += maxw + edgeX
        }
      }
      val theGlyphs = locatedGlyphs.toSeq
      val core: Glyph = new Composite(theGlyphs) {
        override val kind: String = "Grid.asRows"
        val glyphs   = theGlyphs
        val diagonal = Vec(width*(maxw+edgeX), y+(if (i==0) 0 else maxh+edgeY))
        val fg = theseGenerators.fg
        val bg = theseGenerators.bg
        locally {
          setParents()
        }
        def copy(fg: Brush = fg, bg: Brush = bg): Glyph = uniformlyByRows(width)(theGlyphs.map(_.copy()))
      }
      if (fg.getAlpha!=0) core.edged(fg) else core
    }



      def uniformlyByCols(height: Int)(glyphs: Seq[Glyph]): Glyph = {
      val maxh = pady + Measure.maxHeight(glyphs)
      val maxw = padx + Measure.maxWidth(glyphs)
      var x, y = 0f
      var i    = 0
      val it = glyphs.iterator
      val locatedGlyphs: ArrayBuffer[Glyph] = ArrayBuffer[Glyph]()
      while (it.hasNext) {
        val w = it.next().fitToCell(maxw, maxh, fg=theseGenerators.fg, bg=theseGenerators.bg)
        val f = if (fg.getAlpha != 0) w.edged(fg) else w
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
      val core: Glyph = new Composite(theGlyphs) {
        override val kind: String = "Grid.asCols"
        val glyphs   = theGlyphs
        val diagonal = Vec(x+(if (i==0) 0 else maxw), height*maxh)
        val fg = theseGenerators.fg
        val bg = theseGenerators.bg
        locally {
          setParents()
        }
        def copy(fg: Brush = fg, bg: Brush = bg): Glyph = uniformlyByCols(height)(theGlyphs.map(_.copy()))
      }
      if ( (fg.getAlpha != 0)) core.edged(fg) else core
    }

    def Width(width: Int)(theGlyphs: Glyph*): Glyph =
        uniformlyByRows(width)(theGlyphs)

    def Height(height: Int)(theGlyphs: Glyph*): Glyph =
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
        locally {
          println(s"Gen($fg, $bg)")
        }
      }
    }
  }
}
