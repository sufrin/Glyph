package org.sufrin.glyph

import scala.collection.mutable.ArrayBuffer
import GlyphTypes.Scalar

/* The Row and Col APIs are designed so that most of the characteristics of the Row(Col) can
 * be pre-set, thereby making it possible to apply them either to an unbounded number of
 * actual glyphs, or to a sequence of glyphs computed elsewhere.
 */

object NaturalSize {

  val nothing: Brush = Brush("nothing") color 0

  trait RowGenerators {
    theseGenerators =>

    val fg: Brush
    val bg: Brush
    val uniform: Boolean
    val frame: Brush
    val skip: Scalar
    val valign: VAlignment

    def apply(first: Glyph, theGlyphs: Glyph*): Composite =
      aligned(valign.proportion, first :: theGlyphs.toList, valign.atBaseline)

    def apply(theGlyphs: Seq[Glyph]): Composite =
      aligned(valign.proportion, theGlyphs, valign.atBaseline)

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
    def aligned(proportion: Float, theGlyphs: Seq[Glyph], atBaseline: Boolean = false): Composite = {
      // require(theGlyphs.nonEmpty)
      val height = Measure.maxHeight(theGlyphs)
      val width = Measure.totalWidth(theGlyphs)
      val maxWidth = skip+Measure.maxWidth(theGlyphs)
      val theUniformGlyphs =
        if (uniform) theGlyphs.map(_.enlargedTo(maxWidth, height, bg=bg)) else theGlyphs
      var x = 0f
      var y = 0f
      val base = if (atBaseline) theGlyphs.map(_.baseLine).max else 0f

      for {glyph <- theUniformGlyphs} {
        val extra =
          if (atBaseline && glyph.baseLine>0)
            base-glyph.baseLine//((height-glyph.h)+(glyph.h-glyph.baseLine))-(height-base)
          else
            glyph.vStretch(height, proportion, glyph.h)
        glyph @@ Vec(x, extra + y)
        x += glyph.w
      }
      new Composite(theUniformGlyphs) {
        override val kind: String = "Row"
        val glyphs = theUniformGlyphs
        val diagonal = Vec(x, height)
        override
        val baseLine = base

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



  trait ColumnGenerators {
    theseGenerators =>
    val fg: Brush
    val bg: Brush
    val uniform: Boolean
    val frame: Brush
    val skip: Scalar
    val halign: Alignment

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

  /**
   * {{{ Row(align: VAlignment=Top, fg: Brush = nothing, bg: Brush = nothing, uniform: Boolean=false, frame: Brush = nothing, skip: Scalar=0f)(glyphs) }}}
   *
   * Constructs the horizontal catenation of `glyphs`; its height is the largest of the glyphs' heights; its width is
   * normally the sum of the glyphs' widths.
   *
   * The glyphs are vertically aligned as follows in the row, as specified by `align`:
   * {{{
   *   Top      tops of the bounding boxes aligned
   *   Bottom   bottoms of the bounding boxes aligned
   *   Mid      (vertical) centers of the bounding boxes aligned
   *   Baseline baselines aligned when>0; otherwise as Bottom
   * }}}
   *
   * If `uniform`, then the glyphs are all treated as if their height(width)
   * was the same as the maximal height(width) of all the glyphs; and each glyph is treated as if its width were `glyph.w+skip`,
   * thereby leaving a little horizontal space between them in the row.
   *
   * If `frame` is a nontrivial brush, then vertical lines with foreground `frame` are
   * drawn between glyphs.
   *
   */
  object Row extends RowGenerators {
    val bg: Brush = nothing
    val fg: Brush = nothing
    val uniform: Boolean = false
    val frame: Brush = nothing
    val skip: Scalar = 0f
    val valign: VAlignment = Top
  }

  /**
   *
   * {{{ Col(align: Alignment=Left, fg: Brush = nothing, bg: Brush = nothing, uniform: Boolean=false, frame: Brush = nothing, skip: Scalar=0f)(glyphs) }}}
   *
   * Constructs the vertical catenation of `glyphs`; its height is the sum of the glyphs' heights; its height is
   * normally the sum of the glyphs' heights.
   *
   * The glyphs are vertically aligned as follows in the row, as specified by `align`:
   * {{{
   *      Left      left edges of the bounding boxes aligned
   *      Right     right edges of the bounding boxes aligned
   *      Center    centerlines of the bounding boxes aligned
   *    }}}
   *
   *  If `uniform`, then the glyphs are all treated as if their height
   *  was the same as the maximal height of all the glyphs; and each glyph is treated as if its height were `glyph.h+skip`,
   *  thereby leaving a little vertical space between them in the column.
   *
   * If `frame` is a nontrivial brush (ie with alpha>0), then horizontal lines with foreground `frame` are
   * drawn between glyphs.
   *
   */
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
    val width: Int
    val height: Int


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
    def apply(glyphs: Seq[Glyph]): Glyph           = grid(width, height)(glyphs)
    def apply(glyph: Glyph, glyphs: Glyph*): Glyph = grid(width, height)(glyph::glyphs.toList)


      /**
     *  Each col will be as wide as its widest element;
     *  each row will be as high as its highest element.
     */
    def Table(glyph: Glyph, glyphs: Glyph*): Glyph = table(width, height)(glyph::glyphs.toList)
    def Table(glyphs: Seq[Glyph]): Glyph = table(width, height)(glyphs)

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

      /**
       * Glyphs are arranged as a grid of constant-height rows, dimensioned to (just) fit
       * all cells in each row.
       */
    def rows(glyphs: Seq[Glyph]): Glyph = table(width=width, height=0)(glyphs)
    def rows(glyph: Glyph, glyphs: Glyph*): Glyph = table(width=width, height=0)(glyph::glyphs.toList)
    def Rows(glyphs: Seq[Glyph]): Glyph = table(width=width, height=0)(glyphs)
    def Rows(glyph: Glyph, glyphs: Glyph*): Glyph = table(width=width, height=0)(glyph::glyphs.toList)


      /**
       * Glyphs are arranged as a grid of constant-width columns, dimensioned to (just) fit
       * all cells in each column.
       */
    def cols(glyphs: Seq[Glyph]): Glyph = table(width=0, height=height)(glyphs)
    def cols(glyph: Glyph, glyphs: Glyph*): Glyph = table(width=0, height=height)(glyph::glyphs.toList)
    def Cols(glyphs: Seq[Glyph]): Glyph = table(width=0, height=height)(glyphs)
    def Cols(glyph: Glyph, glyphs: Glyph*): Glyph = table(width=0, height=height)(glyph::glyphs.toList)

    private def uniformlyByRows(width: Int)(glyphs: Seq[Glyph]): Glyph = {
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



    private def uniformlyByCols(height: Int)(glyphs: Seq[Glyph]): Glyph = {
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
    val width: Int = 0
    val height: Int = 0

    def apply(fg: Brush=fg, bg: Brush=bg, padx: Scalar = 0, pady: Scalar = 0, width: Int=0, height: Int=0): GridGenerators = {
      val (_fg, _bg, _padx, _pady, _w, _h) = (fg, bg, padx, pady, width, height)
      new GridGenerators {
        val fg: Brush = _fg
        val bg: Brush = _bg
        val padx: Scalar = _padx
        val pady: Scalar = _pady
        val width: Int = _w
        val height: Int =_h
      }
    }
  }
}
