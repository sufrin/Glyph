package org.sufrin.glyph

import GlyphTypes.{Scalar}

/**
 * `FixedSize` glyphs are Rows / Columns whose widths/heights are preset, and that contain
 * `Space` elements that are designed to fill them.
 *
 * When a row (col) is to be set in a given width (height), its natural size is
 * first established by summing the widths (heights) of its glyphs. If this is
 * smaller than the given width (height), then the excess is distributed among its
 * stretchable `Space` glyphs, in proportion to their stretchability in the appropriate
 * dimension. The aggregate width (height) will now be no less than the given width (height),
 * providing that there is at least one `Space` among the glyphs. If there are none then the
 * aggregate width (height) is taken to be the natural width (height).
 *
 * @see FixedSize.Row
 *
 * The Row and Col APIs are designed so that most of the characteristics of the Row(Col) can
 * be pre-set, thereby making it possible to apply them either to an unbounded number of
 * actual glyphs, or to a sequence of glyphs computed elsewhere.
 *
 */
object FixedSize  {

  /**
   * A possibly-stretchable space used to fill fixed-width rows and columns.
   *
   * If the BACKGROUND is a colour with nonzero alpha, and the BASELINE is nonzero then the space is rendered as a line along the baseline in that colour,
   * and this means that stretchable spaces can play the role of expandable rules.
   *
   *
   * @param _w minimum width
   * @param _h minimum height
   * @param xS lateral stretchability
   * @param yS vertical stretchability
   * @param fg foreground
   * @param bg background
   * @param baseline the nominal baseline
   *
   */
  class Space(var _w: Scalar, var _h: Scalar, xS: Scalar, yS: Scalar, val fg: Brush = nothing, val bg: Brush = nothing, override val baseLine: Scalar=0) extends Glyph {
    override val xStretch: Scalar = xS
    override val yStretch: Scalar = yS
    override def toString: String = s"FixedSpace(${_w}±$xS, ${_h}±$yS)"

    def draw(surface: Surface): Unit = {
      super.drawBackground(surface)
      if (baseLine != 0 && bg.getAlpha != 0) {surface.drawLines$(bg, 0f,baseLine,   w,baseLine) }
      if (fg.getAlpha != 0) {surface.drawLines$(fg, 0f,h/2f,   w,h/2f) }
    }

    def w_=(x: Scalar): Unit = _w = x

    def h_=(x: Scalar): Unit = _h = x

    def diagonal: Vec = Vec(_w, _h)


    /** A copy of this glyph; perhaps with different foreground/background */
    def copy(fg: Brush=fg, bg: Brush=bg): Glyph = {
      new Space(w, h, xS, yS, fg, bg, baseLine)
    }

    val thisGlyph: Glyph = this
  }

  object Fill {
    def apply(wh: Scalar, stretch: Scalar): Space = new Space(wh, wh, stretch, stretch)
  }

  object Space {
    /**
     * A stretchable space of width `w` and height `h`, stretchability `stretch` in
     * both directions, and with the given foreground and background
     */
    def apply(w: Scalar, h: Scalar, stretch: Scalar, fg: Brush=nothing, bg: Brush=nothing): Space =
        new Space(w, h, stretch, stretch, fg, bg)

    /** A horizontal/vertical stretchable filler of nominal width 10ux */
    def fill: Space = new Space(10f, 10f, 1.0f, 1.0f)
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

  val nothing = Brush() color 0

  object Row {
    /**
     * Construct a `RowGenerator` for a row of the given width, with the given foreground and background and alignment.
     * {{{
     * Row(
     *  width: Scalar,
     *  align: VAlignment=Top,
     *  fg: Brush = nothing,
     *  bg: Brush = nothing)(glyphs: ((Glyph, Glyph*) or (Seq[Glyph])
     * }}}
     *
     * Constructs the horizontal catenation of `glyphs`; its height is the largest of the glyphs' heights; its width is
     * normally the larger of `width` and the sum of the glyphs' widths. In the former case stretchable spaces are stretched (in proportion
     * to their horizontal stretchability) so that the bounding box is filled.
     *
     * The glyphs are vertically aligned as follows in the row, as specified by `align`:
     * {{{
     *   Top      tops of the bounding boxes aligned
     *   Bottom   bottoms of the bounding boxes aligned
     *   Mid      (vertical) centers of the bounding boxes aligned
     *   Baseline the baselines are aligned (for all glyphs with baseline >0)
     * }}}
     *
     *
     * If the the sum of the glyphs' widths is less than `width`, then the difference between them
     * is divided among the `FixedWidth.Space` glyphs in proportion to their horizontal stretchability. Examples, where
     * `space(n)`` has horizontal stretchability `n`, and `g1.w+g2.w+g3.w<500`
     * {{{
     *    Row(align=..., width=500)(g1, space(1), g2)
     *        g1/g2 is at the left/right edge of the row
     *    Row(align=..., width=500)(space(1), g1, g2, space(1))
     *        g1, g2 are beside each other in the centre of the row
     *    Row(align=..., width=500)(g1, g2, space(1), g3)
     *        g1, g2 are beside each other at the left edge of the row,
     *        and g3 is at the right edge of the row.
     * }}}
     */
    def apply(width: Scalar, align: VAlignment = Top, fg: Brush=nothing, bg: Brush = nothing): RowGenerators = {
      val (_align, _width, _fg, _bg) = (align, width, fg, bg)
      new RowGenerators {
        val fg: Brush = _fg
        val bg: Brush = _bg
        val width: Scalar = _width
        val align: VAlignment = _align
      }
    }
  }

  /** Analogous to `FixedSize.Row` */
  object Col {
    /**
     * Construct a `ColumnGenerator` for a column of the given height and alignment, with the given foreground and background.
     * {{{
     * Col(
     *  height: Scalar,
     *  align:  Alignment  = Left,
     *  fg: Brush = nothing,
     *  bg: Brush = nothing)(glyphs: (Glyph, Glyph*) or (Seq[Glyph])
     * }}}
     *
     * Constructs the vertical catenation of `glyphs`. Its height is
     * normally the larger of `height` and the sum of the glyphs' heights.
     * In the former case stretchable spaces are stretched (in proportion
     * to their vertical stretchability) so that the bounding box is filled.
     *
     * The glyphs are laterally aligned as follows in the row, as specified by `align`:
     * {{{
     *   Left    left edges of the bounding boxes
     *   Right   right edges of the bounding boxes
     *   Center  lateral centers of the bounding boxes
     * }}}
     */
    def apply(height: Scalar, alignment: Alignment=Left, fg: Brush = nothing, bg: Brush = nothing): ColumnGenerators = {
      val (_alignment, _height, _fg, _bg) = (alignment, height, fg, bg)
      new ColumnGenerators {
        val fg: Brush = _fg
        val bg: Brush = _bg
        val height: Scalar = _height
        val alignment: Alignment = _alignment
      }
    }
  }

  trait RowGenerators { theseGenerators =>
    val fg: Brush
    val bg: Brush
    val width: Scalar
    val align: VAlignment

    def apply(theGlyphs: Seq[Glyph]): Composite = aligned(width, align.proportion, theGlyphs, align.atBaseline)
    def apply(first: Glyph, theGlyphs: Glyph*): Composite = aligned(width, align.proportion, first::theGlyphs.toList, align.atBaseline)

    def Baseline(theGlyphs: Seq[Glyph]): Composite = aligned(width, align.proportion, theGlyphs, true)
    def Baseline(first: Glyph, theGlyphs: Glyph*): Composite = aligned(width, align.proportion, first::theGlyphs.toList, true)

    def Top(theGlyphs: Seq[Glyph]): Composite = aligned(width, 0f, theGlyphs, false)
    def Top(first: Glyph, theGlyphs: Glyph*): Composite = aligned(width, 0f, first::theGlyphs.toList, false)

    def Bottom(theGlyphs: Seq[Glyph]): Composite = aligned(width, 1f, theGlyphs, false)
    def Bottom(first: Glyph, theGlyphs: Glyph*): Composite = aligned(width, 1f, first::theGlyphs.toList, false)

    def Mid(theGlyphs: Seq[Glyph]): Composite = aligned(width,0.5f, theGlyphs, false)
    def Mid(first: Glyph, theGlyphs: Glyph*): Composite = aligned(width, 0.5f, first::theGlyphs.toList, false)

    def aligned(theWidth: Scalar, proportion: Float, theGlyphs: Seq[Glyph], atBaseline: Boolean): Composite = {
      require(theGlyphs.nonEmpty)
      require(theWidth>0f)
      HInflate(theWidth, theGlyphs)
      val height = theGlyphs.map(_.h).max
      val width = theGlyphs.map(_.w).sum
      var x, y = 0f
      val base = if (atBaseline) theGlyphs.map(_.baseLine).max else 0f

      for {glyph <- theGlyphs} {
        val extra = if (atBaseline && glyph.baseLine>0) base-glyph.baseLine else  (height - glyph.h) * proportion
        glyph @@ Vec(x, extra + y) //**
        x += glyph.w
      }

      new Composite(theGlyphs) {
        override val kind: String = "FixedSize.Row"
        val glyphs = theGlyphs
        val diagonal = Vec(width, height)
        override
        val baseLine = base

        val fg = theseGenerators.fg //if (glyphs.isEmpty) theseGenerators.fg else if (theseGenerators.fg.color == 0x00000000) glyphs.head.fg else theseGenerators.fg
        val bg = theseGenerators.bg //if (glyphs.isEmpty) theseGenerators.bg else if (theseGenerators.bg.color == 0x00000000) glyphs.head.bg else theseGenerators.bg

        locally {
          setParents()
        }

        def copy(fg: Brush = this.fg, bg: Brush = this.bg): Composite = aligned(theWidth, proportion, theGlyphs.map(_.copy()), atBaseline)
      }
    }
  }

  trait ColumnGenerators { theseGenerators =>
    val fg: Brush
    val bg: Brush
    val height: Scalar
    val alignment: Alignment


    def apply(glyph: Glyph, theGlyphs: Glyph*): Composite = aligned(height, alignment.proportion, glyph::theGlyphs.toList)
    def apply(theGlyphs: Seq[Glyph]): Composite = aligned(height, alignment.proportion, theGlyphs)

    def Left(glyph: Glyph, theGlyphs: Glyph*): Composite = aligned(height, 0f, glyph::theGlyphs.toList)
    def Left(theGlyphs: Seq[Glyph]): Composite = aligned(height, 0f, theGlyphs)

    def Right(glyph: Glyph, theGlyphs: Glyph*): Composite = aligned(height, 1f, glyph::theGlyphs.toList)
    def Right(theGlyphs: Seq[Glyph]): Composite = aligned(height, 1f, theGlyphs)

    def Center(glyph: Glyph, theGlyphs: Glyph*): Composite = aligned(height, 0.5f, glyph::theGlyphs.toList)
    def Center(theGlyphs: Seq[Glyph]): Composite = aligned(height, 0.5f, theGlyphs)


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




}