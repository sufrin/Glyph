package org.sufrin.glyph

import GlyphTypes.Scalar

import io.github.humbleui.skija.{PaintMode, Path}

/**
 *  Lightweight precursor to `Glyph`. This type arrived in Glyph 0.9 and may eventually
 *  be the basis for simplifying the construction of some heavier-weight passive glyphs.
 */
trait GlyphShape { thisShape =>
  def draw(surface: Surface): Unit    // draw on the given surface
  def diagonal:               Vec     // a bounding box
  @inline def w: Scalar  = diagonal.x
  @inline def h: Scalar  = diagonal.y

  /** This drawable scaled uniformly by `factor` */
  def scale(factor: Scalar): GlyphShape = new GlyphShape {
    def draw(surface: Surface): Unit = surface.withScale(factor) { thisShape.draw(surface) }
    def diagonal: Vec = thisShape.diagonal scaled factor
  }

  /**
   * This drawable turned clockwise through `degrees`. The bounding
   * diagonal is made tighter (but only for nearly-rotationally-symmetric drawables)
   * if `symmetric` is true.
   */
  def turn(degrees: Scalar, symmetric: Boolean=false): GlyphShape = GlyphShape.asGlyph(thisShape).turned(degrees, symmetric)

  /** left '''beside''' right */
  def ||| (thatShape: GlyphShape): GlyphShape = new GlyphShape {

    def draw(surface: Surface): Unit = {
      surface.withOrigin(0,      (h - thisShape.h)*0.5f) { thisShape.draw(surface) }
      surface.withOrigin(thisShape.w, (h - thatShape.h)*0.5f) { thatShape.draw(surface) }
    }

    def diagonal: Vec = Vec(thisShape.w+thatShape.w, thisShape.h max thatShape.h)
  }

  /** left '''above''' right */
  def --- (thatShape: GlyphShape): GlyphShape = new GlyphShape {

    def draw(surface: Surface): Unit = {
      surface.withOrigin((w-thisShape.w)*0.5f, 0)      { thisShape.draw(surface) }
      surface.withOrigin((w-thatShape.w)*0.5f, thisShape.h) { thatShape.draw(surface) }
    }

    def diagonal: Vec = Vec(thisShape.w max thatShape.w, thisShape.h + thatShape.h)
  }

  /** If same areas, ''left'' '''superimposed on''' ''right'',
   *
   *  otherwise ''smaller-area'' '''superimposed on''' ''larger-area''
   */
  def ~~~ (thatShape: GlyphShape): GlyphShape = new GlyphShape {
    val (big, small) = if (thisShape.w*thisShape.h <= thatShape.w*thatShape.h) (thatShape, thisShape) else (thisShape, thatShape)
    def draw(surface: Surface): Unit = {
      surface.withOrigin((w-big.w)*0.5f,   (h-big.h)*0.5f)   { big.draw(surface) }
      surface.withOrigin((w-small.w)*0.5f, (h-small.h)*0.5f) { small.draw(surface) }
    }
    def diagonal: Vec = Vec(thisShape.w max thatShape.w, thisShape.h max thatShape.h)
  }

  /**
   *  A shape like `thisShape`, but with an empty bounding box, and drawn
   *  at the given `(dx, dy)`.
   *
   * @see line
   */
  def at(dx: Scalar, dy: Scalar): GlyphShape = new GlyphShape {
    def draw(surface: Surface): Unit =
      surface.withOrigin(dx, dy) { thisShape.draw(surface) }

    def diagonal: Vec = Vec.Zero
  }

  /**
   * This shape, with a background (as far as possible of the same shape) coloured by `brush`.
   */
  def bg(brush: Brush): GlyphShape = thisShape ~~~ GlyphShape.rect(thisShape.w, thisShape.h)(brush)
}

object GlyphShape {
  /**
   * A circle of radius `r`, occupying a square of side `2r+fg.strokeWidth`
   * Unless `fg.mode=PaintMode.STROKE` the circle is filled.
   */
  def circle(r: Scalar)(fg: Brush): GlyphShape = new GlyphShape {
    val delta=Vec(fg.strokeWidth/2, fg.strokeWidth/2)
    def draw(surface: Surface): Unit = surface.withOrigin(delta){
      surface.drawOval(fg, Vec.Zero, diag)
    }

    val diag: Vec = Vec(2*r, 2*r)
    val diagonal: Vec = diag+(delta scaled 2)

    override def bg(brush: Brush): GlyphShape = circle(r)(brush(mode=PaintMode.FILL)) ~~~ this
  }

  /**
   * An oval bounded by a `(width x height)` rectangle, occupying a `(width+delta x height+delta)` rectangle
   * (where delta=`fg.strokeWidth`).
   * Unless `fg.mode=PaintMode.STROKE` the oval is filled.
   */
  def oval(width: Scalar, height: Scalar)(fg: Brush): GlyphShape = new GlyphShape {
    val delta=Vec(fg.strokeWidth/2, fg.strokeWidth/2)
    def draw(surface: Surface): Unit = surface.withOrigin(delta){
      surface.drawOval(fg, Vec.Zero, diag)
    }

    val diag: Vec = Vec(w, h)
    val diagonal: Vec = diag+(delta scaled 2)

    override def bg(brush: Brush): GlyphShape = oval(w, h)(brush(mode=PaintMode.FILL)) ~~~ this
  }

  /**
   *  A line that has no bounding box. Used for drawing lines to be superimposed
   *  on an existing drawing.
   *
   *  For example, the following is the shape of a couple of `spot`s joined by a black line.
   *  It has an empty bounding box, but can be superimposed on another shape (which
   *  should be large enough to contain the spots and the line at their given positions).
   *  {{{
   *    spot.at(p, dy) ~~~
   *    spot.at(dx, p) ~~~
   *    line(Vec(p, dy), Vec(dx, p))(black)
   *  }}}
   *
   * @see at
   */
  def line(start: Vec, end: Vec)(brush: Brush): GlyphShape = new GlyphShape {
    def draw(surface: Surface): Unit =  surface.drawLines(brush, List(start, end))
    def diagonal: Vec = Vec.Zero
  }

  /**
   *
   * @param startx
   * @param starty
   * @param endx
   * @param endy
   * @param brush
   * @return
   */
  def line(startx: Scalar, starty: Scalar, endx: Scalar, endy: Scalar)(brush: Brush): GlyphShape = new GlyphShape {
    def draw(surface: Surface): Unit = surface.drawLines$(brush, startx, starty, endx, endy)
    def diagonal: Vec = Vec.Zero
  }

  /**
   * A `(width, height)` rectangle, occupying a `(width+fg.strokeWidth, height+fg.strokeWidth)` rectangle.
   * Unless `fg.mode=PaintMode.STROKE` the rectangle is filled.
   */
  def rect(width: Scalar, height: Scalar)(fg: Brush): GlyphShape = new GlyphShape {
    val delta=Vec(fg.strokeWidth/2, fg.strokeWidth/2)
    def draw(surface: Surface): Unit = surface.withOrigin(delta){
      if (fg.mode!=STROKE) surface.fillRect(fg, diag)
      surface.drawRect(fg, diag)
    }
    val diag: Vec = Vec(width, height)
    val diagonal: Vec = diag+(delta scaled 2)

    override def bg(brush: Brush): GlyphShape = this ~~~ rect(w, h)(brush(mode=FILL))
  }

  /**
   * A polygon made of `vertext::vertices`, and with the natural bounding box.
   */
  def polygon(vertex: (Scalar, Scalar), vertices: (Scalar, Scalar)*): Brush=>GlyphShape = polygon(vertex::vertices.toList)

  /**
   * A polygon made of the given `vertices`, and with the natural bounding box.
   */
  def polygon(vertices: Iterable[(Scalar, Scalar)])(fg: Brush): GlyphShape = new GlyphShape {
    val kind: String = "Polygon"
    override def toString: String = s"Polygon($diagonal, fg=$fg)(\n     ${vertices.mkString(",")}\n)"

    val path = new Path()
    locally {
      val v = vertices.iterator
      val (sx, sy) = v.next()
      path.moveTo(sx, sy)
      while (v.hasNext) {
        val (vx, vy) = v.next()
        path.lineTo(vx, vy)
      }
    }

    var offsetL, offsetT = 0f

    val diagonal = {
      val bounds = path.getBounds()
      offsetL = bounds._left -fg.strokeWidth/2
      offsetT = bounds._top  -fg.strokeWidth/2
      Vec(bounds._right-bounds._left+fg.strokeWidth, bounds._bottom-bounds._top+fg.strokeWidth)
    }

    def draw(surface: Surface): Unit = surface.withOrigin(-offsetL, -offsetT) { surface.drawPath(fg, path) }

    override def bg(brush: Brush): GlyphShape = this ~~~ polygon(vertices)(brush(mode=FILL))

  }

  /**
   * A shape that is drawn as the superimposition of the given shapes, with the later layers
   * in the sequence appearing on top of the earlier layers regardless of their
   * area. Note that this is not always a `~~~`-reduction: for in that case
   * the smallest area shape percolates to the top.
   *
   * @see ~~~
   */
  def superimposed(shapes: Seq[GlyphShape]): GlyphShape = new GlyphShape {
    val tw = Measure.maxWidth(shapes)
    val th = Measure.maxHeight(shapes)

    def draw(surface: Surface): Unit =
      for { shape <- shapes }
        surface.withOrigin((tw-shape.w)/2, (th-shape.h)/2) { shape.draw(surface)}

    def diagonal: Vec = Vec(tw, th)
  }

  /** @see superimposed */
  def superimposed(shape: GlyphShape, shapes: GlyphShape*): GlyphShape = superimposed(shape::shapes.toList)


  /**
   *  A standard glyph drawn as the given `shape`.
   */
  implicit def asGlyph(shape: GlyphShape): Glyph = new Glyph { wrapped =>
    def copy(fg: Brush, bg: Brush): Glyph = wrapped.copy(fg, bg)
    def draw(surface: Surface): Unit = shape.draw(surface)
    val diagonal: Vec = shape.diagonal
    val fg: Brush = Brushes.transparent
    val bg: Brush = Brushes.transparent
  }

  val STROKE:          PaintMode = PaintMode.STROKE
  val FILL:            PaintMode = PaintMode.FILL
  val STROKE_AND_FILL: PaintMode = PaintMode.STROKE_AND_FILL
}
