package org.sufrin.glyph

import GlyphTypes.Scalar

import io.github.humbleui.skija.{PaintMode, Path}

/**
 *  Lightweight precursor to `Glyph`. This type arrived in Glyph 0.9 and may eventually
 *  be the basis for simplifying the construction of heavier-weight passive glyphs.
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
  def turn(degrees: Scalar, symmetric: Boolean=false): GlyphShape = GlyphShape.toGlyph(thisShape).turned(degrees, symmetric)

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
   * A `(width, height)` rectangle, occupying a `(width+fg.strokeWidth, height+fg.strokeWidth)` rectangle.
   * Unless `fg.mode=PaintMode.STROKE` the rectangle is filled.
   */
  def rect(width: Scalar, height: Scalar)(fg: Brush): GlyphShape = new GlyphShape {
    val delta=Vec(fg.strokeWidth/2, fg.strokeWidth/2)
    def draw(surface: Surface): Unit = surface.withOrigin(delta){
      if (fg.mode!=PaintMode.STROKE) surface.fillRect(fg, diag)
      surface.drawRect(fg, diag)
    }
    val diag: Vec = Vec(width, height)
    val diagonal: Vec = diag+(delta scaled 2)

    override def bg(brush: Brush): GlyphShape = this ~~~ rect(w, h)(brush(mode=PaintMode.FILL))

  }

  def polygon(vertices: (Scalar, Scalar)*): Brush=>GlyphShape = polygon(vertices)

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

    def draw(surface: Surface): Unit = {
      /*surface.withClip(diagonal)*/  { surface.withOrigin(-offsetL, -offsetT) { surface.drawPath(fg, path) } }
    }

    override def bg(brush: Brush): GlyphShape = this ~~~ polygon(vertices)(brush(mode=PaintMode.FILL))

  }


  implicit def toGlyph(drawable: GlyphShape): Glyph = new Glyph { wrapped =>
    def copy(fg: Brush, bg: Brush): Glyph = wrapped.copy(fg, bg)
    def draw(surface: Surface): Unit = drawable.draw(surface)
    val diagonal: Vec = drawable.diagonal
    val fg: Brush = Brushes.transparent
    val bg: Brush = Brushes.transparent
  }
}
