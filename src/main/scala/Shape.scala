package org.sufrin.glyph

import Shape.{composite, rect, superimposed}
import unstyled.dynamic.Animateable
import GlyphTypes.{Font, PaintMode, Path, PathFillMode, Scalar}

import io.github.humbleui.types.Rect


/**
 *  Lightweight precursor to `Glyph`. This type arrived in Glyph 0.9 and may eventually
 *  be the basis for simplifying the construction of some heavier-weight passive glyphs.
 *
 *  Atomic glyph shapes are coloured by a brush, and they are filled (or not) depending
 *  on the `mode` of the brush: `FILL` or `STROKE`.
 */
trait Shape { thisShape =>
  def draw(surface: Surface): Unit                                     // draw on the given surface
  def diagonal:               Vec                                      // a bounding box
  def withBrushes(fg: Brush=fg, bg: Brush=bg): Shape                   // a copy -- with new brushes when that means anything (it doesn't for composites)
  def withBackground(bg: Brush): Shape = superimposed(List(rect(w, h)(bg), this))  // this shape, with a rectangular background coloured bg
  def cardinalPoints: Seq[Vec] = Seq.empty                             // places for handles

  /** width */
  @inline def w:      Scalar  = diagonal.x
  /** height */
  @inline def h:      Scalar  = diagonal.y
  /** offset to centre */
  @inline def centre: Vec     = diagonal * 0.5f

  val fg: Brush = Brushes.transparent
  val bg: Brush = null

  /** Is the given point within the shape: default is within the bounding box */
  @inline def encloses(point: Vec): Boolean = enclosing(point).nonEmpty

  /** The "nearest" shape that encloses the given point */
  def enclosing(point: Vec): Option[Shape] =
    if (0<=point.x && point.x<=w && 0<=point.y && point.y<=h) Some(thisShape) else None

  /**
   * This drawable scaled uniformly by `factor`. Implemented
   * efficiently by a proxy class to avoid space leaks.
   *
   * @see turn
   */
  def scale(factor: Scalar): Shape = if (factor==1) thisShape else new Scaled(thisShape, factor)

  private class Scaled(original: Shape, factor: Scalar) extends Shape {
    def draw(surface: Surface): Unit = surface.withScale(factor) { original.draw(surface) }
    def diagonal: Vec = original.diagonal * (factor)
    override def toString: String = s"$thisShape.scale(${factor})"
    override def enclosing(point: Vec): Option[Shape] = thisShape.enclosing(point/factor)
    override def scale(factor: Scalar): Shape = if (factor==1) original else new Scaled(original, this.factor*factor)
    override def withBrushes(fg: Brush=fg, bg: Brush=bg): Shape = original.withBrushes(fg, bg).scale(factor)
  }

  /**
   * This drawable rotated clockwise about its centre through `degrees`. Implemented
   * efficiently by a proxy class to avoid space leaks.
   *
   * The point is that if implemented by the "normal" method of transforming glyphs (ie by generating a glyph that draws the "host" glyph
   * with an additional transform), the glyph resulting from a sequence of `turn` contains a sequence of transformed glyphs that embody
   * the entire sequence. The method here just aggregates successive rotations.
   */
  def turn(degrees: Scalar, tight: Boolean=false): Shape =
      if (degrees==0f) thisShape else new Turned(thisShape, degrees, tight)

  private class Turned(original: Shape, degrees: Scalar, tight: Boolean) extends Shape {
    override def turn(degrees: Scalar, tight: Boolean=true): Shape =
      if (degrees==0f) original else new Turned(original, this.degrees+degrees, tight)

    override val fg: Brush = original.fg
    override val bg: Brush = original.bg

    override def withBrushes(fg: Brush=fg, bg: Brush=bg): Shape = original.withBrushes(fg, bg).turn(degrees, tight)

    private val Theta = Math.toRadians(((degrees % 360) + 360) % 360)
    private val d = original.diagonal

    @inline private def cos(theta: Double): Scalar = Math.cos(theta).toFloat
    @inline private def sin(theta: Double): Scalar = Math.sin(theta).toFloat
    /** effective diagonal when theta is in q0 or q2 */
    @inline private def q0q2(theta: Double)  = Vec((d.x * cos(theta) + d.y * sin(theta)).abs, (d.x * sin(theta) + d.y * cos(theta)).abs)
    /** effective diagonal when theta is in q1 or q3  */
    @inline private def q1q3(theta: Double) = Vec((d.x * sin(theta) + d.y * cos(theta)).abs, (d.x * cos(theta) + d.y * sin(theta)).abs)

    def diagonal: Vec = if (tight) {
      val D =  d.x max d.y
      Vec(D,D)
    } else {
      val quadrant = (((degrees % 360) + 360) % 360 / 90).toInt
      quadrant match {
        case 0 => q0q2(Theta)
        case 1 => q1q3(Theta - Math.PI/2)
        case 2 => q0q2(Theta - Math.PI)
        case 3 => q1q3(Theta - Math.PI/2 - Math.PI)
      }
    }

    private val originalCentre = d * 0.5f
    private val thisCentre     = diagonal * 0.5f
    private val delta = thisCentre - originalCentre

    def draw(surface: Surface): Unit = {
      surface.withOrigin(delta) {
        surface.withRot(degrees, originalCentre) {
          original.draw(surface)
        }
      }
    }


    @inline private def relativeLocation(glyphPos: Vec): Vec = {
        val theta    = Theta
        val cosTheta = Math.cos(Theta).toFloat
        val sinTheta = Math.sin(Theta).toFloat
        val Vec(x, y) = glyphPos - thisCentre   //vector to the centre of this glyph
        val xr = x*cosTheta + y*sinTheta        //rotated by theta
        val yr = y*cosTheta - x*sinTheta
        originalCentre+Vec(xr, yr)
    }

    override def enclosing(p: Vec): Option[Shape] = {
      val res = original.enclosing(relativeLocation(p))
      res
    }

  }



  /** left '''beside''' right */
  def ||| (thatShape: Shape): Shape = new Shape {
    val dThis = Vec(0,  (h - thisShape.h)*0.5f)
    val dThat = Vec(thisShape.w, (h - thatShape.h)*0.5f)
    def draw(surface: Surface): Unit = {
      surface.withOrigin(dThis) { thisShape.draw(surface) }
      surface.withOrigin(dThat) { thatShape.draw(surface) }
    }

    def diagonal: Vec = Vec(thisShape.w+thatShape.w, thisShape.h max thatShape.h)

    override def enclosing(point: Vec): Option[Shape] =
      thisShape.enclosing(point-dThis) orElse thatShape.enclosing(point-dThat)

    override def toString: String = s"$thisShape|||$thatShape"

    override def scale(factor: Scalar): Shape = if (factor==1) this else thisShape ||| thatShape

    override def withBrushes(fg: Brush=fg, bg: Brush=bg): Shape = thisShape.withBrushes(thisShape.fg, bg) ||| thatShape.withBrushes(thatShape.fg, bg)

  }

  /** left '''above''' right */
  def --- (thatShape: Shape): Shape = new Shape {
     val dThis = Vec((w-thisShape.w)*0.5f, 0)
     val dThat = Vec((w-thatShape.w)*0.5f, thisShape.h)

    def draw(surface: Surface): Unit = {
      surface.withOrigin(dThis) { thisShape.draw(surface) }
      surface.withOrigin(dThat) { thatShape.draw(surface) }
    }

    def diagonal: Vec = Vec(thisShape.w max thatShape.w, thisShape.h + thatShape.h)

    override def enclosing(point: Vec): Option[Shape] =
      thisShape.enclosing(point-dThis) orElse thatShape.enclosing(point-dThat)

    override def toString: String = s"$thisShape---$thatShape"

    override def scale(factor: Scalar): Shape = if (factor==1) this else thisShape --- thatShape

    override def withBrushes(fg: Brush=fg, bg: Brush=bg): Shape = thisShape.withBrushes(thisShape.fg, bg) --- thatShape.withBrushes(thatShape.fg, bg)


  }

  /** If same areas, ''left'' '''superimposed on''' ''right'',
   *
   *  otherwise ''smaller-area'' '''superimposed on''' ''larger-area''
   */
  def ~~~ (thatShape: Shape): Shape = {
    val (big, small) = if (thisShape.w*thisShape.h <= thatShape.w*thatShape.h) (thatShape, thisShape) else (thisShape, thatShape)
    composite(big, small)
  }

  /**
   * A standard glyph with the given foreground and background and drawn as `shape`.
   */
  def asGlyph: Glyph = new Glyph {
    wrapped =>
    /**
     * Stock copy of the glyph
     */
    def copy(fg: Brush, bg: Brush): Glyph =
      thisShape match {
        case g: Glyph => g.copy(fg, bg)
        case other    => other.withBrushes(fg, bg).asGlyph
      }

    def draw(surface: Surface): Unit = thisShape.draw(surface)

    val diagonal: Vec = thisShape.diagonal
    override val fg: Brush = thisShape.fg
    override val bg: Brush = thisShape.bg
  }

  /**
   *  A `LocatedShape` shaped like `thisShape`, initially drawn at `(x,y)`.
   *
   */
  def locatedAt(x: Scalar, y: Scalar): LocatedShape = LocatedShape(x, y, thisShape)
  def locatedAt(pos: (Scalar, Scalar)): LocatedShape = LocatedShape(pos._1, pos._2, thisShape)
  def locatedAt(pos: Vec): LocatedShape = LocatedShape(pos.x, pos.y, thisShape)

  def centredAt(x: Scalar, y: Scalar): LocatedShape = LocatedShape(x-thisShape.w*0.5f, y-thisShape.h*0.5f, thisShape)
  def centredAt(pos: (Scalar, Scalar)): LocatedShape = LocatedShape(pos._1-thisShape.w*0.5f, pos._2-thisShape.h*0.5f, thisShape)
  def centredAt(pos: Vec): LocatedShape = LocatedShape(pos.x-thisShape.w*0.5f, pos.y-thisShape.h*0.5f, thisShape)
  /** An shape at an initiallly-unspecified location */
  def floating(): LocatedShape = LocatedShape(0, 0, thisShape)

  /**
   *  A `TargetShape` shaped like `thisShape`, initially drawn at `(x,y)`.
   *
   * @see TargetShape
   */
  def targetLocatedAt(x: Scalar, y: Scalar): TargetShape = TargetShape(x, y, thisShape)
  def targetLocatedAt(pos: (Scalar, Scalar)): TargetShape = TargetShape(pos._1, pos._2, thisShape)
  def targetLocatedAt(pos: Vec): LocatedShape = TargetShape(pos.x, pos.y, thisShape)
  def targetCentredAt(x: Scalar, y: Scalar): TargetShape = TargetShape(x-thisShape.w*0.5f, y-thisShape.h*0.5f, thisShape)
  def targetCentredAt(pos: (Scalar, Scalar)): TargetShape = TargetShape(pos._1-thisShape.w*0.5f, pos._2-thisShape.h*0.5f, thisShape)
  def targetCentredAt(pos: Vec): LocatedShape = TargetShape(pos.x-thisShape.w*0.5f, pos.y-thisShape.h*0.5f, thisShape)

}


object Shape {
  /**
   * A circle of radius `r`, occupying a square of side `2r+fg.strokeWidth`
   * Unless `fg.mode=PaintMode.STROKE` the circle is filled.
   */
  case class circle(r: Scalar)(brush: Brush) extends Shape {
    override val fg: Brush = brush
    val delta = Vec(fg.strokeWidth / 2, fg.strokeWidth / 2)

    def draw(surface: Surface): Unit = surface.withOrigin(delta) {
      surface.drawOval(fg, Vec.Zero, diag)
    }

    val diag: Vec = Vec(2 * r, 2 * r)
    val diagonal: Vec = diag + (delta * 2)
    val middle = diagonal * 0.5f
    override val cardinalPoints: Seq[Vec] = {
      val halfR = r*0.5
      List(Vec(diag.x, r), Vec(r, diag.y), Vec(0, r), Vec(r, 0))
    }

    override def toString: String = s"circle($r)($fg)"

    override def enclosing(point: Vec): Option[Shape] = {
      val res = if (point.distanceWithin(middle, r)) Some(this) else None
      res
    }

    override def scale(factor: Scalar): Shape = if (factor == 1) this else circle(factor * r)(fg)

    override def withBrushes(fg: Brush=fg, bg: Brush=bg): Shape =
      if (bg==null) circle(r)(fg) else circle(r)(bg) ~~~ circle(r)(fg)

  }

  /**
   * An oval bounded by a `(width x height)` rectangle, occupying a `(width+delta x height+delta)` rectangle
   * (where delta=`brush.strokeWidth`).
   */
  case class oval(width: Scalar, height: Scalar)(brush: Brush) extends Shape {
    override val fg: Brush = brush
    val delta = Vec(fg.strokeWidth / 2, fg.strokeWidth / 2)

    def draw(surface: Surface): Unit = surface.withOrigin(delta) {
      surface.drawOval(fg, Vec.Zero, diag)
    }

    val diag: Vec = Vec(width, height)
    val diagonal: Vec = diag + (delta * 2)

    override def toString: String = s"oval($width,$height)($fg)"


    override def scale(factor: Scalar): Shape = if (factor == 1) this else oval(factor * width, factor * height)(fg)

    //override def rotationallySymmetric: Boolean = .6 < Math.abs(w-h)/(w max h)

    override def withBrushes(fg: Brush=fg, bg: Brush=bg): Shape =
      if (bg==null) oval(width, height)(fg) else oval(width, height)(fg) ~~~ oval(width, height)(fg)
  }

  /**
   * An arc/sector of a (width x height) oval occupying a `(width+delta x height+delta)` rectangle. The
   * path of the arc starts in direction `startAngle` and extends for `sweepAngle`. The centre is included in
   * the path if `incCentre` is true.
   */
  case class arc(width: Scalar, height: Scalar, startAngle: Scalar, sweepAngle: Scalar, incCentre: Boolean)(brush: Brush)extends Shape {
    override val fg: Brush = brush
    val delta = Vec(fg.strokeWidth / 2, fg.strokeWidth / 2)

    def draw(surface: Surface): Unit = surface.withOrigin(delta) {
      surface.canvas.drawArc(0, 0, width, height, startAngle, sweepAngle, incCentre, fg)
    }

    val diag: Vec = Vec(width, height)
    val diagonal: Vec = diag + (delta * 2)

    override def toString: String = s"arc($width,$height,$startAngle,$sweepAngle,$incCentre)($fg)"

    /**
     * This arc superimposed on an arc with the same characteristics filled with `bg`
     */
    override def withBrushes(fg: Brush, bg: Brush=null): Shape = {
      val thisArc = arc(width, height, startAngle, sweepAngle, incCentre)(_)
      if (bg==null) thisArc(fg) else (thisArc(bg(mode = PaintMode.FILL)) ~~~ thisArc(fg))
    }

    override def scale(factor: Scalar): Shape = if (factor == 1) this else oval(factor * width, factor * height)(fg)

  }

  /**
   * A circular pie with the given radius, with slices painted by brushes.
   * {{{ requires brushes.length>0 }}}
   */
  def pie(radius: Scalar)(brushes: Brush*): Shape = {
    require(brushes.length > 0, "A pie must have a positive number of brushes")
    val sector = 360.0f / brushes.length
    var rot = 0.0f
    superimposed(
      brushes.map { brush => rot += sector; arc(2 * radius, 2 * radius, rot, sector, true)(brush) }
    )
  }

  def text(text: String, font: Font=fallback.textFont)(fg: Brush=fallback.textForeground, bg: Brush=fallback.textBackground): Shape =
      unstyled.Text(text, font, fg, bg).asInstanceOf[Shape]

  case class label()


  /**
   * A line that has no bounding box. Used for drawing lines to be superimposed
   * on an existing drawing.
   *
   * For example, the following is the shape of a couple of `spot`s joined by a black line.
   * It has an empty bounding box, but can be superimposed on another shape (which
   * should be large enough to contain the spots and the line at their given positions).
   * {{{
   *    spot.located(p, dy) ~~~
   *    spot.located(dx, p) ~~~
   *    line(Vec(p, dy), Vec(dx, p))(black)
   *    }}}
   *
   * @see located
   */
  def line(start: Vec, end: Vec)(brush: Brush): Shape = line(start.x, start.y, end.x, end.y)(brush)

  /**
   * @see line
   */
  case class line(startx: Scalar, starty: Scalar, endx: Scalar, endy: Scalar)(brush: Brush) extends Shape {
    override val fg: Brush = brush
    def draw(surface: Surface): Unit = surface.drawLines(fg, startx, starty, endx, endy)
    def diagonal: Vec = Vec.Zero
    override def toString: String = s"line(($startx,$starty), ($endx,$endy))($fg)"
    override def withBrushes(fg: Brush=fg, bg: Brush=null): Shape = line(startx, starty, endx, endy)(fg)
  }

  case class lineBetween(l: LocatedShape, r: LocatedShape)(brush: Brush) extends Shape {
      override val fg: Brush = brush
      val diagonal: Vec = Vec.Zero

      def draw(surface: Surface): Unit =
        surface.drawLines(fg, l.x+l.w*0.5f, l.y+l.h*0.5f, r.x+r.w*0.5f, r.y+r.h*0.5f)

      override def toString: String = s"lineBetween($l,$r)($fg)"

      override def withBrushes(fg: Brush, bg: Brush=null): Shape = lineBetween(l, r)(fg)
  }

  def arrow(brush: Brush): Shape = {
    val f = 4f
    val a = f*3.5f
    val b = 3f *a
    val c = f* 20f
    val d = f * 30f
    polygon((0, a), (0, b), (c, b), (c, a + b), (d, (a + b) / 2), (c, 0), (c, a))(brush) //~~~rect(d,a+b)(black(mode=STROKE))
  }

  /**
   * A `(width, height)` rectangle, occupying a `(width+brush.strokeWidth, height+brush.strokeWidth)` rectangle.
   * The rectangle is filled, or not, according to  `brush.mode: PaintMode`
   */
  def rect(width: Scalar, height: Scalar): Brush => Shape = polygon((0, 0), (width, 0), (width, height), (0, height))


  /**
   * A closed polygon made of `vertex::vertices`.
   */
  def polygon(vertex: (Scalar, Scalar), vertices: (Scalar, Scalar)*): Brush => Shape = polygon(vertex :: vertices.toList)

  /**
   * A closed polygon made of `vertices`.
   */
  def polygon(vertices: Seq[Vec]): Brush =>Shape = polygon(vertices.map { v =>(v.x, v. y)})

  /**
   * A closed polygon made of the given `vertices`. It is "normalized", in that
   * its width is the distance from its leftmost to its rightmost visible point,
   * and its height is the distance from its topmost to its bottommost
   * visible point; and its top,left point appears at the origin.
   *
   */
  case class polygon(vertices: Seq[(Scalar, Scalar)])(brush: Brush) extends Shape {
    override val fg: Brush=brush
    val kind: String = "Polygon"

    override def toString: String = s"Polygon(...)[$diagonal](fg=$fg)"

    override def withBrushes(fg: Brush=fg, bg: Brush=bg): Shape =  polygon(vertices)(fg)

    val path = new Path()
    locally {
      val v = vertices.iterator
      val (sx, sy) = v.next()
      path.moveTo(sx, sy)
      while (v.hasNext) {
        val (vx, vy) = v.next()
        path.lineTo(vx, vy)
      }
      path.closePath()
    }


    private var offsetL, offsetT = 0f

    override def cardinalPoints: Seq[Vec] = vertices.map{ case (x,y) => Vec(x-offsetL, y-offsetT) }

    val diagonal = {
      val bounds = path.getBounds()
      path.updateBoundsCache()
      offsetL = bounds._left - fg.strokeWidth / 2
      offsetT = bounds._top - fg.strokeWidth / 2
      Vec(bounds._right - bounds._left + fg.strokeWidth, bounds._bottom - bounds._top + fg.strokeWidth)
    }

    override def enclosing(point: Vec): Option[Shape] = {
      val origin = point + (offsetL, offsetT)
      if (path.contains(origin.x, origin.y)) Some(this) else None
    }

    def draw(surface: Surface): Unit = surface.withOrigin(-offsetL, -offsetT) {
      surface.drawPath(fg, path)
    }

    override def scale(factor: Scalar): Shape = if (factor ==1) this else {
      polygon(vertices.map { case ( x,y) => (factor*x, factor * y)})(fg)
    }


  }

  /**
   * A glyph defined by the same methods as an `AnimatedGlyph`. It invokes a redraw whenever
   * its shape has been regenerated.
   */
  abstract class AnimatedShapeGlyph[T](initial: T, diagonal: Vec) extends AnimatedShape[T](initial, diagonal) with Glyph {
    override def afterShape(): Unit = reDraw()
    def copy(fg: Brush, bg: Brush): AnimatedShapeGlyph[T] = { throw new UnsupportedOperationException (s"abstract AnimatedShapeGlyph.copy($fg, $bg) -- requires concrete override"); null }
  }




  /**
   * A shape that is drawn as the superimposition of the given shapes, with the later layers
   * in the sequence appearing on top of the earlier layers regardless of their
   * area. Note that this is not always a `~~~`-reduction: for in that case
   * the smallest area shape percolates to the top.
   *
   * @see ~~~
   */
  case class superimposed(shapes: Seq[Shape]) extends Shape {
    val tw = Measure.maxWidth(shapes)
    val th = Measure.maxHeight(shapes)
    val deltas = for {shape <- shapes} yield (Vec((tw - shape.w) / 2, (th - shape.h) / 2))

    def draw(surface: Surface): Unit = {
      val shape = shapes.iterator
      for {delta <- deltas}
        surface.withOrigin(delta) {
          shape.next().draw(surface)
        }
    }

    def diagonal: Vec = Vec(tw, th)

    override def enclosing(point: Vec): Option[Shape] = {
      var it: Option[Shape] = None
      val delta = deltas.iterator
      val shape = shapes.iterator
      //println(s"$this.enclosing($point)")
      while (it.isEmpty && delta.hasNext) {
        val s = shape.next()
        val d = delta.next()
        //println(s"  $s@${point-d}")
        it = s.enclosing(point - d)
      }
      it
    }

    override def toString: String = s"(${shapes.mkString("~~~")})"

    override def scale(factor: Scalar): Shape = if (factor==1) this else superimposed(shapes.map(_.scale(factor)))

    override def withBrushes(fg: Brush=fg, bg: Brush=bg): Shape = superimposed(shapes)

  }

  /** @see superimposed */
  def composite(shape: Shape, shapes: Shape*): Shape = superimposed(shape :: shapes.toList)


  /** Superposition of located shapes */
  def composite(shape: LocatedShape, shapes: LocatedShape*): Shape = composite(shape :: shapes.toList)

  /** Superposition of located shapes */
  case class composite(shapes: Iterable[LocatedShape]) extends Shape {
    def withBrushes(brush: Brush): Shape =  this
    val diagonals = for {shape <- shapes} yield (shape.x+shape.w, shape.y+shape.h)
    val tw = diagonals.map(_._1).max
    val th = diagonals.map(_._2).max

    def draw(surface: Surface): Unit = {
      for {shape <- shapes}
         {
          shape.draw(surface)
        }
    }

    def diagonal: Vec = Vec(tw, th)

    override def enclosing(point: Vec): Option[Shape] = {
      var it: Option[Shape] = None
      val shape = shapes.iterator
      //println(s"$this.enclosing($point)")
      while (it.isEmpty && shape.hasNext) {
        val s = shape.next()
        //println(s"  $s@${point-d}")
        it = s.shape.enclosing(point - (s.x, s.y))
      }
      it
    }

    override def withBrushes(fg: Brush=fg, bg: Brush=bg): Shape = composite(shapes.map(_.withBrushes(fg, bg)))

    override def toString: String = s"(${shapes.mkString("~~~")})"

  }

  import PaintMode.{FILL, STROKE, STROKE_AND_FILL}


  def cardinalPoints(shape: Shape): Seq[Vec] = {
    import shape.{h, w}
    if (shape.cardinalPoints.nonEmpty)
      shape.cardinalPoints
    else
      Array(Vec(w, h / 2), Vec(w, h), Vec(w / 2, h), Vec(0, h), Vec(0, h / 2), Vec(0, 0), Vec(w / 2, 0), Vec(w, 0)).toSeq
  }


  case class fromGlyph(glyph: Glyph) extends Shape {
    def draw(surface: Surface): Unit = glyph.draw(surface)
    def diagonal: Vec = glyph.diagonal
    override def withBrushes(fg: Brush=glyph.fg, bg: Brush=glyph.bg): Shape = glyph.copy(fg, bg)
  }

  implicit def toVec(pair: (Scalar, Scalar)): Vec = Vec(pair._1, pair._2)
  implicit def toPair(vec: Vec): (Scalar, Scalar) = (vec.x, vec.y)

}

case class Handle(thing: TargetShape, shape: Shape, origin: Vec) {
  def draw(surface: Surface): Unit = { surface.withOrigin(origin) { shape.draw(surface) } }
  def canHandle(point: Vec): Boolean = {
    val br = origin+shape.diagonal
    origin.x<=point.x && point.x<=br.x && origin.y<=point.y && point.y<=br.y
  }
}

case class Handles(thing: TargetShape, handles: Seq[Handle]) {
  var enabled: Boolean = true
  def draw(surface: Surface): Unit =
    if (enabled) for { handle<-handles } handle.draw(surface)
  def canHandle(point: Vec): Seq[Handle] = {
    if (enabled)
      for { handle<-handles if handle.canHandle(point) } yield handle
    else
      Seq.empty
  }
}

/**
 * Experimental mutable shape. If `absolute` then
 * the path is drawn "as is" -- with all coordinates interpreted relative to the origin, otherwise it is drawn
 * so that the top/left coordinate of the path is at the origin.
 *
 * Its diagonal always denotes the distance from the top left to the
 * bottom right coordinate of the path.
 */
class PathShape(override val fg: Brush, absolute: Boolean = true) extends Shape {
  hostShape =>
  val path = new Path
  locally {
    if (absolute) path.moveTo(0, 0)
  }

  /** A mutable shape that shares the same path; but is drawn with a possibly-different (foreground) brush */
  override def withBrushes(fg: Brush=fg, bg: Brush=bg): Shape =  {
    val (f, b) = (fg, bg)
    new Shape {
      override val fg: Brush = f
      override val bg: Brush = b
      override def draw(surface: Surface): Unit = hostShape.draw(fg, surface)
      override def diagonal: Vec = hostShape.diagonal
      override def withBrushes(fg: Brush=fg, bg: Brush=bg): Shape = hostShape.withBrushes(fg, bg)
    }
  }

  def reset(): Unit = {
    path.reset()
    if (absolute) path.moveTo(0, 0)
  }

  def draw(surface: Surface): Unit = draw(fg, surface)

  def draw(fg: Brush, surface: Surface): Unit = if (absolute) {
    surface.drawPath(fg, path)
  } else {
    val bounds = path.getBounds()
    val offsetL = bounds._left - fg.strokeWidth / 2
    val offsetT = bounds._top - fg.strokeWidth / 2
    surface.withOrigin(-offsetL, -offsetT) {
      surface.drawPath(fg, path)
    }
  }

  def topLeft: Vec = {
    val bounds = path.getBounds()
    Vec(bounds._left, bounds._top)
  }

  def diagonal: Vec = {
    val bounds = path.getBounds()
    Vec(bounds._right - bounds._left + fg.strokeWidth, bounds._bottom - bounds._top + fg.strokeWidth)
  }

  def fillMode: PathFillMode = path.getFillMode

  def fillMode(mode: PathFillMode) = path.setFillMode(mode)

  def moveTo(x: Scalar, y: Scalar): PathShape = {
    path.moveTo(x, y)
    this
  }

  def moveTo(pos:(Scalar, Scalar)): PathShape = {
    path.moveTo(pos._1, pos._2)
    this
  }

  def lineTo(x: Scalar, y: Scalar): PathShape = {
    path.lineTo(x, y)
    this
  }

  def lineTo(pos:(Scalar, Scalar)): PathShape = {
    path.lineTo(pos._1, pos._2)
    this
  }

  def closePath: PathShape = {
    path.closePath()
    this
  }

  def addRect(x: Scalar, y: Scalar, w: Scalar, h: Scalar): PathShape = {
    path.addRect(Rect.makeXYWH(x, y, w, h)); this
  }

  def addOval(x: Scalar, y: Scalar, w: Scalar, h: Scalar): PathShape = {
    path.addOval(Rect.makeXYWH(x, y, w, h)); this
  }

  def addOval(pos: (Scalar, Scalar), dim: (Scalar,Scalar)): PathShape = {
    path.addOval(Rect.makeXYWH(pos._1, pos._2, dim._1, dim._2)); this
  }

  def addCircle(x: Scalar, y: Scalar, r: Scalar): PathShape = {
    path.addCircle(x, y, r); this
  }

  def addCircle(pos: (Scalar, Scalar), r: Scalar): PathShape = {
    path.addCircle(pos._1, pos._2, r); this
  }

  def addPathShape(shape: PathShape, x: Scalar, y: Scalar): PathShape = {
    path.addPath(shape.path, x-shape.w/2, y-shape.h/2)
    this
  }

  def addPathShape(shape: PathShape, pos: (Scalar, Scalar)): PathShape = {
    path.addPath(shape.path, pos._1-shape.w/2, pos._2-shape.h/2)
    this
  }

  override def toString: String = s"PathShape($fg)(${path.getVerbs.toSeq.mkString(",")})"

  /*override def enclosing(point: Vec): Option[Shape] = {
    val r = if (path.contains(point.x, point.y)) Some(this) else None
    println(s"\n$this\n .enclosing($point)\n =$r")
    r
  }

   */

}

/**
 * An `Animateable` glyph shape that regenerates whenever its `current` value is set to a value that differs from its
 * `current: T`.
 */
abstract class AnimatedShape[T](initial: T, val diagonal: Vec) extends Shape with Animateable[T] {
  /** Generate the shape corresponding to `t` */
  def toShape(t: T): Shape
  /** Invoked by `set` after shape generation */
  def afterShape(): Unit = {}

  protected var currentShape: Shape = toShape(initial)
  protected var current: T               = initial

  def center: Vec   = diagonal * 0.5f

  /**
   * Set the current state, and show its `toShape`.
   */
  def set(state: T): Unit =  {
    if (state != current) { currentShape = toShape(state); afterShape() }
    current = state
  }

  /** Get the current state. */
  def get: T = current

  override def draw(surface: Surface): Unit = {
    surface.withClip(diagonal) { currentShape.draw(surface) }
  }
}








