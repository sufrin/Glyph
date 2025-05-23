package org.sufrin.glyph

import GlyphTypes.Scalar

import io.github.humbleui.skija.{PaintMode, Path, PathFillMode}
import io.github.humbleui.types.Rect
import org.sufrin.glyph.Brush.{ROUND, SQUARE}
import org.sufrin.glyph.Brushes.{black, green, invisible, lightGrey}
import org.sufrin.glyph.GlyphShape.{asGlyph, circle, rect, superimposed, FILL, STROKE}

/**
 *  Lightweight precursor to `Glyph`. This type arrived in Glyph 0.9 and may eventually
 *  be the basis for simplifying the construction of some heavier-weight passive glyphs.
 *
 *  Atomic glyph shapes are coloured by a brush, and they are filled (or not) depending
 *  on the `mode` of the brush: `FILL` or `STROKE`.
 */
trait GlyphShape { thisShape =>
  def draw(surface: Surface): Unit            // draw on the given surface
  def diagonal:               Vec             // a bounding box
  def withForeground(brush: Brush): GlyphShape // a copy, with a new foreground brush
  @inline def w:      Scalar  = diagonal.x
  @inline def h:      Scalar  = diagonal.y
  @inline def centre: Vec     = diagonal * 0.5f

  /** Is the given point within the shape: default is within the bounding box */
  @inline def encloses(point: Vec): Boolean = enclosing(point).nonEmpty

  /** The "nearest" enclosing shape */
  def enclosing(point: Vec): Option[GlyphShape] =
    if (0<=point.x && point.x<=w && 0<=point.y && point.y<=h) Some(thisShape) else None

  /**
   * This drawable scaled uniformly by `factor`. Implemented
   * efficiently by a proxy class to avoid space leaks.
   *
   * @see turn
   */
  def scale(factor: Scalar): GlyphShape = if (factor==1) thisShape else new Scaled(thisShape, factor)

  private class Scaled(original: GlyphShape, factor: Scalar) extends GlyphShape {
    def draw(surface: Surface): Unit = surface.withScale(factor) { original.draw(surface) }
    def diagonal: Vec = original.diagonal * (factor)
    override def toString: String = s"$thisShape.scale(${factor})"
    override def enclosing(point: Vec): Option[GlyphShape] = thisShape.enclosing(point/factor)
    override def scale(factor: Scalar): GlyphShape = if (factor==1) original else new Scaled(original, this.factor*factor)
    def withForeground(brush: Brush): GlyphShape = original.withForeground(brush).scale(factor)
  }

  /**
   * This drawable rotated clockwise about its centre through `degrees`. Implemented 
   * efficiently by a proxy class to avoid space leaks.
   *
   * The point is that if implemented by the "normal" method of transforming glyphs (ie by generating a glyph that draws the "host" glyph
   * with an additional transform), the glyph resulting from a sequence of `turn` contains a sequence of transformed glyphs that embody
   * the entire sequence. The method here just aggregates successive rotations.
   */
  def turn(degrees: Scalar, tight: Boolean=false): GlyphShape = if (degrees==0f) thisShape else new Turned(thisShape, degrees, tight)

  private class Turned(shape: GlyphShape, degrees: Scalar, tight: Boolean) extends GlyphTransforms.Turned(shape, degrees, tight, invisible, invisible) {
    override def turn(degrees: Scalar, tight: Boolean=true): GlyphShape =
      if (degrees==0f) shape else new Turned(shape, this.degrees+degrees, tight)

    override def originals: Seq[GlyphShape] = List(shape)

    override def withForeground(brush: Brush): GlyphShape = shape.withForeground(brush).turn(degrees, tight)

  }

  def originals: Seq[GlyphShape] = List(thisShape)

//  private class XTurned(original: GlyphShape, degrees: Scalar, tight: Boolean) extends GlyphShape {
//    override def toString: String = s"$original.turn($degrees)"
//
//    override def turn(degrees: Scalar, tight: Boolean=true): GlyphShape =
//      if (degrees==0f) original else new Turned(original, this.degrees+degrees, tight)
//
//    // Bounding box of the transformed shape -- pessimistic (see `GlyphTransforms.Turned`)
//    def diagonal: Vec = {
//      val side = original.diagonal.x max original.diagonal.y
//      Vec(side, side)
//    }
//
//    // Centre of the original shape's bounding box
//    private val originalCentre = original.diagonal scaled 0.5f
//
//    // Distance of the new centre from the host centre
//    private val delta = (diagonal scaled 0.5f) - originalCentre
//
//    def draw(surface: Surface): Unit =
//      surface.withOrigin(delta) {
//        surface.withRot(degrees, originalCentre) {
//          original.draw(surface)
//        }
//      }
//
//    private val thisCentre: Vec = diagonal scaled 0.5f
//
//    @inline private def relativeLocation(glyphPos: Vec): Vec = {
//      import Math.{cos, sin, PI}
//      val theta = degrees * (PI / 180)
//      val cosTheta = cos(theta).toFloat
//      val sinTheta = sin(theta).toFloat
//      val Vec(x, y) = glyphPos - thisCentre  // vector to the centre of this glyph
//      val xr = x*cosTheta + y*sinTheta       // rotated by theta
//      val yr = y*cosTheta - x*sinTheta
//      originalCentre+Vec(xr, yr)
//    }
//
//    override def enclosing(p: Vec): Option[GlyphShape] = {
//      val res = original.enclosing(relativeLocation(p))
//      res
//    }
//
//  }


  /** left '''beside''' right */
  def ||| (thatShape: GlyphShape): GlyphShape = new GlyphShape {
    val dThis = Vec(0,  (h - thisShape.h)*0.5f)
    val dThat = Vec(thisShape.w, (h - thatShape.h)*0.5f)
    def draw(surface: Surface): Unit = {
      surface.withOrigin(dThis) { thisShape.draw(surface) }
      surface.withOrigin(dThat) { thatShape.draw(surface) }
    }

    def diagonal: Vec = Vec(thisShape.w+thatShape.w, thisShape.h max thatShape.h)

    override def enclosing(point: Vec): Option[GlyphShape] =
      thisShape.enclosing(point-dThis) orElse thatShape.enclosing(point-dThat)

    override def toString: String = s"$thisShape|||$thatShape"

    override def originals: Seq[GlyphShape] = List(thisShape, thatShape)

    override def scale(factor: Scalar): GlyphShape = if (factor==1) this else thisShape ||| thatShape

    def withForeground(brush: Brush): GlyphShape =  this


  }

  /** left '''above''' right */
  def --- (thatShape: GlyphShape): GlyphShape = new GlyphShape {
     val dThis = Vec((w-thisShape.w)*0.5f, 0)
     val dThat = Vec((w-thatShape.w)*0.5f, thisShape.h)

    def draw(surface: Surface): Unit = {
      surface.withOrigin(dThis) { thisShape.draw(surface) }
      surface.withOrigin(dThat) { thatShape.draw(surface) }
    }

    def diagonal: Vec = Vec(thisShape.w max thatShape.w, thisShape.h + thatShape.h)

    override def enclosing(point: Vec): Option[GlyphShape] =
      thisShape.enclosing(point-dThis) orElse thatShape.enclosing(point-dThat)

    override def toString: String = s"$thisShape---$thatShape"

    override def originals: Seq[GlyphShape] = List(thisShape, thatShape)

    override def scale(factor: Scalar): GlyphShape = if (factor==1) this else thisShape ||| thatShape

    def withForeground(brush: Brush): GlyphShape =  this


  }

  /** If same areas, ''left'' '''superimposed on''' ''right'',
   *
   *  otherwise ''smaller-area'' '''superimposed on''' ''larger-area''
   */
  def ~~~ (thatShape: GlyphShape): GlyphShape = {
    val (big, small) = if (thisShape.w*thisShape.h <= thatShape.w*thatShape.h) (thatShape, thisShape) else (thisShape, thatShape)
    superimposed(big, small)
  }

  /**
   *  A `LocatedShape` shaped like `thisShape`, initially drawn at `(x,y)`.
   *
   */
  def locatedAt(x: Scalar, y: Scalar): LocatedShape = LocatedShape(x, y, thisShape)

  /**
   *  A `TargetShape` shaped like `thisShape`, initially drawn at `(x,y)`.
   *
   * @see TargetShape
   */
  def targetLocatedAt(x: Scalar, y: Scalar): TargetShape = TargetShape(x, y, thisShape)

  def targetLocatedAt(pos: (Scalar, Scalar)): TargetShape = TargetShape(pos._1, pos._2, thisShape)


  /**
   * This shape, with a background (as far as possible of the same shape) coloured by `brush`.
   */
  def bg(brush: Brush): GlyphShape = thisShape ~~~ GlyphShape.rect(thisShape.w, thisShape.h)(brush)

  /**
   * This shape, with a foreground (as far as possible of the same shape) coloured by `brush`.
   */
  def apply(brush: Brush): GlyphShape = thisShape
}



object GlyphShape {
  /**
   * A circle of radius `r`, occupying a square of side `2r+fg.strokeWidth`
   * Unless `fg.mode=PaintMode.STROKE` the circle is filled.
   */
  def circle(r: Scalar)(fg: Brush): GlyphShape = new GlyphShape {
    val delta = Vec(fg.strokeWidth / 2, fg.strokeWidth / 2)

    def draw(surface: Surface): Unit = surface.withOrigin(delta) {
      surface.drawOval(fg, Vec.Zero, diag)
    }

    val diag: Vec = Vec(2 * r, 2 * r)
    val diagonal: Vec = diag + (delta * 2)
    val middle = diagonal * 0.5f

    override def toString: String = s"circle($r)($fg)"

    override def enclosing(point: Vec): Option[GlyphShape] = {
      val res = if (point.distanceWithin(middle, r)) Some(this) else None
      res
    }


    /**
     * This circle superimposed on a circle of the same radius filled with `brush`
     */
    override def bg(brush: Brush): GlyphShape = circle(r)(brush(mode = PaintMode.FILL)) ~~~ this

    override def apply(brush: Brush): GlyphShape = circle(r)(brush)

    override def scale(factor: Scalar): GlyphShape = if (factor == 1) this else circle(factor * r)(fg)

    def withForeground(brush: Brush): GlyphShape = circle(r)(brush)

  }

  /**
   * An oval bounded by a `(width x height)` rectangle, occupying a `(width+delta x height+delta)` rectangle
   * (where delta=`fg.strokeWidth`).
   * Unless `fg.mode=PaintMode.STROKE` the oval is filled.
   */
  def oval(width: Scalar, height: Scalar)(fg: Brush): GlyphShape = new GlyphShape {
    val delta = Vec(fg.strokeWidth / 2, fg.strokeWidth / 2)

    def draw(surface: Surface): Unit = surface.withOrigin(delta) {
      surface.drawOval(fg, Vec.Zero, diag)
    }

    val diag: Vec = Vec(width, height)
    val diagonal: Vec = diag + (delta * 2)

    override def toString: String = s"oval($width,$height)($fg)"


    /**
     * This oval superimposed on an oval of the same dimension filled with `brush`
     */
    override def bg(brush: Brush): GlyphShape = oval(w, h)(brush(mode = PaintMode.FILL)) ~~~ this

    override def apply(brush: Brush): GlyphShape = oval(w, h)(brush)

    override def scale(factor: Scalar): GlyphShape = if (factor == 1) this else oval(factor * width, factor * height)(fg)

    def withForeground(brush: Brush): GlyphShape = oval(width, height)(brush)
  }

  /**
   * An arc/sector of a (width x height) oval occupying a `(width+delta x height+delta)` rectangle. The
   * path of the arc starts in direction `startAngle` and extends for `sweepAngle`. The centre is included in
   * the path if `incCentre` is true.
   */
  def arc(width: Scalar, height: Scalar, startAngle: Scalar, sweepAngle: Scalar, incCentre: Boolean)(fg: Brush): GlyphShape = new GlyphShape {
    val delta = Vec(fg.strokeWidth / 2, fg.strokeWidth / 2)

    def draw(surface: Surface): Unit = surface.withOrigin(delta) {
      surface.canvas.drawArc(0, 0, width, height, startAngle, sweepAngle, incCentre, fg)
    }

    val diag: Vec = Vec(width, height)
    val diagonal: Vec = diag + (delta * 2)

    override def toString: String = s"arc($width,$height,$startAngle,$sweepAngle,$incCentre)($fg)"

    def withForeground(brush: Brush): GlyphShape = arc(width, height, startAngle, sweepAngle, incCentre)(brush)


    /**
     * This arc superimposed on an arc with the same characteristics filled with `brush`
     */
    override def bg(brush: Brush): GlyphShape = arc(width, height, startAngle, sweepAngle, incCentre)(brush(mode = PaintMode.FILL)) ~~~ this

    override def apply(brush: Brush): GlyphShape = arc(width, height, startAngle, sweepAngle, incCentre)(brush)

    override def scale(factor: Scalar): GlyphShape = if (factor == 1) this else oval(factor * width, factor * height)(fg)

  }

  /**
   * A circular pie with the given radius, with slices painted by brushes.
   * {{{ requires brushes.length>0 }}}
   */
  def pie(radius: Scalar)(brushes: Brush*): GlyphShape = {
    require(brushes.length > 0, "A pie must have a positive number of brushes")
    val sector = 360.0f / brushes.length
    var rot = 0.0f
    superimposed(
      brushes.map { brush => rot += sector; arc(2 * radius, 2 * radius, rot, sector, true)(brush) }
    )
  }


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
  def line(start: Vec, end: Vec)(fg: Brush): GlyphShape = new GlyphShape {
    def draw(surface: Surface): Unit =  surface.drawLines(fg, List(start, end))
    def diagonal: Vec = Vec.Zero
    override def toString: String = s"line($start,$end)($fg)"
    def withForeground(brush: Brush): GlyphShape = line(start, end)(brush)
  }

  /**
   * @see line
   */
  def line(startx: Scalar, starty: Scalar, endx: Scalar, endy: Scalar)(fg: Brush): GlyphShape = new GlyphShape {
    def draw(surface: Surface): Unit = surface.drawLines$(fg, startx, starty, endx, endy)
    def diagonal: Vec = Vec.Zero
    override def toString: String = s"line(($startx,$starty), ($endx,$endy))($fg)"
    def withForeground(brush: Brush): GlyphShape = line(startx, starty, endx, endy)(brush)

  }

  def lineBetween(l: LocatedShape, r: LocatedShape)(fg: Brush): GlyphShape = new GlyphShape {
      val diagonal: Vec = Vec.Zero

      def draw(surface: Surface): Unit =
        surface.drawLines$(fg, l.x, l.y, r.x, r.y)

      override def toString: String = s"lineBetween($l,$r)($fg)"

      def withForeground(brush: Brush): GlyphShape = lineBetween(l, r)(brush)
  }

  def arrow(brush: Brush): GlyphShape = {
    val f = 4f
    val a = f*3.5f
    val b = 3f *a
    val c = f* 20f
    val d = f * 30f
    val fg = if (brush.cap == SQUARE) brush else brush(cap = SQUARE)
    polygon((0, a), (0, b), (c, b), (c, a + b), (d, (a + b) / 2), (c, 0), (c, a))(fg) //~~~rect(d,a+b)(black(mode=STROKE))
  }

  /**
   * A `(width, height)` rectangle, occupying a `(width+fg.strokeWidth, height+fg.strokeWidth)` rectangle.
   * Unless `fg.mode=PaintMode.STROKE` the rectangle is filled.
   */
  def rect(width: Scalar, height: Scalar)(fg: Brush): GlyphShape = new GlyphShape {
    def withForeground(brush: Brush): GlyphShape = rect(width, height)(brush)

    val delta = Vec(fg.strokeWidth / 2, fg.strokeWidth / 2)

    def draw(surface: Surface): Unit = surface.withOrigin(delta) {
      if (fg.mode != STROKE) surface.fillRect(fg, diag)
      surface.drawRect(fg, diag)
    }

    val diag: Vec = Vec(width, height)
    val diagonal: Vec = diag + (delta * 2)

    override def toString: String = s"rect($width,$height)($fg)"


    /**
     * This rectangle superimposed on a rectangle with the same dimensions filled with `brush`
     */
    override def bg(brush: Brush): GlyphShape = this ~~~ rect(w, h)(brush(mode = FILL))

    override def apply(brush: Brush): GlyphShape = rect(width, height)(brush)

    override def scale(factor: Scalar): GlyphShape = if (factor ==1) this else rect(factor * width, factor * height)(fg)

  }

  /**
   * A closed polygon made of `vertex::vertices`, and with the natural bounding box.
   */
  def polygon(vertex: (Scalar, Scalar), vertices: (Scalar, Scalar)*): Brush => GlyphShape = polygon(vertex :: vertices.toList)

  /**
   * A closed polygon made of `vertices`, and with the natural bounding box.
   */
  def polygon(vertices: Seq[Vec]): Brush =>GlyphShape = polygon(vertices.map {v =>(v.x, v. y)})

  /**
   * A closed polygon made of the given `vertices`, and with the natural bounding box.
   */
  def polygon(vertices: Iterable[(Scalar, Scalar)])(fg: Brush): GlyphShape = new GlyphShape {
    val kind: String = "Polygon"

    override def toString: String = s"Polygon(...)[$diagonal](fg=$fg)"

    def withForeground(brush: Brush): GlyphShape =  polygon(vertices)(brush)

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

    val diagonal = {
      val bounds = path.getBounds()
      path.updateBoundsCache()
      offsetL = bounds._left - fg.strokeWidth / 2
      offsetT = bounds._top - fg.strokeWidth / 2
      Vec(bounds._right - bounds._left + fg.strokeWidth, bounds._bottom - bounds._top + fg.strokeWidth)
    }

    override def enclosing(point: Vec): Option[GlyphShape] = {
      val origin = point + (offsetL, offsetT)
      if (path.contains(origin.x, origin.y)) Some(this) else None
    }

    def draw(surface: Surface): Unit = surface.withOrigin(-offsetL, -offsetT) {
      surface.drawPath(fg, path)
    }


    /**
     * This polygon superimposed on a polygon with the same vertices filled with `brush`
     */
    override def bg(brush: Brush): GlyphShape = this ~~~ polygon(vertices)(brush(mode = FILL))

    override def scale(factor: Scalar): GlyphShape = if (factor ==1) this else {
      polygon(vertices.map { case ( x,y) => (factor*x, factor * y)})(fg)
    }


  }

  /**
   *
   * Experimental mutable shape. If `absolute` then
   * the path is drawn "as is", otherwise it is drawn
   * so that the top/left coordinate of the path is at the origin.
   * Its diagonal always denotes the distance from the top left to the
   * bottom right coordinate of the path.
   */
  class PathShape(fg: Brush, absolute: Boolean = true) extends GlyphShape {
    val path = new Path
    locally {
      if (absolute) path.moveTo(0, 0)
    }

    def withForeground(brush: Brush): GlyphShape =  this

    def reset(): Unit = {
      path.reset()
      if (absolute) path.moveTo(0, 0)         
    }

    def draw(surface: Surface): Unit = if (absolute) {
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

    /*override def enclosing(point: Vec): Option[GlyphShape] = {
      val r = if (path.contains(point.x, point.y)) Some(this) else None
      println(s"\n$this\n .enclosing($point)\n =$r")
      r
    }

     */

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
    val deltas = for {shape <- shapes} yield (Vec((tw - shape.w) / 2, (th - shape.h) / 2))

    def draw(surface: Surface): Unit = {
      val shape = shapes.iterator
      for {delta <- deltas}
        surface.withOrigin(delta) {
          shape.next().draw(surface)
        }
    }

    override def originals: Seq[GlyphShape] = shapes

    def diagonal: Vec = Vec(tw, th)

    override def enclosing(point: Vec): Option[GlyphShape] = {
      var it: Option[GlyphShape] = None
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

    override def scale(factor: Scalar): GlyphShape = if (factor==1) this else superimposed(shapes.map(_.scale(factor)))

    def withForeground(brush: Brush): GlyphShape =  this

  }

  /** @see superimposed */
  def superimposed(shape: GlyphShape, shapes: GlyphShape*): GlyphShape = superimposed(shape :: shapes.toList)


  /** Superposition of located shapes */
  def superimposed(shape: LocatedShape, shapes: LocatedShape*): GlyphShape = superimposed(shape :: shapes.toList)

  /** Superposition of located shapes */
  def superimposed(shapes: Iterable[LocatedShape]): GlyphShape = new GlyphShape {
    def withForeground(brush: Brush): GlyphShape =  this
    val tw = Measure.maxWidth(shapes.map(_.shape))
    val th = Measure.maxHeight(shapes.map(_.shape))
    val deltas = for {shape <- shapes} yield (Vec((tw - shape.w) / 2, (th - shape.h) / 2))

    def draw(surface: Surface): Unit = {
      val shape = shapes.iterator
      for {delta <- deltas}
        surface.withOrigin(delta) {
          shape.next().draw(surface)
        }
    }

    def diagonal: Vec = Vec(tw, th)

    override def enclosing(point: Vec): Option[GlyphShape] = {
      var it: Option[GlyphShape] = None
      val delta = deltas.iterator
      val shape = shapes.iterator
      //println(s"$this.enclosing($point)")
      while (it.isEmpty && delta.hasNext) {
        val s = shape.next()
        val d = delta.next()
        //println(s"  $s@${point-d}")
        it = s.shape.enclosing(point - d)
      }
      it
    }

    override def toString: String = s"(${shapes.mkString("~~~")})"

  }


  /**
   * A standard glyph drawn as the given `shape`.
   */
  implicit def asGlyph(shape: GlyphShape): Glyph = new Glyph {
    wrapped =>
    def copy(fg: Brush, bg: Brush): Glyph = wrapped.copy(fg, bg)

    def draw(surface: Surface): Unit = shape.draw(surface)

    val diagonal: Vec = shape.diagonal
    val fg: Brush = Brushes.transparent
    val bg: Brush = Brushes.transparent
  }

  val STROKE: PaintMode = PaintMode.STROKE
  val FILL: PaintMode = PaintMode.FILL
  val STROKE_AND_FILL: PaintMode = PaintMode.STROKE_AND_FILL


  def cardinalPoints(shape: GlyphShape): Seq[Vec] = {
    import shape.{w, h}
    Array(Vec(w, h / 2), Vec(w, h), Vec(w / 2, h), Vec(0, h), Vec(0, h / 2), Vec(0, 0), Vec(w / 2, 0), Vec(w, 0)).toSeq
  }


  def fromGlyph(glyph: Glyph): GlyphShape = new GlyphShape {
    def draw(surface: Surface): Unit = glyph.draw(surface)
    def diagonal: Vec = glyph.diagonal
    def withForeground(brush: Brush): GlyphShape = glyph.copy(brush)
  }

}

case class Handle(thing: TargetShape, shape: GlyphShape, origin: Vec) {
  def draw(surface: Surface): Unit = { surface.withOrigin(origin) { shape.draw(surface) } }
  def canHandle(point: Vec): Boolean = {
    val br = origin+shape.diagonal
    origin.x<=point.x && point.x<=br.x && origin.y<=point.y && point.y<=br.y
  }
}

case class Handles(thing: TargetShape, handles: Seq[Handle]) {
  def draw(surface: Surface): Unit = for { handle<-handles } handle.draw(surface)
  def canHandle(point: Vec): Seq[Handle] =
    for { handle<-handles if handle.canHandle(point) } yield handle
}

/**
 * A variably-located and rotated shape currently located at `(x,y)`.
 * Shown with a "handle" whose appearance can be changed when hovering or selected.
 */
case class TargetShape(x$: Scalar, y$: Scalar, shape$: GlyphShape) extends LocatedShape(x$, y$, shape$) {

  val handleBrush0 = Brushes.yellow(width = 2f, mode = STROKE, cap = ROUND, alpha = 0.4f)
  val handleBrush = handleBrush0.copy()
  val hoverBrush0 = Brushes.white(width = 1, mode = STROKE, cap = ROUND, alpha = 0.7f)
  val hoverBrush = hoverBrush0.copy()
  val radius = ((w max h) / 12) min 5

  private val handle = rect(radius, radius)(handleBrush)
  val deltaHandle = handle.diagonal * 0.5f
  val handles = Handles(this, for { loc <- GlyphShape.cardinalPoints(this.shape) } yield Handle(this, handle, loc-deltaHandle))

  val hover =  circle(radius)(hoverBrush)

  @inline private def centred(g: GlyphShape): Vec = Vec(x, y) + ((diagonal - g.diagonal) * 0.5f)

  def setHovering(state: Boolean): Unit = {
    if (state) {
      hoverBrush.strokeWidth(hoverBrush0.strokeWidth * 2)
      hoverBrush.alpha(1.0)
      handleBrush.alpha(1f)
      handleBrush.strokeWidth(2*handleBrush0.strokeWidth)
    } else {
      hoverBrush.strokeWidth(hoverBrush0.strokeWidth)
      hoverBrush.alpha(hoverBrush0.alpha)
      handleBrush.alpha(handleBrush0.alpha)
      handleBrush.strokeWidth(handleBrush0.strokeWidth)
    }
  }

  def setSelected(state: Boolean): Unit = {
    if (state) {
      handleBrush.mode(FILL)
      handleBrush.alpha(1.0)
    } else {
      handleBrush.mode(STROKE)
      handleBrush.alpha(hoverBrush0.alpha)
    }
  }

  /** one of the glyph's handle(s) contains the mouse pointer */
  def canHandle(point: Vec): Boolean = handles.canHandle(point - (x,y)).nonEmpty

  /** Draw the shape and its auxiliary shapes */
  override def draw(surface: Surface): Unit = {
    surface.withOrigin(x, y) {
      shape.draw(surface)
      handles.draw(surface)
    }
    surface.withOrigin(centred(hover)) { hover.draw(surface) }
  }

  override def copyState: TargetShape = TargetShape(x, y, shape)

  override def toString: String = s"$shape.variable${(x, y)}"

}

class LocatedShape(var x: Scalar, var y: Scalar, val shape: GlyphShape) {
  def w: Scalar = shape.w
  def h: Scalar = shape.h

  @inline def topLeft: Vec = Vec(x, y)
  @inline private def centred(g: GlyphShape): Vec = Vec(x, y) + ((diagonal - g.diagonal) * 0.5f)
  @inline def centre: Vec = topLeft + (diagonal * 0.5f)

  /** the glyph's bounding box contains the mouse pointer */
  def isBeneath(point: Vec): Boolean = shape.encloses((point - (x,y)))

  /** Draw the shape and its auxiliary shapes */
  def draw(surface: Surface): Unit = {
    surface.withOrigin(x, y) {
      shape.draw(surface)
    }
  }

  def copyState: LocatedShape = LocatedShape(x, y, shape)

  def diagonal: Vec = shape.diagonal

  override def toString: String = s"$shape.variable${(x, y)}"

  def isIn(set: Seq[LocatedShape]): Boolean = set contains this

  def notIn(set: Seq[LocatedShape]): Boolean = !(set contains this)

  def moveTo(x: Scalar, y: Scalar): Unit = {
    this.x = x; this.y = y
  }

  def placeAt(x: Scalar, y: Scalar): Unit = {
    this.x = x; this.y = y
  }

  def placeAt(loc: Vec): Unit = {
    this.x = loc.x; this.y = loc.y
  }

  def moveBy(x: Scalar, y: Scalar): Unit = {
    this.x += x; this.y += y
  }

  def center: Vec = (diagonal * 0.5f) + (x, y)

}

object LocatedShape {
  def apply(x: Scalar, y: Scalar, shape: GlyphShape): LocatedShape = new LocatedShape(x, y, shape)
}
