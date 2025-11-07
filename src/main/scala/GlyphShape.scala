package org.sufrin.glyph

import Brush.ROUND
import Brushes.red
import GlyphShape.{circle, composite, rect, superimposed, FILL, STROKE}
import GlyphTypes.{Font, Scalar}
import unstyled.dynamic.Animateable

import io.github.humbleui.skija.{PaintMode, Path, PathFillMode}
import io.github.humbleui.types.Rect

/**
 *  Lightweight precursor to `Glyph`. This type arrived in Glyph 0.9 and may eventually
 *  be the basis for simplifying the construction of some heavier-weight passive glyphs.
 *
 *  Atomic glyph shapes are coloured by a brush, and they are filled (or not) depending
 *  on the `mode` of the brush: `FILL` or `STROKE`.
 */
trait GlyphShape { thisShape =>
  def draw(surface: Surface): Unit                                     // draw on the given surface
  def diagonal:               Vec                                      // a bounding box
  def withBrushes(fg: Brush=fg, bg: Brush=bg): GlyphShape              // a copy -- with new brushes when that means anything (it doesn't for composites)
  def withBackground(bg: Brush): GlyphShape = superimposed(List(rect(w, h)(bg), this))  // this shape, with a rectangular background coloured bg
  def cardinalPoints: Seq[Vec] = Seq.empty                             // places for handles

  @inline def w:      Scalar  = diagonal.x
  @inline def h:      Scalar  = diagonal.y
  @inline def centre: Vec     = diagonal * 0.5f

  val fg: Brush = Brushes.transparent
  val bg: Brush = null

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
    override def withBrushes(fg: Brush=fg, bg: Brush=bg): GlyphShape = original.withBrushes(fg, bg).scale(factor)
  }

  /**
   * This drawable rotated clockwise about its centre through `degrees`. Implemented
   * efficiently by a proxy class to avoid space leaks.
   *
   * The point is that if implemented by the "normal" method of transforming glyphs (ie by generating a glyph that draws the "host" glyph
   * with an additional transform), the glyph resulting from a sequence of `turn` contains a sequence of transformed glyphs that embody
   * the entire sequence. The method here just aggregates successive rotations.
   */
  def turn(degrees: Scalar, tight: Boolean=false): GlyphShape =
      if (degrees==0f) thisShape else new Turned(thisShape, degrees, tight)

  private class Turned(original: GlyphShape, degrees: Scalar, tight: Boolean) extends GlyphShape {
    override def turn(degrees: Scalar, tight: Boolean=true): GlyphShape =
      if (degrees==0f) original else new Turned(original, this.degrees+degrees, tight)

    override val fg: Brush = original.fg
    override val bg: Brush = original.bg


    override def withBrushes(fg: Brush=fg, bg: Brush=bg): GlyphShape = original.withBrushes(fg, bg).turn(degrees, tight)

    private val Theta = Math.toRadians(((degrees % 360) + 360) % 360)
    private val d = original.diagonal

    @inline private def cos(theta: Double): Scalar = Math.cos(theta).toFloat
    @inline private def sin(theta: Double): Scalar = Math.sin(theta).toFloat
    @inline private def oddQuadrant(theta: Double)  = Vec((d.x * cos(theta) + d.y * sin(theta)).abs, (d.x * sin(theta) + d.y * cos(theta)).abs)
    @inline private def evenQuadrant(theta: Double) = Vec((d.x * sin(theta) + d.y * cos(theta)).abs, (d.x * cos(theta) + d.y * sin(theta)).abs)

    def diagonal: Vec = if (tight) {
      val D =  d.x max d.y
      Vec(D,D)
    } else {
      val quadrant = (((degrees % 360) + 360) % 360 / 90).toInt + 1
      quadrant match {
        case 1 => oddQuadrant(Theta)
        case 2 => evenQuadrant(Theta - Math.PI/2)
        case 3 => oddQuadrant(Theta  - Math.PI)
        case 4 => evenQuadrant(Theta - Math.PI/2 - Math.PI)
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

    override def enclosing(p: Vec): Option[GlyphShape] = {
      val res = original.enclosing(relativeLocation(p))
      res
    }

  }



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

    override def scale(factor: Scalar): GlyphShape = if (factor==1) this else thisShape ||| thatShape

    override def withBrushes(fg: Brush=fg, bg: Brush=bg): GlyphShape = thisShape.withBrushes(thisShape.fg, bg) ||| thatShape.withBrushes(thatShape.fg, bg)

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

    override def scale(factor: Scalar): GlyphShape = if (factor==1) this else thisShape --- thatShape

    override def withBrushes(fg: Brush=fg, bg: Brush=bg): GlyphShape = thisShape.withBrushes(thisShape.fg, bg) --- thatShape.withBrushes(thatShape.fg, bg)


  }

  /** If same areas, ''left'' '''superimposed on''' ''right'',
   *
   *  otherwise ''smaller-area'' '''superimposed on''' ''larger-area''
   */
  def ~~~ (thatShape: GlyphShape): GlyphShape = {
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


object GlyphShape {
  /**
   * A circle of radius `r`, occupying a square of side `2r+fg.strokeWidth`
   * Unless `fg.mode=PaintMode.STROKE` the circle is filled.
   */
  def circle(r: Scalar)(brush: Brush): GlyphShape = new GlyphShape {
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

    override def enclosing(point: Vec): Option[GlyphShape] = {
      val res = if (point.distanceWithin(middle, r)) Some(this) else None
      res
    }

    override def scale(factor: Scalar): GlyphShape = if (factor == 1) this else circle(factor * r)(fg)

    override def withBrushes(fg: Brush=fg, bg: Brush=bg): GlyphShape =
      if (bg==null) circle(r)(fg) else circle(r)(bg) ~~~ circle(r)(fg)

  }

  /**
   * An oval bounded by a `(width x height)` rectangle, occupying a `(width+delta x height+delta)` rectangle
   * (where delta=`brush.strokeWidth`).
   */
  def oval(width: Scalar, height: Scalar)(brush: Brush): GlyphShape = new GlyphShape {
    override val fg: Brush = brush
    val delta = Vec(fg.strokeWidth / 2, fg.strokeWidth / 2)

    def draw(surface: Surface): Unit = surface.withOrigin(delta) {
      surface.drawOval(fg, Vec.Zero, diag)
    }

    val diag: Vec = Vec(width, height)
    val diagonal: Vec = diag + (delta * 2)

    override def toString: String = s"oval($width,$height)($fg)"


    override def scale(factor: Scalar): GlyphShape = if (factor == 1) this else oval(factor * width, factor * height)(fg)

    //override def rotationallySymmetric: Boolean = .6 < Math.abs(w-h)/(w max h)

    override def withBrushes(fg: Brush=fg, bg: Brush=bg): GlyphShape =
      if (bg==null) oval(width, height)(fg) else oval(width, height)(fg) ~~~ oval(width, height)(fg)
  }

  /**
   * An arc/sector of a (width x height) oval occupying a `(width+delta x height+delta)` rectangle. The
   * path of the arc starts in direction `startAngle` and extends for `sweepAngle`. The centre is included in
   * the path if `incCentre` is true.
   */
  def arc(width: Scalar, height: Scalar, startAngle: Scalar, sweepAngle: Scalar, incCentre: Boolean)(brush: Brush): GlyphShape = new GlyphShape {
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
    override def withBrushes(fg: Brush, bg: Brush=null): GlyphShape = {
      val thisArc = arc(width, height, startAngle, sweepAngle, incCentre)(_)
      if (bg==null) thisArc(fg) else (thisArc(bg(mode = PaintMode.FILL)) ~~~ thisArc(fg))
    }

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

  def text(text: String, font: Font=fallback.textFont)(fg: Brush=fallback.textForeground, bg: Brush=fallback.textBackground): GlyphShape =
      unstyled.Text(text, font, fg, bg).asInstanceOf[GlyphShape]

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
  def line(start: Vec, end: Vec)(brush: Brush): GlyphShape = new GlyphShape {
    override val fg: Brush = brush
    def draw(surface: Surface): Unit =  surface.drawLines(fg, List(start, end))
    def diagonal: Vec = Vec.Zero
    override def toString: String = s"line($start,$end)($fg)"
    override def withBrushes(fg: Brush=fg, bg: Brush=null): GlyphShape = line(start, end)(fg)
  }

  /**
   * @see line
   */
  def line(startx: Scalar, starty: Scalar, endx: Scalar, endy: Scalar)(brush: Brush): GlyphShape = new GlyphShape {
    override val fg: Brush = brush
    def draw(surface: Surface): Unit = surface.drawLines(fg, startx, starty, endx, endy)
    def diagonal: Vec = Vec.Zero
    override def toString: String = s"line(($startx,$starty), ($endx,$endy))($fg)"
    override def withBrushes(fg: Brush=fg, bg: Brush=null): GlyphShape = line(startx, starty, endx, endy)(fg)
  }

  def lineBetween(l: LocatedShape, r: LocatedShape)(brush: Brush): GlyphShape = new GlyphShape {
      override val fg: Brush = brush
      val diagonal: Vec = Vec.Zero

      def draw(surface: Surface): Unit =
        surface.drawLines(fg, l.x+l.w*0.5f, l.y+l.h*0.5f, r.x+r.w*0.5f, r.y+r.h*0.5f)

      override def toString: String = s"lineBetween($l,$r)($fg)"

      override def withBrushes(fg: Brush, bg: Brush=null): GlyphShape = lineBetween(l, r)(fg)
  }

  def arrow(brush: Brush): GlyphShape = {
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
  def rect(width: Scalar, height: Scalar): Brush => GlyphShape = polygon((0, 0), (width, 0), (width, height), (0, height))


  /**
   * A closed polygon made of `vertex::vertices`.
   */
  def polygon(vertex: (Scalar, Scalar), vertices: (Scalar, Scalar)*): Brush => GlyphShape = polygon(vertex :: vertices.toList)

  /**
   * A closed polygon made of `vertices`.
   */
  def polygon(vertices: Seq[Vec]): Brush =>GlyphShape = polygon(vertices.map {v =>(v.x, v. y)})

  /**
   * A closed polygon made of the given `vertices`. It is "normalized", in that
   * its width is the distance from its leftmost to its rightmost visible point,
   * and its height is the distance from its topmost to its bottommost
   * visible point; and its top,left point appears at the origin.
   *
   */
  def polygon(vertices: Seq[(Scalar, Scalar)])(brush: Brush): GlyphShape = new GlyphShape {
    override val fg: Brush=brush
    val kind: String = "Polygon"

    override def toString: String = s"Polygon(...)[$diagonal](fg=$fg)"

    override def withBrushes(fg: Brush=fg, bg: Brush=bg): GlyphShape =  polygon(vertices)(fg)

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

    override def enclosing(point: Vec): Option[GlyphShape] = {
      val origin = point + (offsetL, offsetT)
      if (path.contains(origin.x, origin.y)) Some(this) else None
    }

    def draw(surface: Surface): Unit = surface.withOrigin(-offsetL, -offsetT) {
      surface.drawPath(fg, path)
    }

    override def scale(factor: Scalar): GlyphShape = if (factor ==1) this else {
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
   * An `Animateable` glyph shape that regenerates whenever its `current` value is set to a value that differs from its
   * `current: T`.
   */
  abstract class AnimatedShape[T](initial: T, val diagonal: Vec) extends GlyphShape with Animateable[T] {
    /** Generate the shape corresponding to `t` */
    def toShape(t: T): GlyphShape
    /** Invoked by `set` after shape generation */
    def afterShape(): Unit = {}

    protected var currentShape: GlyphShape = toShape(initial)
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

  /**
   * Experimental mutable shape. If `absolute` then
   * the path is drawn "as is" -- with all coordinates interpreted relative to the origin, otherwise it is drawn
   * so that the top/left coordinate of the path is at the origin.
   *
   * Its diagonal always denotes the distance from the top left to the
   * bottom right coordinate of the path.
   */
  class PathShape(override val fg: Brush, absolute: Boolean = true) extends GlyphShape {
    hostShape =>
    val path = new Path
    locally {
      if (absolute) path.moveTo(0, 0)
    }

    /** A mutable shape that shares the same path; but is drawn with a possibly-different (foreground) brush */
    override def withBrushes(fg: Brush=fg, bg: Brush=bg): GlyphShape =  {
      val (f, b) = (fg, bg)
      new GlyphShape {
        override val fg: Brush = f
        override val bg: Brush = b
        override def draw(surface: Surface): Unit = hostShape.draw(fg, surface)
        override def diagonal: Vec = hostShape.diagonal
        override def withBrushes(fg: Brush=fg, bg: Brush=bg): GlyphShape = hostShape.withBrushes(fg, bg)
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

    override def withBrushes(fg: Brush=fg, bg: Brush=bg): GlyphShape = superimposed(shapes)

  }

  /** @see superimposed */
  def composite(shape: GlyphShape, shapes: GlyphShape*): GlyphShape = superimposed(shape :: shapes.toList)


  /** Superposition of located shapes */
  def composite(shape: LocatedShape, shapes: LocatedShape*): GlyphShape = composite(shape :: shapes.toList)

  /** Superposition of located shapes */
  def composite(shapes: Iterable[LocatedShape]): GlyphShape = new GlyphShape {
    def withBrushes(brush: Brush): GlyphShape =  this
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

    override def enclosing(point: Vec): Option[GlyphShape] = {
      var it: Option[GlyphShape] = None
      val shape = shapes.iterator
      //println(s"$this.enclosing($point)")
      while (it.isEmpty && shape.hasNext) {
        val s = shape.next()
        //println(s"  $s@${point-d}")
        it = s.shape.enclosing(point - (s.x, s.y))
      }
      it
    }

    override def withBrushes(fg: Brush=fg, bg: Brush=bg): GlyphShape = composite(shapes.map(_.withBrushes(fg, bg)))

    override def toString: String = s"(${shapes.mkString("~~~")})"

  }






  val STROKE: PaintMode = PaintMode.STROKE
  val FILL: PaintMode = PaintMode.FILL
  val STROKE_AND_FILL: PaintMode = PaintMode.STROKE_AND_FILL


  def cardinalPoints(shape: GlyphShape): Seq[Vec] = {
    import shape.{h, w}
    if (shape.cardinalPoints.nonEmpty)
      shape.cardinalPoints
    else
      Array(Vec(w, h / 2), Vec(w, h), Vec(w / 2, h), Vec(0, h), Vec(0, h / 2), Vec(0, 0), Vec(w / 2, 0), Vec(w, 0)).toSeq
  }


  def fromGlyph(glyph: Glyph): GlyphShape = new GlyphShape {
    def draw(surface: Surface): Unit = glyph.draw(surface)
    def diagonal: Vec = glyph.diagonal
    override def withBrushes(fg: Brush=glyph.fg, bg: Brush=glyph.bg): GlyphShape = glyph.copy(fg, bg)
  }

  implicit def toVec(pair: (Scalar, Scalar)): Vec = Vec(pair._1, pair._2)
  implicit def toPair(vec: Vec): (Scalar, Scalar) = (vec.x, vec.y)

}

case class Handle(thing: TargetShape, shape: GlyphShape, origin: Vec) {
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
 * A variably-located and rotated shape currently located at `(x,y)`.
 * Shown with a "handle" whose appearance can be changed when hovering or selected.
 */
case class TargetShape(x$: Scalar, y$: Scalar, shape$: GlyphShape) extends LocatedShape(x$, y$, shape$) {

  val handleBrush0 = Brushes.yellow(width = 1f, mode = STROKE, cap = ROUND, alpha = 0.6f)
  val handleBrush = handleBrush0.copy()
  val centreBrush0 = Brushes.red(width = 1, mode = STROKE, cap = ROUND, alpha = 0.7f)
  val centreBrush = centreBrush0.copy()
  val radius = ((w max h) / 12) min 5

  private val handle = rect(radius, radius)(handleBrush)
  val deltaHandle = handle.diagonal * 0.5f
  val handles = Handles(this, for { loc <- GlyphShape.cardinalPoints(this.shape) } yield Handle(this, handle, loc-deltaHandle))

  val centreHandle = circle(radius)(centreBrush)

  @inline private def centeredAbout(g: GlyphShape): Vec = Vec(x, y) + ((diagonal - g.diagonal) * 0.5f)

  override def withForeground(brush: Brush): TargetShape = new TargetShape(x, y, shape$.withBrushes(brush))

  def setHovering(state: Boolean): Unit = {
    if (state) {
      handleBrush.alpha(1f)
      handleBrush.strokeWidth(2*handleBrush0.strokeWidth)
    } else {
      handleBrush.alpha(handleBrush0.alpha)
      handleBrush.strokeWidth(handleBrush0.strokeWidth)
      handleBrush.color(handleBrush0.color)
    }
  }

  def setSelected(state: Boolean): Unit = {
    if (state) {
      handleBrush.mode(FILL)
      handleBrush.alpha(1.0)
      handleBrush.color(red.color)
      centreBrush.mode(FILL)
    } else {
      handleBrush.mode(STROKE)
      handleBrush.alpha(handleBrush0.alpha)
      handleBrush.color(handleBrush0.color)
      centreBrush.mode(centreBrush0.mode)
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
    surface.withOrigin(centeredAbout(centreHandle)) { centreHandle.draw(surface) }
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

  def withForeground(brush: Brush): LocatedShape = new LocatedShape(x, y, shape.withBrushes(brush))

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

  def centerAt(loc: Vec): Unit = {
    this.x = loc.x-(w*0.5f); this.y = loc.y-(w*0.5f)
  }

  def moveBy(x: Scalar, y: Scalar): Unit = {
    this.x += x; this.y += y
  }

  def center: Vec = (diagonal * 0.5f) + (x, y)

  def withBrushes(fg: Brush=shape.fg, bg: Brush=shape.bg): LocatedShape = LocatedShape(x, y, shape.withBrushes(fg, bg))

}

object LocatedShape {
  def apply(x: Scalar, y: Scalar, shape: GlyphShape): LocatedShape = new LocatedShape(x, y, shape)
  def apply(pos: Vec, shape: GlyphShape): LocatedShape = new LocatedShape(pos.x, pos.y, shape)
  def apply(pair: (Scalar, Scalar), shape: GlyphShape): LocatedShape = new LocatedShape(pair._1, pair._2, shape)
}

