package org.sufrin.glyph

import GlyphTypes.Scalar

import io.github.humbleui.skija.{PaintMode, Path, PathFillMode}
import io.github.humbleui.types.Rect
import org.sufrin.glyph.Brush.ROUND
import org.sufrin.glyph.Brushes.{black, green}
import org.sufrin.glyph.GlyphShape.{asGlyph, circle, rect, superimposed, FILL, STROKE}

/**
 *  Lightweight precursor to `Glyph`. This type arrived in Glyph 0.9 and may eventually
 *  be the basis for simplifying the construction of some heavier-weight passive glyphs.
 *
 *  Atomic glyph shapes are coloured by a brush, and they are filled (or not) depending
 *  on the `mode` of the brush: `FILL` or `STROKE`.
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
    override def toString: String = s"$thisShape.scale($factor)"
  }

  /** Is the given point within the shape: default is within the bounding box */
  @inline def encloses(point: Vec): Boolean = enclosing(point).nonEmpty

  /** The "nearest" enclosing shape */
  def enclosing(point: Vec): Option[GlyphShape] =
    if (0<=point.x && point.x<=w && 0<=point.y && point.y<=h) Some(thisShape) else None

  def turn(degrees: Scalar, tight: Boolean=true): GlyphShape = this.turned(degrees, tight)

  /**
   * This drawable rotated clockwise about its centre through `degrees`,
   */
  def xturn(degrees: Scalar): GlyphShape = new GlyphShape {
    override def toString: String = s"$thisShape.turn($degrees)"

    // Bounding box of the transformed shape -- pessimistic
    def diagonal: Vec = {
      val side = thisShape.diagonal.x max thisShape.diagonal.y
      Vec(side, side)
    }

    // Centre of the original shape's bounding box
    private val originalCentre = thisShape.diagonal scaled 0.5f

    // Distance of the new centre from the host centre
    private val delta = (diagonal scaled 0.5f) - originalCentre

    def draw(surface: Surface): Unit =
        surface.withOrigin(delta) {
          surface.withRot(degrees, originalCentre) {
            thisShape.draw(surface)
          }
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
   *  A `GlyphVariable` shaped like `thisShape`, initially drawn at `(x,y)` rotated through `degrees`.
   *
   * @see GlyphVariable
   */
  def variable(x: Scalar, y: Scalar, degrees: Scalar=0): GlyphVariable = GlyphVariable(x, y, degrees, thisShape)

  /**
   *  A `GlyphLocated` shaped like `thisShape` always drawn at (x,y)`.
   *
   * @see GlyphLocated
   */
  def located(x: Scalar, y: Scalar): GlyphLocated = GlyphLocated(x, y,thisShape)

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
    val delta=Vec(fg.strokeWidth/2, fg.strokeWidth/2)
    def draw(surface: Surface): Unit = surface.withOrigin(delta){
      surface.drawOval(fg, Vec.Zero, diag)
    }

    val diag: Vec = Vec(2*r, 2*r)
    val diagonal: Vec = diag+(delta scaled 2)

    override def toString: String = s"circle($r)($fg)"


    /**
     * This circle superimposed on a circle of the same radius filled with `brush`
     */
    override def bg(brush: Brush): GlyphShape = circle(r)(brush(mode=PaintMode.FILL)) ~~~ this
    override def apply(brush: Brush): GlyphShape = circle(r)(brush)
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

    val diag: Vec = Vec(width, height)
    val diagonal: Vec = diag+(delta scaled 2)

    override def toString: String = s"oval($width,$height)($fg)"


    /**
     * This oval superimposed on an oval of the same dimension filled with `brush`
     */
    override def bg(brush: Brush): GlyphShape = oval(w, h)(brush(mode=PaintMode.FILL)) ~~~ this

    override def apply(brush: Brush): GlyphShape = oval(w, h)(brush)

  }

  /**
   * An arc/sector of a (width x height) oval occupying a `(width+delta x height+delta)` rectangle. The
   * path of the arc starts in direction `startAngle` and extends for `sweepAngle`. The centre is included in
   * the path if `incCentre` is true.
   */
  def arc(width: Scalar, height: Scalar, startAngle: Scalar, sweepAngle: Scalar, incCentre: Boolean)(fg: Brush): GlyphShape = new GlyphShape {
    val delta=Vec(fg.strokeWidth/2, fg.strokeWidth/2)
    def draw(surface: Surface): Unit = surface.withOrigin(delta){
      surface.canvas.drawArc(0, 0, width, height, startAngle, sweepAngle, incCentre, fg)
    }

    val diag: Vec = Vec(width, height)
    val diagonal: Vec = diag+(delta scaled 2)

    override def toString: String = s"arc($width,$height,$startAngle,$sweepAngle,$incCentre)($fg)"


    /**
     * This arc superimposed on an arc with the same characteristics filled with `brush`
     */
    override def bg(brush: Brush): GlyphShape = arc(width, height, startAngle, sweepAngle, incCentre)(brush(mode=PaintMode.FILL)) ~~~ this

    override def apply(brush: Brush): GlyphShape = arc(width, height, startAngle, sweepAngle, incCentre)(brush)

  }

  /**
   * A circular pie with the given radius, with slices painted by brushes.
   * {{{ requires brushes.length>0 }}}
   */
  def pie(radius: Scalar)(brushes: Brush*): GlyphShape = {
    require(brushes.length>0, "A pie must have a positive number of brushes")
    val sector = 360.0f / brushes.length
    var rot = 0.0f
    superimposed(
      brushes.map { brush => rot += sector; arc(2*radius, 2*radius, rot, sector, true)(brush) }
    )
  }


  /**
   *  A line that has no bounding box. Used for drawing lines to be superimposed
   *  on an existing drawing.
   *
   *  For example, the following is the shape of a couple of `spot`s joined by a black line.
   *  It has an empty bounding box, but can be superimposed on another shape (which
   *  should be large enough to contain the spots and the line at their given positions).
   *  {{{
   *    spot.located(p, dy) ~~~
   *    spot.located(dx, p) ~~~
   *    line(Vec(p, dy), Vec(dx, p))(black)
   *  }}}
   *
   * @see located
   */
  def line(start: Vec, end: Vec)(fg: Brush): GlyphShape = new GlyphShape {
    def draw(surface: Surface): Unit =  surface.drawLines(fg, List(start, end))
    def diagonal: Vec = Vec.Zero
    override def toString: String = s"line($start,$end)($fg)"
  }

  /**
   * @see line
   */
  def line(startx: Scalar, starty: Scalar, endx: Scalar, endy: Scalar)(fg: Brush): GlyphShape = new GlyphShape {
    def draw(surface: Surface): Unit = surface.drawLines$(fg, startx, starty, endx, endy)
    def diagonal: Vec = Vec.Zero
    override def toString: String = s"line(($startx,$starty), ($endx,$endy))($fg)"
  }

  def lineBetween(l: GlyphVariable, r: GlyphVariable)(fg: Brush): GlyphShape = new GlyphShape {
      val diagonal: Vec = Vec.Zero

      def draw(surface: Surface): Unit =
        surface.drawLines$(fg, l.x, l.y, r.x, r.y)

      override def toString: String = s"lineBetween($l,$r)($fg)"

  }

  def arrow(fg: Brush): GlyphShape = {
    val a = 3.5f
    val b = 3f*a
    val c = 20f
    val d = 30f
    polygon((0,a), (0, b), (c, b), (c, a+b), (d, (a+b)/2), (c, 0), (c, a), (0, a))(fg)//~~~rect(d,a+b)(black(mode=STROKE))
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
    override def toString: String = s"rect($width,$height)($fg)"


    /**
     * This rectangle superimposed on a rectangle with the same dimensions filled with `brush`
     */
    override def bg(brush: Brush): GlyphShape = this ~~~ rect(w, h)(brush(mode=FILL))

    override def apply(brush: Brush): GlyphShape = rect(width, height)(brush)

  }

  /**
   * A polygon made of `vertex::vertices`, and with the natural bounding box.
   */
  def polygon(vertex: (Scalar, Scalar), vertices: (Scalar, Scalar)*): Brush=>GlyphShape = polygon(vertex::vertices.toList)

  /**
   * A polygon made of the given `vertices`, and with the natural bounding box.
   */
  def polygon(vertices: Iterable[(Scalar, Scalar)])(fg: Brush): GlyphShape = new GlyphShape {
    val kind: String = "Polygon"

    override def toString: String = s"Polygon(...)[$diagonal](fg=$fg)"

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

    private var offsetL, offsetT = 0f

    val diagonal = {
      val bounds = path.getBounds()
      path.updateBoundsCache()
      offsetL = bounds._left -fg.strokeWidth/2
      offsetT = bounds._top  -fg.strokeWidth/2
      Vec(bounds._right-bounds._left+fg.strokeWidth, bounds._bottom-bounds._top+fg.strokeWidth)
    }

    override def enclosing(point: Vec): Option[GlyphShape] = {
      val origin = point+(offsetL, offsetT)
      if (path.contains(origin.x, origin.y)) Some(this) else None
    }

    def draw(surface: Surface): Unit = surface.withOrigin(-offsetL, -offsetT) { surface.drawPath(fg, path) }


    /**
     * This polygon superimposed on a polygon with the same vertices filled with `brush`
     */
    override def bg(brush: Brush): GlyphShape = this ~~~ polygon(vertices)(brush(mode=FILL))

  }

  /** Experimental mutable shape. */
  class PathShape(fg: Brush, absolute: Boolean=true) extends GlyphShape {
    val path = new Path

    def draw(surface: Surface): Unit = if (absolute) {
      surface.drawPath(fg, path)
    } else {
        val bounds = path.getBounds()
        val offsetL = bounds._left -fg.strokeWidth/2
        val offsetT = bounds._top  -fg.strokeWidth/2
        surface.withOrigin(-offsetL, -offsetT) { surface.drawPath(fg, path) }
    }

    def topLeft: Vec = {
      val bounds = path.getBounds()
      Vec(bounds._left, bounds._top)
    }

    def diagonal: Vec =  {
      val bounds = path.getBounds()
      Vec(bounds._right-bounds._left+fg.strokeWidth, bounds._bottom-bounds._top+fg.strokeWidth)
    }

    def fillMode: PathFillMode = path.getFillMode
    def fillMode(mode: PathFillMode) = path.setFillMode(mode)

    def moveTo(x: Scalar, y: Scalar):PathShape = { path.moveTo(x, y); this }
    def lineTo(x: Scalar, y: Scalar):PathShape = { path.lineTo(x, y); this }
    def closePath: PathShape = { path.closePath(); this }
    def addRect(x: Scalar, y: Scalar, w: Scalar, h: Scalar): PathShape =
        { path.addRect(Rect.makeXYWH(x, y, w, h)); this }

    override def toString: String = s"PathShape($fg)(${path.getVerbs.toSeq.mkString(",")})"

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
    val deltas = for { shape <- shapes } yield(Vec((tw-shape.w)/2, (th-shape.h)/2))

    def draw(surface: Surface): Unit = {
      val shape = shapes.iterator
      for { delta <- deltas }
        surface.withOrigin(delta) { shape.next().draw(surface)}
    }

    def diagonal: Vec = Vec(tw, th)

    override def enclosing(point: Vec): Option[GlyphShape] = {
      var it: Option[GlyphShape] = None
      val delta = deltas.iterator
      val shape = shapes.iterator
      println(s"$this.enclosing($point)")
      while (it.isEmpty && delta.hasNext)
        { val s = shape.next()
          val d = delta.next()
          println(s"  $s@${point-d}")
          it = s.enclosing(point-d)
        }
        it
    }

    override def toString: String = s"(${shapes.mkString("~~~")})"

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

/**
 * A variably-located and rotated shape currently located at `(x,y)` and currently rotated by `degrees.`
 * Shown with a "handle" whose appearance can be changed when hovering or selected.
 */
case class GlyphVariable(var x: Scalar, var y: Scalar, private var degrees: Scalar, shape: GlyphShape) extends GlyphShape {
  val brush      = Brushes.yellow(width=2.5f, mode=STROKE, cap=ROUND)
  val radius     = (shape.w max shape.h)/8
  val handle     = rect(radius*2, radius*2)(brush)//circle(radius)(brush)
  var shapeCache = shape.turn(degrees)

  def centre = (diagonal-handle.diagonal) scaled 0.5f

  def setHovering(state: Boolean): Unit =
    if (state) brush.width(4f) else brush.width(2.5f)

  def setSelected(state: Boolean): Unit = {
    brush.mode(if (state) FILL else STROKE)
    brush.color(if (state) green.color else Brushes.yellow.color)
  }

  def draw(surface: Surface): Unit = {
    surface.withOrigin(x, y) {
      shapeCache.draw(surface)
      surface.withOrigin(centre) { handle.draw(surface) }
    }
  }

  def copyState: GlyphVariable = GlyphVariable(x, y, degrees, shape)

  def diagonal: Vec = shapeCache.diagonal

  override def toString: String = s"GlyphVariable(${(x,y)}, $degrees, $shapeCache)"

  /** the glyph's handle contains the mouse */
  def handles(p: Vec): Boolean = handle.encloses(p-(x,y)-centre)//Math.abs(x+w/2-p.x)<radius && Math.abs(y+h/2-p.y)<radius

  /** the glyph's bounding box contains the mouse */
  def beneath(p: Vec): Boolean = shapeCache.encloses(p-(x,y))

  def isIn(set: Seq[GlyphVariable]): Boolean = set contains this

  def notIn(set: Seq[GlyphVariable]): Boolean = !(set contains this)

  def moveTo(x: Scalar, y: Scalar): Unit = { this.x = x;  this.y = y }

  def moveBy(x: Scalar, y: Scalar): Unit = { this.x += x; this.y += y }

  def turnBy(degrees: Scalar): Unit =
    if (degrees != 0) {
      this.degrees += degrees
      shapeCache=shape.turn(this.degrees)
    }

  def turnTo(degrees: Scalar): Unit = {
    this.degrees = degrees
    shapeCache=shape.turn(this.degrees)
  }
}

case class GlyphLocated(x: Scalar, y: Scalar, shape: GlyphShape) extends GlyphShape {

  def draw(surface: Surface): Unit = surface.withOrigin(x, y) { shape.draw(surface) }

  def diagonal: Vec = shape.diagonal

  override def toString: String = s"$shape.located($x,$y)"

  override def enclosing(point: Vec): Option[GlyphShape] = shape.enclosing(point-(x,y))

}
