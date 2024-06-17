package org.sufrin.glyph
import GlyphTypes._

/**  2D vectors and their arithmetic
  */
trait Vec {
  val x: Scalar
  val y: Scalar

  def toPoint: io.github.humbleui.types.Point =
    new io.github.humbleui.types.Point(x, y)

  override def toString: String = s"($x,$y)"

  def +(delta: Vec): Vec = Vec(x + delta.x, y + delta.y)

  def +(dx: Scalar, dy: Scalar): Vec = Vec(x + dx, y + dy)

  def -(delta: Vec): Vec = Vec(x - delta.x, y - delta.y)

  def -(dx: Scalar, dy: Scalar): Vec = Vec(x - dx, y - dy)

  def scaled(factor: Vec): Vec = Vec(x * factor.x, y * factor.y)

  def scaled(fx: Scale, fy: Scale): Vec = Vec(x * fx, y * fy)

  def scaled(factor: Scale): Vec = Vec(x * factor, y * factor)

  def deScaled(fx: Scale, fy: Scale): Vec = Vec(x / fx, y / fy)

  def deScaled(factor: Vec): Vec = Vec(x / factor.x, y / factor.y)

  def toPair: (Int, Int) = (x.toInt, y.toInt)

  /** This `Vec` rotated about `c` */
  def turned(degrees: Scalar, c: Vec): Vec = {
    import Math.{PI, cos, sin}
    val dx = x - c.x
    val dy = y - c.y
    val theta = degrees * (PI / 180)
    val costheta = cos(theta).toFloat
    val sintheta = sin(theta).toFloat
    Vec(
      dx * costheta - dy * sintheta + c.x,
      dx * sintheta + dy * costheta + c.y
    )
  }

  /** This `Vec` skewed by `skewX, skewY`
    */
  def skewed(skewX: Scalar, skewY: Scalar): Vec = {
    Vec(x + y * skewX, y + x * skewY)
  }

  def toInts: (Int, Int) = (x.toInt, y.toInt)

  def distanceTo(other: Vec): Scalar = {
    val dx = x - other.x
    val dy = y - other.y
    root(dx * dx + dy + dy)
  }

  def union(other: Vec): Vec = Vec(this.x max other.x, this.y max other.y)

  /**  Does this `Vec` fall wholly inside the box of the given `diagonal` at `origin`
    */
  def inside(origin: Vec, diagonal: Vec): Boolean =
    origin.x <= x && x <= origin.x + diagonal.x &&
      origin.y <= y && y <= origin.y + diagonal.y

  def length: Scalar = root(x * x + y * y)

  def root(d: Scalar): Scalar = Math.sqrt(d).toFloat

  def ==(other: Vec): Boolean = x == other.x && y == other.y
  @inline def reverse: Vec = Vec(-x, -y)
  @inline def rotate: Vec = Vec(this.y, this.x)
  @inline def rotate(quadrants: Int): Vec = (quadrants % 4) match {
    case 0 | 2 => this
    case 1 | 3 => Vec(this.y, this.x)
  }
}

object Vec {
  import GlyphTypes._

  val Zero: Vec = Vec(0.0f, 0.0f)
  val Origin: Vec = Zero

  @inline def unapply(v: Vec): Option[(Scalar, Scalar)] = Some(v.x, v.y)

  @inline def apply(_x: Scalar, _y: Scalar): Vec = new Vec {
    val x = _x
    val y = _y
  }

  def apply(_x: Scalar, _y: Int): Vec = new Vec {
    val x = _x
    val y = _y.toFloat
  }

  def apply(_x: Int, _y: Scalar): Vec = new Vec {
    val x = _x.toFloat
    val y = _y
  }

  def apply(_x: Double, _y: Double): Vec = new Vec {
    val x = _x.toFloat
    val y = _y.toFloat
  }

  def apply(pos: (Int, Int)): Vec = { val (x, y) = pos; Vec(x, y) }

  def apply(_x: Int, _y: Int): Vec = new Vec {
    val x = _x.toFloat
    val y = _y.toFloat
  }

  def scaleToPixels(scale: Scalar, pos: (Int, Int)): Pixels = {
    val (x, y) = pos; ((x * scale).toInt, (y * scale).toInt)
  }

  def scaleToPixels(scale: Scalar, x: Scalar, y: Scalar): (Int, Int) =
    ((x * scale).toInt, (y * scale).toInt)
}
