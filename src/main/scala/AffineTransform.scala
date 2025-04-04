package org.sufrin.glyph

/**
 * Supplementary operations on generalized 2-D transforms.
 */

object AffineTransform {

  import GlyphTypes.Scalar

  import io.github.humbleui.skija.Matrix33
  /*
   * Transforms are arganised as 3 rows of 3, and represented as `Array[Scalar](9)`
   *
   * {{{
   *   a b c
   *   d e f
   *   g h i
   * }}}
   */
  type Transform = Array[Scalar] // of size 9
  def  from(matrix: Matrix33): Transform = matrix.getMat

  /**
   * The inverse of `transform`
   */
  def inverse(transform: Transform): Transform = {
    val Array(a, b, c,  d, e, f,  g, h, i) = transform

    // calculate the transpose of determinant(transform)*transform.inverse.transpose
    val A = e * i - f * h; val B = f * g - d * i; val C = d * h - e * g
    val D = c * h - b * i; val E = a * i - c * g; val F = b * g - a * h
    val G = b * f - c * e; val H = c * d - a * f; val I = a * e - b * d
    // Sarrus's rule for the determinant
    val det = a * A + b * B + c * C

    // retranspose and divide
    Array(
      A / det, D / det, G / det,
      B / det, E / det, H / det,
      C / det, F / det, I / det)
  }

  /**
   * The `Vec` yielded by `trans` applied to `(x,y)`
   */
  @inline def transform(trans: Transform, x: Scalar, y: Scalar): Vec = {
    val Array(a, b, c,  d, e, f,  g, h, i) = trans
    Vec(a*x + b*y + c, d*x + e*y + f)
  }

  /**
   * The `Vec` yielded by `trans` applied to the `Vec` `p`
   */
  def transform(trans: Transform)(p: Vec): Vec = transform(trans, p.x, p.y)

  /** Yield a function that reverses the effect of `transform(trans)` on a `Vec`  */
  def reverse(trans: Transform): Vec => Vec = {
    val Array(a, b, c, d, e, f, g, h, i) = trans
    val A = e * i - f * h; val B = f * g - d * i; val C = d * h - e * g
    val D = c * h - b * i; val E = a * i - c * g; //val F = b * g - a * h
    val G = b * f - c * e; val H = c * d - a * f; //val I = a * e - b * d

    // Sarrus's rule for the determinant
    val det = a * A + b * B + c * C

    val (aa,bb,cc,dd,ee,ff) =
        (A / det, D / det, G / det,
         B / det, E / det, H / det)

    { case v: Vec  => Vec(aa * v.x + bb * v.y + cc, dd * v.x + ee * v.y + ff) }
  }

  /**
   * `reverse(t, ambientScale)(v) = reverse(t)(v scaled ambientScale)`
   */
  def reverse(trans: Transform, ambientScale: Scalar): Vec => Vec = {
    val Array(a, b, c, d, e, f, g, h, i) = trans
    val A = e * i - f * h; val B = f * g - d * i; val C = d * h - e * g
    val D = c * h - b * i; val E = a * i - c * g; //val F = b * g - a * h
    val G = b * f - c * e; val H = c * d - a * f; //val I = a * e - b * d
    // Sarrus's rule for the determinant
    val det = a * A + b * B + c * C

    val (aa, bb, cc, dd, ee, ff) =
      (A / det, D / det, G / det,
       B / det, E / det, H / det)

    {
      case v: Vec => Vec(aa * v.x*ambientScale + bb * v.y*ambientScale + cc, dd * v.x*ambientScale + ee * v.y*ambientScale + ff)
    }
  }

}

