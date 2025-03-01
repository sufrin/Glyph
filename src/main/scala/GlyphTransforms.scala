package org.sufrin.glyph

import DefaultBrushes.nothing
import GlyphTypes._
import NaturalSize.{Col, Row}

import org.sufrin.glyph.Glyphs.{FilledRect, Rect}
import org.sufrin.glyph.GlyphTransforms.{Edged, Framed, WithBaseline}

/**
 *  As a notational convenience. `Glyphs` have intrinsic transforms that correspond to the external transformers implemented
 *  by the classes nested within the object `GlyphTransforms`. The trait `GlyphTransforms` is a mixin used to define the
 *  class `Glyph`. Transforms are (unless semantic identities, such as `scale(1)`) generative; ie: `transform(g) ne g`.
 *  The transformed glyphs `always` have a "sensible" bounding box; ie one that completely contains the transformed
 *  glyph.
 *
 *  Transformed `Reactive` glyphs usually react "appropriately": for example any `AbstractButton` `rotated(1)` still behaves like
 *  the same button; and its "sensitive" bounding box will also be rotated.
 *
 *  If `R(g1, g2, g3)` generates an abstract reactive glyph from the glyphs `(g1, g2, g3)`, and `T(x)` is an affine transform (marked [A] below), then the
 *  two reactive glyphs below will have the same appearance; and their "sensitive" boxes will, in general, be the same:
 *  {{{
 *    R(g1, g2, g3).T(x)
 *    R(g1.T(x), g2.T(x), g3.T(x))
 *  }}}
 *
 * When supplied, the `Brush` parameters (`bg` in particular) specify the treatment of any "extra" space in the transformed glyph. We have
 * attempted to make the defaults depend "sensibly" on the similarly-named properties of the transformed glyph; but there may be inconsistencies
 * in our treatment that make it necessary to specify them explicitly.
 */
trait GlyphTransforms {
  thisGlyph: Glyph =>

  /**  This glyph scaled by the given `scale` factor. [A] */
  def scaled(scale: Scale): Glyph =
    if (scale == 1f) thisGlyph else new GlyphTransforms.Scaled(thisGlyph, Vec(scale, scale), thisGlyph.fg, thisGlyph.bg)

  def scaleWidth(scalex: Scale): Glyph =
    if (scalex == 1f) thisGlyph else new GlyphTransforms.Scaled(thisGlyph, Vec(scalex, 1.0), thisGlyph.fg, thisGlyph.bg)

  /**  This glyph enlarged in height and width by the given `delta`.*/
  def enlarged(delta: Scalar, fg: Brush = thisGlyph.fg, bg: Brush = null): Glyph =
    if (delta <= 0f) this else GlyphTransforms.Enlarged(delta, delta, fg, bg)(thisGlyph)

  /**  This glyph enlarged, if possible, to have height `h`, and width `w`. */
  def enlargedTo(w: Scalar, h: Scalar, fg: Brush = thisGlyph.fg, bg: Brush = thisGlyph.bg): Glyph =
    GlyphTransforms.Enlarged.toSize(w, h, fg, bg)(thisGlyph)

  /**  This glyph with height enlarged by `h`, and width enlarged by `w` . */
  def enlargedBy(dw: Scalar, dh: Scalar, fg: Brush = thisGlyph.fg, bg: Brush = thisGlyph.bg): Glyph =
    GlyphTransforms.Enlarged.bySize(dw, dh, fg, bg)(thisGlyph)

  /**  This glyph rotated by `quadrants*90` degrees (clockwise) about the centre of its bounding box. [A] */
  def rotated(quadrants: Int, fg: Brush = thisGlyph.fg, bg: Brush = thisGlyph.bg): Glyph =
    GlyphTransforms.Rotated(quadrants, fg, bg)(thisGlyph)

  /**  This glyph rotated by `degrees` degrees (clockwise) about the centre of its bounding box. [A]
   * The resulting bounding box is calculated as the corresponding rotation of
   * the current bounding box. In some cases this is too big: but
   * if `tight` is true the resulting bounding box is calculated more carefully;
   * this is appropriate for glyphs whose actual bounds
   * remain more or less invariant under rotation (for example circular glyphs).
   */
  def turned(degrees: Scalar, tight: Boolean=false, fg: Brush = thisGlyph.fg, bg: Brush = thisGlyph.bg): Glyph =
    new GlyphTransforms.Turned(thisGlyph, degrees, tight, fg, bg)

  /**
   * Same as `this.turned(degrees, fg, bg)` but with an imposed bounding box `(bw, bh)`
   */
  def turnedBoxed(bw: Scalar, bh: Scalar)(degrees: Scalar, fg: Brush = thisGlyph.fg, bg: Brush = thisGlyph.bg): Glyph =
    new GlyphTransforms.Turned(thisGlyph, degrees, false, fg, bg) {
      override def box: Vec = Vec(bw, bh)
    }

  /**
   * A glyph that renders this glyph inside a surround painted with `fg`, and a mount painted with `bg`.
   * If `fg.strokeCap` is not `ROUND` then the surround/mount are painted as rectangles; otherwise they are
   * painted as round rectangles. A `Mounted` glyph can be extracted from its mount: usually done when
   * a collection of glyphs is to be provided with uniform dimensions.
   *
   * @see Framed
   */
  def framed(fg: Brush = GlyphTransforms.Framed.defaultFG,
             bg: Brush = GlyphTransforms.Framed.defaultBG,
             radius: Scalar = 0f): Glyph =  Framed(fg = fg, bg = bg, radius)(thisGlyph)

  /** Same as `framed` */
  def mounted(fg:     Brush = nothing,
              bg:     Brush = nothing,
              radius: Scalar = 0): Glyph = Framed(fg, bg, radius)(thisGlyph)

  /**
   * This `glyph` with a `fg`-coloured edge around it, and a background of `bg` (==`fg` if `bg` is unspecified).
   * The `fg` can have any `strokewidth`. The overall bounding diagonal is that of the glyph enlarged by
   * twice `fg.strokeWidth`. The roundness, if any, of the edge depends on its stroke width; use `framed` if you
   * need a more pronounced rounding.
   *
   * @see framed
   */
  def edged(fg: Brush = GlyphTransforms.Framed.defaultFG,
            bg: Brush = GlyphTransforms.Framed.defaultBG): Glyph = Edged(fg = fg, bg = bg)(thisGlyph)

  /**  This glyph skewed by factors `skewX`, `skewY`. */
  def skewed(skewX: Scalar, skewY: Scalar, fg: Brush = thisGlyph.fg, bg: Brush = thisGlyph.bg): Glyph =
    GlyphTransforms.Skewed(skewX, skewY, fg, bg)(thisGlyph)

  /**  This glyph mirrored about one or both centre lines. */
  def mirrored(leftRight: Boolean, topBottom: Boolean, fg: Brush = thisGlyph.fg, bg: Brush = thisGlyph.bg): Glyph =
    GlyphTransforms.Mirrored(leftRight, topBottom, fg, bg)(thisGlyph)

  /**  This glyph enlarged by `enlarge*`, with rectilinear shading of width `delta`, at the bottom right corner (if down)
   *   else the top left corner. The enlargement is `enlarge` itself if `enlarge>1`, otherwise `enlarge * (thisGlyph.w min thisGlyph.h)`
   */
  def shaded(fg: Brush = thisGlyph.fg, bg: Brush = thisGlyph.bg, enlarge: Scalar = 0.25f, delta: Scalar = 8f, down: Boolean=false): Glyph =
    new GlyphTransforms.Shaded(thisGlyph.enlarged(if (enlarge < 1f) enlarge * (thisGlyph.w min thisGlyph.h) else enlarge), fg = fg, bg = bg, delta = delta, down)

  /** This glyph in a cavity of size `(w,h)` displaced by `(dx, dy)` */
  def inCavity(w: Scalar, h: Scalar, dx: Scalar, dy: Scalar): Glyph =
    new GlyphTransforms.InCavity(w, h, dx, dy, thisGlyph, thisGlyph.fg, thisGlyph.bg)

  /**  `Row.Center(thisGlyph, g)` */
  def beside(g: Glyph, align: VAlignment=Mid): Glyph = Row(align=align)(thisGlyph, g)
  def besideTop(g: Glyph): Glyph = beside(g, align=Top)
  def besideBottom(g: Glyph): Glyph = beside(g, align=Bottom)

  /**  `Col.Center(thisGlyph, g)` */
  def above(g: Glyph, align: Alignment=Center): Glyph = Col(align=align)(thisGlyph, g)
  def aboveLeft(g: Glyph): Glyph = above(g, align=Left)
  def aboveRight(g: Glyph): Glyph = above(g, align=Right)

  /** behaves as this glyph with the specified baseLine */
  def withBaseline(baseLine: Scalar): Glyph = WithBaseline(thisGlyph, baseLine)
}

object GlyphTransforms {
  /**
   * A glyph that renders as `glyph` inside a surround painted with `fg`, and a mount painted with `bg`.
   * If `fg.strokeCap` is not `ROUND` then the surround/mount are painted as rectangles; otherwise they are
   * painted as round rectangles. A `Mounted` glyph can be extracted from its mount: usually done when
   * a collection of glyphs is to be provided with uniform dimensions.
   *
   * @see Mounted
   */
  object Framed extends DefaultPaints {
    def apply(fg: Brush=nothing,  bg: Brush=nothing, radius: Scalar = 0f)(glyph: Glyph): Glyph = {
      val rad = if (radius<0f) 0 else {
        if (radius==0f)   bg.strokeWidth max fg.strokeWidth max 1.0f
        if (radius<=1.0f) (glyph.h min glyph.w)*radius
        else radius
      }

      @inline def round(brush: Brush): Brush = if (rad==0) brush else brush(width=rad).rounded(rad)

      lazy val frameOnly:  Glyph = Rect(glyph.w+rad*2, glyph.h+rad*2,   fg=round(fg))  // open rectangle (may be curved)
      lazy val mountOnly:  Glyph = FilledRect(glyph.w+rad, glyph.h+rad, fg=round(bg))  // closed rectangle with (rounded) bg
      lazy val frameAfter: Glyph = FilledRect(mountOnly.w+rad*2, mountOnly.h+rad*2, fg=round(fg))

      Glyphs.Concentric(bg=round(bg))(
        (fg.getAlpha!=0, bg.getAlpha!=0) match {
          case (true, true)  => List(mountOnly, frameAfter, glyph)
          case (true, false) => List(frameOnly, glyph)
          case (false, true) => List(mountOnly, glyph)
          case _ => List(glyph)
        }
      )
    }

    //def apply(fg: Brush = defaultFG, bg: Brush = defaultBG, radius: Scalar=0f)(glyph: Glyph): Glyph =
    //  new Framed(glyph, fg, bg, radius)
  }


  /**
   * A `TransformedGlyph` is built from a subject glyph, and
   * can forward a variety of methods to its subjects.
   */
  abstract class TransformedGlyph extends Glyph {
    val glyph: Glyph
    override def enabled(state: Boolean): Boolean = {
      glyph.enabled(state)
    }
  }

  /**
   * Same as `Framed`
   */
  object Mounted extends DefaultPaints {
    def apply(fg: Brush = defaultFG, bg: Brush = defaultBG, radius: Scalar=0)(glyph: Glyph): Glyph = Framed(fg, bg, radius)(glyph)
  }


  /** The glyph displaced by `(dx,dy)` in a cavity of size `(w, h)` */
  private class InCavity(w: Scalar, h: Scalar, dx: Scalar, dy: Scalar, val glyph: Glyph, val fg: Brush, val bg: Brush) extends TransformedGlyph {

    override def toString: String = s"InCavity($w, $h, $dx, $dy)($glyph)"
    override def reactiveContaining(p: Vec): Option[ReactiveGlyph] = glyph.reactiveContaining(p-delta)
    override def glyphContaining(p: Vec): Option[Hit] = glyph.glyphContaining(p-delta)

    /**
     * Draw the glyph on the surface at its given size (as if at the origin).
     */
    def draw(surface: Surface): Unit = {
      drawBackground(surface)
      surface.withClip(diagonal) {
        surface.withOrigin(delta) {
          glyph.draw(surface)
        }
      }
    }

    private val delta: Vec = Vec(dx, dy)

    /**
     * The diagonal size of the glyph
     */
    val diagonal: Vec = Vec(w, h)

    /** A copy of this glyph; perhaps with different foreground/background */
    def copy(fg: Brush=this.fg, bg: Brush=this.bg): Glyph = new InCavity(w, h, dx, dy, glyph, fg, bg)

  }
  /**
   * A glyph that renders as `glyph` framed by a surround painted with `fg`, on a mount painted with `bg`.
   *
   * Unless `fg.cap` is `ROUND` or `radius` is `0` then the surround/mount are rectangles; otherwise they are
   * round rectangles, with lateral/vertical radius factors both specified as `radius` (if it is nonzero), or
   * `.25f` if it is zero.
   *
   * The size of the mounted glyph is always extended by a multiple, `K` of `fg.strokeWidth` in each direction.
   * When `fg.strokeCap` is `ROUND`, `K` is 3; otherwise it is `2`. The former factor is usually enough for the
   * bounding box of the original glyph to fit inside the rim of the frame; except when `fg.strokeWidth` is
   * small.
   *
   * A `Framed` glyph can be extracted from its mount: usually done when
   * a collection of glyphs is to be provided with uniform dimensions.
   *
   */
  class Framed(val glyph: Glyph, val fg: Brush, val bg: Brush, val radiusFactor: Scalar=0f) extends TransformedGlyph {
    import Glyphs.{FilledRect, Rect, RRect}

    override def toString: String = s"Mounted($fg, $bg)($glyph)"
    override def reactiveContaining(p: Vec): Option[ReactiveGlyph] = delegate.reactiveContaining(p)
    override def glyphContaining(p: Vec): Option[Hit] = delegate.glyphContaining(p)
    override def contains(p: Vec): Boolean = delegate.contains(p)
    override def copy(fg: Brush = fg, bg: Brush = bg): Framed = new Framed(glyph.copy(), fg, bg, radiusFactor)


    val delegate: Glyph = {
      if (fg.strokeCap == Brush.ROUND || radiusFactor>0f) {
        val gw = glyph.w + fg.strokeWidth * 3 // Larger than for rectangular: the curvature bites otherwise
        val gh = glyph.h + fg.strokeWidth * 3
        // calculate sensible rounding radii
        val (xrf, yrf) =
          if (radiusFactor>0f) (radiusFactor, radiusFactor) else (.25f, .25f)

        Glyphs.Concentric(rowAlign=Mid, colAlign=Center)(
          RRect(gw, gh, solid = true, xrf = xrf, yrf = yrf, fg = bg, bg = nothing),
          RRect(gw, gh, solid = false, xrf = xrf, yrf = yrf, fg = fg, bg = nothing),
          glyph)
      } else {
        val gw = glyph.w + fg.strokeWidth * 2
        val gh = glyph.h + fg.strokeWidth * 2
        Glyphs.Concentric(rowAlign=Mid, colAlign=Center)(
          FilledRect(gw, gh, fg = bg, bg = nothing),
          Rect(gw, gh, fg = fg, bg = nothing),
          glyph)
      }
    }

    val diagonal: Vec = delegate.diagonal

    override def draw(surface: Surface): Unit = {
      delegate.draw(surface)
    }

    locally { delegate.parent = this }

  }

  object Edged {
    /**
     * The `glyph` with a `fg`-coloured edge around it, and a background of `bg` (==`fg` if `bg` is unspecified).
     * The `fg` can have any `strokewidth`. The overall bounding diagonal is that of the glyph enlarged by
     * twice `fg.strokeWidth`. The roundness, if any, of the edge depends on its stroke width; use `Framed` if you
     * need a more pronounced rounding.
     */
    def apply(fg: Brush=DefaultBrushes.black, bg: Brush=nothing)(glyph: Glyph): Glyph =
      new Edged(glyph, fg, bg)
  }

  class Edged(val glyph: Glyph, val fg: Brush, val bg: Brush) extends TransformedGlyph {

    private val edgeWidth: Scalar = fg.strokeWidth
    private val inset: Vec = Vec(edgeWidth / 2f, edgeWidth / 2f)
    private val offset: Vec = Vec(edgeWidth, edgeWidth)

    /**
     * The diagonal size of the glyph
     */
    val diagonal: Vec = glyph.diagonal + offset.scaled(2f)

    def draw(surface: Surface): Unit = {
      // avoid the background spilling outside a rounded frame
      if ((bg ne null) && bg.getAlpha !=0  && fg.strokeCap == Brush.ROUND) {
          surface.withOrigin(offset) {
            surface.fillRect(bg, glyph.diagonal)
          }
        } else {
          drawBackground(surface)
        }
      surface.drawRect(fg, inset, diagonal-offset)
      surface.withOrigin(offset) {
          glyph.draw(surface)
        }
    }

    locally { glyph.parent = this }

    override def glyphContaining(p: Vec): Option[Hit] = glyph.glyphContaining(p-offset)
    override def reactiveContaining(p: Vec): Option[ReactiveGlyph] = glyph.reactiveContaining(p-offset)


    /** A copy of this glyph; perhaps with different foreground/background */
    def copy(fg: Brush=fg, bg: Brush=bg): Glyph = new Edged(glyph.copy(), fg, bg)
  }


  object Enlarged {
    /**
     *
     * If both `dw, dh` are zero, then the original `glyph` is returned.
     *
     * Otherwise return a `Glyph` with a bounding box enlarged by `(dw, dh)`. This is
     * drawn by drawing `glyph` in the centre of the enlarged bounding box.
     *
     * Unless otherwise specified, `fg` and `bg` are inherited from `glyph`.
     */
    def apply(dw: Scalar, dh: Scalar, fg: Brush = null, bg: Brush = null)(_glyph: Glyph): Glyph =
      if (dw <= 0f && dh <= 0f) _glyph else {
        val (_fg, _bg) = (fg, bg)
        new TransformedGlyph {
          val glyph: Glyph = _glyph
          val fg: Brush = if (_fg eq null) glyph.fg else _fg
          val bg: Brush = if (_bg eq null) glyph.bg else _bg

          private val offset = Vec(dw / 2f, dh / 2f)

          /**
           * Draw the glyph on the surface at its given size (as if at the origin).
           */
          def draw(surface: Surface): Unit = {
            surface.withClip(diagonal) {
              drawBackground(surface)
              surface.withOrigin(offset) {
                glyph.draw(surface)
              }
            }
          }

          /**
           * The diagonal size of the glyph
           */
          def diagonal: Vec = glyph.diagonal + (dw, dh)

          locally {
            glyph.parent = this
          }

          override def isReactive: Boolean = glyph.isReactive

          override def reactiveContaining(p: Vec): Option[ReactiveGlyph] =
            glyph.reactiveContaining(p - offset)

          override def glyphContaining(p: Vec): Option[Hit] =
            glyph.glyphContaining(p - offset)


          /** A copy of this glyph; perhaps with different foreground/background */
          def copy(fg: Brush, bg: Brush): Glyph = Enlarged(dw, dh)(glyph.copy(fg, bg))

        }
      }

      def toSize(w: Scalar, h: Scalar, _fg: Brush = null, _bg: Brush = null)(glyph: Glyph): Glyph =
        Enlarged(w - glyph.w, h - glyph.h, _fg, _bg)(glyph)

      def bySize(dw: Scalar, dh: Scalar, _fg: Brush = null, _bg: Brush = null)(glyph: Glyph): Glyph =
        Enlarged(dw, dh, _fg, _bg)(glyph)

    }


  /**
   * The given `glyph` rotated by `quadrants%4*90` degrees.
   *
   * Unless otherwise specified, `fg` and `bg` are inherited from `glyph`.
   *
   * @see Turned
   */
  class Rotated(val glyph: Glyph, quads: Int, val fg: Brush, val bg: Brush) extends TransformedGlyph {
    override val kind = "Rotated"
    private val quadrants = Rotated.mod4(quads)
    override def toString: String = s"Rotated($quadrants, fg=$fg, bg=$bg)(${glyph.toString})"

    private val d  = glyph.diagonal
    private val dx = d.x
    private val dy = d.y

    val (diagonal, x, y) = quadrants % 4 match {
      case 0 => (d,        0f, 0f)
      case 1 => (d.rotate, dy, 0f)
      case 2 => (d,        dx, dy)
      case 3 => (d.rotate, 0f, dx)
    }




    def draw(surface: Surface): Unit = {
      drawBackground(surface)
      surface.withOrigin(x, y) {
        surface.withRot(90f * (quadrants % 4)) {
          glyph.draw(surface)
        }
      }
    }

    locally {
      glyph.parent = this
    }

    /**
     *  Translate `p` to its location relative to the new origin
     */
    @inline def translate(p: Vec): Vec = {
      val res = quadrants % 4 match {
        case 0 => p // p
        case 2 => Vec(dx - p.x, dy - p.y) // d-p
        case 1 => Vec(p.y, dy - p.x)
        case 3 => Vec(dx - p.y, p.x)
      }
      res
    }

    override def reactiveContaining(p: Vec): Option[ReactiveGlyph] = glyph.reactiveContaining(translate(p))

    override def glyphContaining(p: Vec): Option[Hit] =  glyph.glyphContaining(translate(p))

    // override def transform(absolute: Vec): Vec = translate(absolute)


    def copy(fg: Brush = fg, bg: Brush = bg): Rotated =
      new Rotated(glyph.copy(), quadrants, fg, bg)
  }

  object Rotated extends DefaultPaints {

    import scala.annotation.tailrec

    @inline @tailrec private def mod4(m: Int): Int = if (m<0) mod4(4+m) else m % 4
    /**
     * Returns `glyph` rotated by `quadrants*90` degrees.
     *
     * Unless otherwise specified, `fg` and `bg` are inherited from `glyph`.
     */
    def apply(quadrants: Int, fg: Brush = null, bg: Brush = null)(glyph: Glyph): Rotated =
      new Rotated(glyph, quadrants, fg=if (fg eq null) glyph.fg else fg,
                                    bg=if (bg eq null) glyph.bg else bg)
  }

  /**
   *  The given `glyph` rotated by `degrees` degrees. Unless `tight` the bounding box of the
   *  result is calculated by rotating the glyph's bounding box, and may (for near-circular glyphs)
   *  be insufficiently tight. When `tight` is true, then the bounding box is a square whose side is the
   *  larger of the sides of the glyph's box: this is tighter for near-circular glyphs.
   *
   *  It took me an unconscionably long time to get the `relativeLocation` function right. In the end
   *  it turned out to be obvious.
   */
  class Turned(val glyph: Glyph, degrees: Scalar, tight: Boolean, val fg: Brush, val bg: Brush) extends TransformedGlyph {
    import Math.PI
    private val `pi`    = PI
    private val `pi/2`  = `pi`/2
    private val `3pi/2` = 3*`pi/2`

    override val kind = "Turned"
    override def toString: String = s"Turned($degrees, fg=$fg, bg=$bg)(${glyph.toString})"

    private val d = glyph.diagonal

    // degrees mod 360 as radians
    private val Theta: Scalar = {
       var norm: Scalar = degrees
       while (norm<0)    norm += 360f
       while (norm>360f) norm -= 360f
      (norm * PI / 180f).toFloat
    }

    @inline private def cos(theta: Double): Scalar = Math.cos(theta).toFloat
    @inline private def sin(theta: Double): Scalar = Math.sin(theta).toFloat

    private val cosTheta = cos(Theta)
    private val sinTheta = sin(Theta)


    /** default bounding box: overridden for `turnedBoxed` */
    def box: Vec =
      if (tight) {
        val D = d.x max d.y
        Vec(D,D)
      }
      else
      {   // Dispatch on the quadrant
          @inline def oddQuadrant(theta: Double)  = Vec((d.x * cos(theta) + d.y * sin(theta)).abs, (d.x * sin(theta) + d.y * cos(theta)).abs)
          @inline def evenQuadrant(theta: Double) = Vec((d.x * sin(theta) + d.y * cos(theta)).abs, (d.x * cos(theta) + d.y * sin(theta)).abs)
          if (Theta <= `pi/2`)
            oddQuadrant(Theta)
          else
          if (Theta <= `pi`)
            evenQuadrant(Theta - `pi/2`)
          else if (Theta <= `3pi/2`)
            oddQuadrant(Theta - `pi`)
          else
            evenQuadrant(Theta - `3pi/2`)
      }

    // Bounding box of the transformed glyph
    def diagonal: Vec = box

    // Centre of the glyph's bounding box
    private val glyphCentre = d scaled 0.5f
    // Centre of this bounding box
    private val thisCentre = diagonal scaled 0.5f

    // Distance of the new centre from the old centre
    private val delta  = thisCentre - glyphCentre


    def draw(surface: Surface): Unit = {
        drawBackground(surface)
        surface.withClip(diagonal) {
          surface.withOrigin(delta) {
            surface.withRot(degrees, glyphCentre) {
              glyph.draw(surface)
//              if (debug) {
//                surface.drawPoint(lastLoc, BLUE)
//                surface.drawPoint(glyphCentre, RED)
//              }
            }
          }
//          if (debug) {
//            surface.drawPoint(thisCentre, GREEN)
//            surface.drawPoint(lastCursor, RED)
//          }
        }
    }

    locally { glyph.parent = this }

    @inline private def relativeLocation(glyphPos: Vec): Vec = {
      val Vec(x, y) = glyphPos - thisCentre  // vector to the centre of this glyph
      val xr = x*cosTheta + y*sinTheta       // rotated by theta
      val yr = y*cosTheta - x*sinTheta
      glyphCentre+Vec(xr, yr)
    }

    override def reactiveContaining(glyphPos: Vec): Option[ReactiveGlyph] =
      glyph.reactiveContaining(relativeLocation(glyphPos))


    override def glyphContaining(glyphPos: Vec): Option[Hit] =
      glyph.glyphContaining(relativeLocation(glyphPos))


    def copy(fg: Brush = fg, bg: Brush = bg): Turned =
      new Turned(glyph.copy(), degrees, tight, fg, bg)
  }

  object Turned extends DefaultPaints {

    /**
     * @see Turned
     */
    def apply(degrees: Scalar, fg: Brush = null, bg: Brush = null)(glyph: Glyph): Turned =
        new Turned(glyph, degrees, degrees<=0f,
          fg = if (fg eq null) glyph.fg else fg,
          bg = if (bg eq null) glyph.bg else bg)
    def tight(degrees: Scalar, fg: Brush = null, bg: Brush = null)(glyph: Glyph): Turned =
      new Turned(glyph, degrees, true,
        fg = if (fg eq null) glyph.fg else fg,
        bg = if (bg eq null) glyph.bg else bg)
  }

  /**
   * Glyph formed by skewing the "top" of glyph to the right, and downwards using
   * non-negative factors `skewX` and `skewY`.
   *
   * The transformer `Skewed(skewX: Scalar, skewY: Scalar)(glyph)` implements negative
   * skew factors correctly by pre- and post- mirroring where necessary. This could undoubtedly
   * be made more efficient by calculating the `skewed` transform appropriately.
   *
   */
  class Skewed(val glyph: Glyph, skewX: Scalar, skewY: Scalar, val fg: Brush, val bg: Brush) extends TransformedGlyph {
    require(skewX>=0f && skewY>=0f, "Skew factors must be non-negative")
    override val kind = "Skewed"
    override def toString: String = s"Skewed($skewX, $skewY, fg=$fg, bg=$bg)(${glyph.toString})"

    private val d = glyph.diagonal
    private val center = d scaled 0.5f

    val diagonal: Vec = d + (skewX*d.y, skewY*d.x)

    private val delta = Vec(-diagonal.x, 0f)

    def translate(p: Vec): Vec = p.skewed(-skewX, -skewY)

    private val skew = Array(1f,   skewX,  diagonal.x,
                             skewY, 1f,    0f,
                             0f,    0f,    1f)

    def draw(surface: Surface): Unit = {
        drawBackground(surface)
        surface.withOrigin(delta) { surface.withTransform(skew) { glyph.draw(surface) } }
    }

    locally {
      glyph.parent = this
    }

    override def reactiveContaining(p: Vec): Option[ReactiveGlyph] = glyph.reactiveContaining(translate(p))

    override def glyphContaining(p: Vec): Option[Hit] = glyph.glyphContaining(translate(p))

    def copy(fg: Brush = fg, bg: Brush = bg): Skewed =
      new Skewed(glyph.copy(), skewX, skewY, fg, bg)
  }

  object Skewed extends DefaultPaints {
    /**
     * Yields a glyph whose image is skewed (aka sheared) using factors `skewX, skewY`. When `skewX` is non-negative, the skew
     * is "rightwards from top to bottom"; when negative the skew is "leftwards from top to bottom". Analogously, when `skewY`
     * is positive the image "dips" from left to right; when negative, it "lifts" from left to right.
     *
     * Unless otherwise specified, `fg` and `bg` are inherited from `glyph`.
     */
    def apply(skewX: Scalar, skewY: Scalar, fg: Brush = null, bg: Brush = null)(glyph: Glyph): Glyph = {
      val ffg = if (fg eq null) glyph.fg else fg
      val bbg = if (bg eq null) glyph.bg else bg

      (skewX>=0f, skewY>=0f) match {
        case (true, true)   => new Skewed(glyph, skewX, skewY, ffg, bbg)
        case (true, false)  => Mirrored.leftRight(fg, bg)(new Skewed(Mirrored.leftRight(ffg, bbg)(glyph), skewX, -skewY, fg, bg))
        case (false, true)  => Mirrored.topBottom(fg, bg)(new Skewed(Mirrored.topBottom(ffg, bbg)(glyph), -skewX, skewY, fg, bg))
        case (false, false) => Mirrored.topBottom(fg, bg)(new Skewed(Mirrored.topBottom(ffg, bbg)(glyph), -skewX, -skewY, fg, bg))
      }
    }
  }

  class Mirrored(val glyph: Glyph, leftRight: Boolean, topBottom: Boolean, val fg: Brush, val bg: Brush) extends TransformedGlyph {

    override val kind = "Mirrored"

    override def toString: String = s"Mirrored($leftRight, $topBottom, fg=$fg, bg=$bg)(${glyph.toString})"

    val diagonal: Vec = glyph.diagonal

    private val (xf, dx) = if (leftRight) (-1f, diagonal.x) else (1f, 0f)
    private val (yf, dy) = if (topBottom) (-1f, diagonal.y) else (1f, 0f)

    private def translate(p: Vec): Vec = Vec(if (leftRight) diagonal.x-p.x else p.x, if (topBottom) diagonal.y-p.y else p.y)

    private val mirror: Array[Float] =
      Array(xf, 0f, dx,
            0f, yf, dy,
            0f, 0f, 1f)

    def draw(surface: Surface): Unit = {
      drawBackground(surface)
      surface.withTransform(mirror) { glyph.draw(surface) }
      }

    locally {
      glyph.parent = this
    }

    override def reactiveContaining(p: Vec): Option[ReactiveGlyph] = glyph.reactiveContaining(translate(p))

    override def glyphContaining(p: Vec): Option[Hit] = glyph.glyphContaining(translate(p))

    def copy(fg: Brush = fg, bg: Brush = bg): Glyph =
      new Mirrored(glyph.copy(), leftRight, topBottom, fg, bg)
  }

  object Mirrored extends DefaultPaints {
    def apply(leftRight: Boolean, topBottom: Boolean, fg: Brush = defaultFG, bg: Brush = defaultBG)(glyph: Glyph): Mirrored =
      new Mirrored(glyph, leftRight, topBottom, fg, bg)

    def leftRight(fg: Brush = defaultFG, bg: Brush = defaultBG)(glyph: Glyph): Mirrored =
      new Mirrored(glyph, leftRight = true, topBottom = false, fg, bg)

    def topBottom(fg: Brush = defaultFG, bg: Brush = defaultBG)(glyph: Glyph): Mirrored =
      new Mirrored(glyph, leftRight = false, topBottom = true, fg, bg)

    def bothAxes(fg: Brush = defaultFG, bg: Brush = defaultBG)(glyph: Glyph): Mirrored =
      new Mirrored(glyph, leftRight = true, topBottom = true, fg, bg)
  }


  /**
   * The given `glyph` scaled by the given hardwareScale.
   *
   * TODO: when buttons constructed with `RawButton` are scaled by large factors, their behaviour under mouse motion is flaky  (the
   *       display flashes, and mouse-down events can be missed).
   *       We don't expect post-construction scaling of buttons to be used very often, but until the behaviour has been investigated
   *       and fixed it is better to use pre-scaled `up`, `down`, `hover` glyphs inside such buttons,
   *       and the `.scaled(...)` methods of `RawButton` have been adjusted to use pre-scaled copies of `up`, `down`, `hover`.
   *
   * @see RawButton.hardwareScale
   *
   */
  class Scaled(val glyph: Glyph, scale: Vec, val fg: Brush, val bg: Brush) extends TransformedGlyph {
    override def toString: String = s"Scaled($scale)($glyph, fg=$fg, bg=$bg)"

    def draw(surface: Surface): Unit = {
      drawBackground(surface)
      surface.withScale(scale) {
        glyph.draw(surface)
      }
    }

    def diagonal: Vec = glyph.diagonal scaled scale

    locally {
      glyph.parent = this
    }

    override def reactiveContaining(p: Vec): Option[ReactiveGlyph] =
      glyph.reactiveContaining(p.deScaled(scale))

    override def glyphContaining(p: Vec): Option[Hit] =
      glyph.glyphContaining(p.deScaled(scale))

    override def contains(p: Vec): Boolean =
      glyph.contains(p)

   // override def transform(position: Vec): Vec = position.deScaled(hardwareScale)

    def copy(fg: Brush = fg, bg: Brush = bg): Scaled = new Scaled(glyph.copy(), scale, fg, bg)
  }

  object Scaled extends DefaultPaints {
    def apply(scale: Scale, fg: Brush = defaultFG, bg: Brush = defaultBG)(glyph: Glyph): Glyph =
      if (scale == 1f) glyph else new Scaled(glyph, Vec(scale, scale), fg, bg)

    def apply(wScale: Scale, hScale: Scalar, fg: Brush, bg: Brush)(glyph: Glyph): Glyph =
      if (wScale == 1f && hScale == 1f) glyph else new Scaled(glyph, Vec(wScale, hScale), fg, bg)
  }

  object Shaded {
    def apply(glyph: Glyph, fg: Brush, bg: Brush, enlarge: Scalar = 0.25f, delta: Scalar = 8f, down: Boolean=false): Glyph =
        new Shaded(glyph.enlarged(if (enlarge <= 1f) enlarge * (glyph.w min glyph.h) else enlarge), fg = fg, bg = bg, delta = delta, down)
  }

  class Shaded(val glyph: Glyph, val fg: Brush, val bg: Brush, delta: Scalar, val down: Boolean) extends TransformedGlyph {

    override def toString: String = s"Shaded.Static($fg, $bg, delta=$delta, $down)\n  ($glyph)"

    private val offset    = Vec(delta, delta)
    private val linePaint = Brush().setColor(0x99070707).setStrokeWidth(0f) //.setStrokeCap(PaintStrokeCap.SQUARE)
    private val shading   = Glyphs.Shaded.shadingPaths(glyph.w, glyph.h, delta)

    val diagonal: Vec = glyph.diagonal + offset

    def draw(surface: Surface): Unit = {
      if (down) {
        surface.drawPath(linePaint, shading.topLeft)
        surface.withOrigin(offset) {
          surface.fillRect(bg, glyph.diagonal)
          glyph.draw(surface)
        }
      } else {
        surface.fillRect(bg, glyph.diagonal)
        glyph.draw(surface)
        surface.drawPath(linePaint, shading.bottomRight)
      }
    }

    locally {
      glyph.parent = this
    }

    override def reactiveContaining(p: Vec): Option[ReactiveGlyph] =
      glyph.reactiveContaining(if (down) p-offset else p)

    override def glyphContaining(p: Vec): Option[Hit] =
      glyph.glyphContaining(if (down) p-offset else p)


    /** A copy of this glyph; perhaps with different foreground/background */
    def copy(fg: Brush, bg: Brush): Glyph = new Shaded(glyph.copy(), fg, bg, delta, down)

  }

  object WithBaseline {
    def apply(glyph: Glyph, baseLine$: Scalar): Glyph = new Glyph { thisGlyph =>

      locally { glyph.parent=thisGlyph }

      override def toString: String = s"$glyph.withBaseline(${baseLine$})"

      override def reactiveContaining(p: Vec): Option[ReactiveGlyph] = glyph.reactiveContaining(p)

      override def glyphContaining(p: Vec): Option[Hit] = glyph.glyphContaining(p)

      override def baseLine: Scalar = baseLine$

      def diagonal: Vec = Vec(glyph.w, glyph.h)

      def copy(fg: Brush=fg, bg: Brush=bg): Glyph = {
        WithBaseline(glyph.copy(fg, bg), baseLine$)
      }

      val fg: Brush = glyph.fg
      val bg: Brush = glyph.bg

      def draw(surface: Surface): Unit = glyph.draw(surface)
    }
  }
}
