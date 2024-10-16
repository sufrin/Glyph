package org.sufrin.glyph

import Glyphs.nothing
import GlyphTypes._
import NaturalSize.{Col, Row}

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
abstract trait GlyphTransforms {
  thisGlyph: Glyph =>

  /**  This glyph scaled by the given `scale` factor. [A] */
  def scaled(scale: Scale): Glyph =
    if (scale == 1f) thisGlyph else new GlyphTransforms.Scaled(thisGlyph, Vec(scale, scale), thisGlyph.fg, thisGlyph.bg)

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
             radiusFactor: Scalar = 0f): Glyph =
             new GlyphTransforms.Framed(thisGlyph, fg = fg, bg = bg, radiusFactor)

  /** Same as `framed` */
  def mounted(fg: Brush = GlyphTransforms.Framed.defaultFG,
              bg: Brush = fg): Glyph = new GlyphTransforms.Framed(thisGlyph, fg = fg, bg = bg)

  /**
   * This `glyph` with a `fg`-coloured edge around it, and a background of `bg` (==`fg` if `bg` is unspecified).
   * The `fg` can have any `strokewidth`. The overall boounding diagonal is that of the glyph enlarged by
   * twice `fg.strokeWidth`. The roundness, if any, of the edge depends on its stroke width; use `framed` if you
   * need a more pronounced rounding.
   *
   * @see framed
   */
  def edged(fg: Brush = GlyphTransforms.Framed.defaultFG,
            bg: Brush = GlyphTransforms.Framed.defaultBG): Glyph = GlyphTransforms.Edged(fg = fg, bg = bg)(thisGlyph)

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

  /**  `Row.centered(thisGlyph, g)` */
  def beside(g: Glyph): Glyph = Row.centered(thisGlyph, g)
  def besideTop(g: Glyph): Glyph = Row.atTop(thisGlyph, g)
  def besideBottom(g: Glyph): Glyph = Row.atBottom(thisGlyph, g)

  /**  `Col.centered(thisGlyph, g)` */
  def above(g: Glyph): Glyph = Col.centered(thisGlyph, g)
  def aboveLeft(g: Glyph): Glyph = Col.atLeft(thisGlyph, g)
  def aboveRight(g: Glyph): Glyph = Col.atRight(thisGlyph, g)
}

object GlyphTransforms {
  /**
   * A glyph that renders as `glyph` inside` a surround painted with `fg`, and a mount painted with `bg`.
   * If `fg.strokeCap` is not `ROUND` then the surround/mount are painted as rectangles; otherwise they are
   * painted as round rectangles. A `Mounted` glyph can be extracted from its mount: usually done when
   * a collection of glyphs is to be provided with uniform dimensions.
   *
   * @see Mounted
   */
  object Framed extends DefaultPaints {
    val NOTHING: Brush = Brush("NOTHING").color(0x00000000)
    def apply(fg: Brush = defaultFG, bg: Brush = defaultBG, radiusFactor: Scalar=0f)(glyph: Glyph): Glyph =
      new Framed(glyph, fg, bg, radiusFactor)
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
    val NOTHING: Brush = Brush("NOTHING").color(0x00000000)
    def apply(fg: Brush = defaultFG, bg: Brush = defaultBG)(glyph: Glyph): Glyph = new Framed(glyph, fg, bg)
  }


  /** The glyph displaced by `(dx,dy)` in a cavity of size `(w, h)` */
  class InCavity(w: Scalar, h: Scalar, dx: Scalar, dy: Scalar, val glyph: Glyph, val fg: Brush, val bg: Brush) extends TransformedGlyph {

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

    def delta: Vec = Vec(dx, dy)
    /**
     * The diagonal size of the glyph
     */
    def diagonal: Vec = Vec(w, h)

    /** A copy of this glyph; perhaps with different foreground/background */
    def copy(fg: Brush=this.fg, bg: Brush=this.bg): Glyph = new InCavity(w, h, dx, dy, glyph, fg, bg)

  }
  /**
   * A glyph that renders as `glyph` framed by a surround painted with fg`, on a mount painted with `bg`.
   *
   * Unless `fg.cap` is `ROUND` or `radiusFactor` is `0` then the surround/mount are rectangles; otherwise they are
   * round rectangles, with lateral/vertical radius factors both specified as `radiusFactor` (if it is nonzero), or
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
    override def copy(fg: Brush = fg, bg: Brush = bg): Framed = new Framed(glyph.copy(), fg, bg)


    val delegate: Glyph = {
      if (fg.strokeCap == Brush.ROUND || radiusFactor>0f) {
        val gw = glyph.w + fg.strokeWidth * 3 // Larger than for rectangular: the curvature bites otherwise
        val gh = glyph.h + fg.strokeWidth * 3
        // caclulate sensible rounding radii
        val whratio = gw/gh
        val (xrf, yrf) =
          if (radiusFactor>0f) (radiusFactor, radiusFactor) else (.25f, .25f)

        Glyphs.Concentric()(
          RRect(gw, gh, true, xrf = xrf, yrf = yrf, fg = bg, bg = nothing),
          RRect(gw, gh, false, xrf = xrf, yrf = yrf, fg = fg, bg = nothing),
          glyph)
      } else {
        val gw = glyph.w + fg.strokeWidth * 2
        val gh = glyph.h + fg.strokeWidth * 2
        Glyphs.Concentric()(
          FilledRect(gw, gh, fg = bg, bg = nothing),
          Rect(gw, gh, fg = fg, bg = nothing),
          glyph)
      }
    }

    val diagonal = delegate.diagonal

    override def draw(surface: Surface): Unit = {
      delegate.draw(surface)
    }

    locally { delegate.parent = this }


  }

  object Edged {
    /**
     * The `glyph` with a `fg`-coloured edge around it, and a background of `bg` (==`fg` if `bg` is unspecified).
     * The `fg` can have any `strokewidth`. The overall boounding diagonal is that of the glyph enlarged by
     * twice `fg.strokeWidth`. The roundness, if any, of the edge depends on its stroke width; use `Framed` if you
     * need a more pronounced rounding.
     */
    def apply(fg: Brush=DefaultBrushes.black, bg: Brush=nothing)(glyph: Glyph): Glyph =
      new Edged(glyph, fg, bg)
  }

  class Edged(val glyph: Glyph, val fg: Brush, val bg: Brush) extends TransformedGlyph {

    import Glyphs.RRect

    val edgeWidth = fg.strokeWidth
    val inset = Vec(edgeWidth / 2f, edgeWidth / 2f)
    val offset = Vec(edgeWidth, edgeWidth)
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

          val offset = Vec(dw / 2f, dh / 2f)

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
    val quadrants = Rotated.mod4(quads)
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
    @inline def translate(p: Vec): Vec = quadrants%4 match {
      case 0 => p                       // p
      case 2 => Vec(dx-p.x, dy-p.y)     // d-p
      case 1 => Vec(dy-p.x, p.y)
      case 3 => Vec(dx-p.y, p.x)
    }

    override def reactiveContaining(p: Vec): Option[ReactiveGlyph] = glyph.reactiveContaining(translate(p))

    override def glyphContaining(p: Vec): Option[Hit] =  glyph.glyphContaining(translate(p))

    // override def transform(absolute: Vec): Vec = translate(absolute)


    def copy(fg: Brush = fg, bg: Brush = bg): Rotated =
      new Rotated(glyph.copy(), quadrants, fg, bg)
  }

  object Rotated extends DefaultPaints {

    import scala.annotation.tailrec

    @inline @tailrec def mod4(m: Int): Int = if (m<0) (mod4(4+m)) else m % 4
    /**
     * Returns `glyph` rotated by `quadrants*90` degrees.
     *
     * Unless otherwise specified, `fg` and `bg` are inherited from `glyph`.
     */
    def apply(quadrants: Int, fg: Brush = null, bg: Brush = null)(glyph: Glyph): Rotated =
      new Rotated(glyph, quadrants, fg=(if (fg eq null) glyph.fg else fg),
                                    bg=(if (bg eq null) glyph.bg else bg))
  }

  /**
   *  The given `glyph` rotated by `degrees` degrees. Unless `tight` the bounding box of the
   *  result is calculated by rotating the glyph's bounding box, and may (for near-circular glyphs)
   *  be insufficnelty tight. When `tight` is true, then the bounding box is a square whose side is the
   *  larger of the sides of the glyph's box: this is tighter for near-circular glyphs.
   */
  class Turned(val glyph: Glyph, degrees: Scalar, tight: Boolean, val fg: Brush, val bg: Brush) extends TransformedGlyph {
    import Math.PI
    val `pi`    = PI
    val `pi/2`  = `pi`/2
    val `2pi`   = `pi`*2
    val `3pi/2` = 3*`pi/2`

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

    def cos(theta: Double): Scalar = Math.cos(theta).toFloat
    def sin(theta: Double): Scalar = Math.sin(theta).toFloat


    // Centre of the glyph's bounding box
    private val center = d scaled 0.5f

    val (ww, hh) =
      if (tight) {
        val D = d.x max d.y
        (D,D)
      }
      else {
        // calculations dispatch on the quadrant.
        @inline def f(theta: Double): (Scalar, Scalar)    = ((d.x*cos(theta) + d.y*sin(theta)).abs, (d.x*sin(theta)+d.y*cos(theta)).abs)
        @inline def rotf(theta: Double): (Scalar, Scalar) = ((d.x*sin(theta)+d.y*cos(theta)).abs, (d.x*cos(theta) + d.y*sin(theta)).abs)
        if (Theta<=`pi/2`) f(Theta)
        else
        if (Theta<=`pi`)
          rotf(Theta-`pi/2`)
        else
        if (Theta<=`3pi/2`)
          f(Theta-`pi`)
        else
          rotf(Theta-`3pi/2`)
      }


    // default bounding box: overridden for `turnedBoxed`
    def box: Vec = Vec(ww, hh)

    // Bounding box of the transformed glyph
    val diagonal = box

    // Distance of the new centre from the old centre
    private val delta  = (diagonal scaled .5f) - center

    def translate(p: Vec): Vec = p - delta

    def draw(surface: Surface): Unit = {
        drawBackground(surface)
        surface.withClip(diagonal) {
          surface.withOrigin(delta) { surface.withRot(degrees, center) { glyph.draw(surface) } }
        }
    }

    locally { glyph.parent = this }

    override def reactiveContaining(p: Vec): Option[ReactiveGlyph] = glyph.reactiveContaining(translate(p))

    override def glyphContaining(p: Vec): Option[Hit] = glyph.glyphContaining(translate(p))

    def copy(fg: Brush = fg, bg: Brush = bg): Turned =
      new Turned(glyph.copy(), degrees, tight, fg, bg)
  }

  object Turned extends DefaultPaints {
    /**
     * @see Turned
     */
    def apply(degrees: Scalar, fg: Brush = null, bg: Brush = null)(glyph: Glyph): Turned =
        new Turned(glyph, degrees, degrees<=0f,
          fg = (if (fg eq null) glyph.fg else fg),
          bg = (if (bg eq null) glyph.bg else bg))
    def tight(degrees: Scalar, fg: Brush = null, bg: Brush = null)(glyph: Glyph): Turned =
      new Turned(glyph, degrees, true,
        fg = (if (fg eq null) glyph.fg else fg),
        bg = (if (bg eq null) glyph.bg else bg))
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

    val diagonal = d + (skewX*d.y, skewY*d.x)

    private val delta = Vec(-diagonal.x, 0f)

    def translate(p: Vec): Vec = p.skewed(-skewX, -skewY)

    val skew = Array(1f,   skewX, diagonal.x,
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
      val ffg = (if (fg eq null) glyph.fg else fg)
      val bbg = (if (bg eq null) glyph.bg else bg)

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

    val diagonal = glyph.diagonal

    val (xf, dx) = if (leftRight) (-1f, diagonal.x) else (1f, 0f)
    val (yf, dy) = if (topBottom) (-1f, diagonal.y) else (1f, 0f)

    def translate(p: Vec): Vec = Vec(dx-p.x, dy-p.y)

    val mirror: Array[Float] = Array(
      xf, 0f, dx,
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

    def apply(wscale: Scale, hscale: Scalar, fg: Brush, bg: Brush)(glyph: Glyph): Glyph =
      if (wscale == 1f && hscale == 1f) glyph else new Scaled(glyph, Vec(wscale, hscale), fg, bg)
  }

  object Shaded {
    def apply(glyph: Glyph, fg: Brush, bg: Brush, enlarge: Scalar = 0.25f, delta: Scalar = 8f, down: Boolean=false): Glyph =
        new Shaded(glyph.enlarged(if (enlarge <= 1f) enlarge * (glyph.w min glyph.h) else enlarge), fg = fg, bg = bg, delta = delta, down)
  }

  class Shaded(val glyph: Glyph, val fg: Brush, val bg: Brush, delta: Scalar, val down: Boolean) extends TransformedGlyph {

    override def toString: String = s"Shaded.Static($fg, $bg, delta=$delta, $down)\n  ($glyph)"

    val fgWidth = fg.getStrokeWidth
    val offset = Vec(delta, delta)
    val diagonal = glyph.diagonal + offset
    val linePaint = Brush().setColor(0x99070707).setStrokeWidth(0f) //.setStrokeCap(PaintStrokeCap.SQUARE)

    val shading = Glyphs.Shaded.shadingPaths(glyph.w, glyph.h, delta)

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
}
